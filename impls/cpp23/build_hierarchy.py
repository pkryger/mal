#!/usr/bin/env python

"""Build class hierarchy to be used in Hierarchy.h.

Most likely call it with:
  build_hierarchy.py --mode file Types.h
or:
  build_hierarchy.py --mode build build

The latter is slower, but more thorough when it comes to determining a class hierarchy.
It also may pick up some compiler flags that were not hard coded in this script.
"""

import argparse
import logging
import pathlib
import shutil
import subprocess
import sys
from functools import cache

from clang import cindex


@cache
def get_top_level() -> pathlib.PurePath:
    """Return top level directory of current git repository."""
    return pathlib.PurePath(
        subprocess.check_output(
            [  # noqa: S607
                "git",
                "rev-parse",
                "--show-toplevel",
            ],
        )
        .decode()
        .strip(),
    )


@cache
def get_llvm_prefix() -> pathlib.PurePath:
    """Return LLVM prefix."""
    if sys.platform == "darwin":
        return pathlib.PurePath(
            subprocess.check_output(
                [  # noqa: S607
                    "brew",
                    "--prefix",
                    "llvm",
                ],
            )
            .decode()
            .strip(),
        )
    llvm_config = shutil.which("llvm-config")
    if llvm_config is None:
        for version in range(30, 14, -1):
            llvm_config = shutil.which(f"llvm-config-{version}")
            if llvm_config:
                break
        if llvm_config is None:
            msg = "Cannot find llvm-config"
            raise RuntimeError(msg)
        return pathlib.PurePath(
            subprocess.check_output(  # noqa: S603
                [llvm_config, "--prefix"],
            )
            .decode()
            .strip(),
        )
    return None


@cache
def get_clang_flags(llvm_prefix: pathlib.PurePath) -> list[str]:
    """Return extra flags for clang."""
    if sys.platform != "darwin":
        return []

    sysroot = pathlib.PurePath(
        subprocess.check_output(
            [  # noqa: S607
                "xcrun",
                "--show-sdk-path",
            ],
        )
        .decode()
        .strip(),
    )
    resource_dir = pathlib.PurePath(
        subprocess.check_output(  # noqa: S603
            [llvm_prefix.joinpath("bin", "clang++"), "-print-resource-dir"],
        )
        .decode()
        .strip(),
    )
    return [
        f"--sysroot={sysroot}",
        f"-resource-dir={resource_dir}",
    ]


@cache
def get_readline_prefix() -> pathlib.PurePath:
    """Return readline library prefix."""
    if sys.platform == "darwin":
        return pathlib.PurePath(
            subprocess.check_output(
                [  # noqa: S607
                    "brew",
                    "--prefix",
                    "readline",
                ],
            )
            .decode()
            .strip(),
        )
    return pathlib.PurePath(
        subprocess.check_output(
            [  # noqa: S607
                "pkg-config--variable=prefix",
                "readline",
            ],
        )
        .decode()
        .strip(),
    )


@cache
def get_index(llvm_prefix: pathlib.PurePath) -> cindex.Index:
    """Return a clang index configured for current LLVM prefix."""
    if sys.platform == "darwin":
        library_file = llvm_prefix.joinpath("lib", "libclang.dylib")
        if library_file.as_posix() != cindex.Config.library_file:
            cindex.Config.set_library_file(library_file.as_posix())
    return cindex.Index.create()


class UltimateHelpFormatter(
    argparse.RawTextHelpFormatter,
    argparse.ArgumentDefaultsHelpFormatter,
):
    """Combine RawTextHelpFormatter and ArgumentDefaultHelpFormatter."""


def _parse_args(args: list[str] | None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        prog="build_hierarchy.py",
        formatter_class=UltimateHelpFormatter,
        description=sys.modules[__name__].__doc__,
    )
    parser.add_argument(
        "--mode",
        choices=["file", "build"],
        required=True,
        help="whether to scan a single file or whole compilation database",
    )
    parser.add_argument(
        "path",
        type=pathlib.Path,
        help="directory to get compile_commands.json to scan for hierarchy",
    )
    parser.add_argument(
        "--sources-dir",
        default=get_top_level().joinpath("impls", "cpp23"),
        help="directory where sources are; --file and --build are relative to this",
    )
    parser.add_argument(
        "--prefix-class",
        default="detail::DFSNode",
        help="class to use for hierarchy nodes; "
        "it should be a template<typename T, typename... CHILDREN>",
    )
    parser.add_argument(
        "--root-class",
        default="Value",
        help="the root class where the hierarchy starts",
    )
    parser.add_argument(
        "--root-key",
        choices=["class", "struct"],
        default="class",
        help="the root class key",
    )
    parser.add_argument(
        "--llvm-prefix",
        default=get_llvm_prefix(),
        type=pathlib.Path,
        help="prefix to llvm to use to parse code",
    )
    parser.add_argument(
        "--readline-prefix",
        default=get_readline_prefix(),
        type=pathlib.Path,
        help="prefix to readline library",
    )
    return parser.parse_args(args)


logger = logging.getLogger(__name__)


class HierarchyParser:
    """Parse C++ code and emit class hierarchy."""

    def __init__(
        self,
        index: cindex.Index,
        llvm_prefix: str | None,
        root_key: str,
        root_class: str,
        prefix_class: str,
    ) -> None:
        """Initialise a parser.

        Use the LLVM prefix and index to parse starting from root class and emit
        hierarchy with a prefix class.
        """
        self._index = index
        self._llvm_prefix = llvm_prefix
        self._prefix_class = prefix_class
        self._nodes: dict[str, set[str]] = {root_class: set()}
        self._order: dict[str, int] = {root_class: 0}
        self.declarations: list[str] = [f"{root_key} {root_class};"]

    def _update_nodes(self, tu: cindex.TranslationUnit) -> None:
        for cursor in tu.cursor.walk_preorder():
            if (
                cursor.kind
                in (
                    cindex.CursorKind.CLASS_DECL,
                    cindex.CursorKind.STRUCT_DECL,
                )
                and cursor.spelling not in self._nodes
            ):
                for child in cursor.get_children():
                    if (
                        child.kind == cindex.CursorKind.CXX_BASE_SPECIFIER
                        and child.spelling in self._nodes
                    ):
                        self._nodes[cursor.spelling] = set()
                        self._nodes[child.spelling].add(cursor.spelling)
                        self._order[cursor.spelling] = max(self._order.values()) + 1
                declaration = f"{
                    'class' if cursor.kind == cindex.CursorKind.CLASS_DECL else 'struct'
                } {cursor.spelling};"
                if (
                    declaration not in self.declarations
                    and cursor.spelling in self._nodes
                ):
                    self.declarations += [declaration]

    def parse(self, path: pathlib.PurePath, extra_args: list[str]) -> None:
        """Parse file at path with extra clang args."""
        tu = self._index.parse(
            path,
            get_clang_flags(self._llvm_prefix) + extra_args,
            None,
            cindex.TranslationUnit.PARSE_INCOMPLETE
            | cindex.TranslationUnit.PARSE_SKIP_FUNCTION_BODIES,
        )
        for diag in tu.diagnostics:
            logger.warning("[SEVERITY: %s]: %s", diag.severity, diag.spelling)
        self._update_nodes(tu)

    def emit(self, name: str, indent: int = 1) -> str:
        """Emit parsed hierarchy."""
        prefix = "  " * indent + self._prefix_class
        children = sorted(self._nodes[name], key=lambda child: self._order[child])
        if not children:
            return f"{prefix}<{name}>"
        inner = ",\n".join(self.emit(child, indent + 1) for child in children)
        return f"{prefix}<{name},\n{inner}>"


def main(raw_args: list[str] | None = None) -> None:
    """Parse C++ code with command line arguments and print class hierarchy."""
    args = _parse_args(raw_args)
    logging.basicConfig(level=logging.INFO)
    index = get_index(args.llvm_prefix)
    hierarchy_parser = HierarchyParser(
        index,
        args.llvm_prefix,
        args.root_key,
        args.root_class,
        args.prefix_class,
    )

    if args.mode == "file":
        hierarchy_parser.parse(
            args.sources_dir.joinpath(args.path),
            [
                "-x",
                "c++",
                "-std=gnu++2b",
                f"-I{args.sources_dir}",
                f"-I{args.readline_prefix.joinpath('include')}",
            ],
        )
    else:
        db = cindex.CompilationDatabase.fromDirectory(
            args.sources_dir.joinpath(args.path),
        )
        for cmd in db.getAllCompileCommands():
            hierarchy_parser.parse(
                cmd.filename,
                list(cmd.arguments)[2:-2],
            )

    print("\n".join(hierarchy_parser.declarations))  # noqa: T201
    print(  # noqa: T201
        f"\nusing {args.root_class}Hierarchy = "
        f"\n{hierarchy_parser.emit(args.root_class)}",
    )


if __name__ == "__main__":
    main()

# Test cases:
# main(["--mode", "build", "build"]) # noqa: ERA001
# main(["--mode", "file", "Types.h"]) # noqa: ERA001
