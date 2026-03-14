#!/usr/bin/env python

import subprocess
import sys
import shutil
import textwrap
import argparse
import pathlib
from clang import cindex
from functools import cache

@cache
def get_top_level () -> pathlib.PurePath:
    return pathlib.PurePath(
        subprocess.check_output(
            ["git", "rev-parse", "--show-toplevel"]
        ).decode().strip()
    )

@cache
def get_llvm_prefix() -> pathlib.PurePath:
    if sys.platform == "darwin":
        return pathlib.PurePath(
            subprocess.check_output(
                ["brew", "--prefix", "llvm"]
            ).decode().strip()
        )
    llvm_config = shutil.which("llvm-config")
    if llvm_config is None:
        for version in range(30, 14, -1):
            llvm_config = shutil.which(f"llvm-config-{version}")
            if llvm_config:
                break
        if llvm_config is None:
            raise RuntimeError("Cannot find llvm-config")
        return pathlib.PurePath(
            subprocess.check_output(
                [llvm_config, "--prefix"]
            ).decode().strip()
        )

@cache
def get_clang_flags(llvm_prefix : pathlib.PurePath) -> list[str]:
    if sys.platform != "darwin":
        return []

    sysroot = pathlib.PurePath(
        subprocess.check_output(
            ["xcrun", "--show-sdk-path"]
        ).decode().strip()
    )
    resource_dir = pathlib.PurePath(
        subprocess.check_output(
            [llvm_prefix.joinpath("bin", "clang++"), "-print-resource-dir"]
        ).decode().strip()
    )
    return [
        f"--sysroot={sysroot}",
        f"-resource-dir={resource_dir}",
    ]

@cache
def get_readline_prefix() -> pathlib.PurePath:
    if sys.platform == "darwin":
        return pathlib.PurePath(
            subprocess.check_output(
                ["brew", "--prefix", "readline"]
            ).decode().strip()
        )
    return pathlib.PurePath(
        subprocess.check_output(
            ["pkg-config", "--variable=prefix", "readline"]
        ).decode().strip()
    )

@cache
def get_index(llvm_prefix : pathlib.PurePath) -> cindex.Index:
    if sys.platform == "darwin":
        library_file = llvm_prefix.joinpath("lib", "libclang.dylib")
        if library_file.as_posix() != cindex.Config.library_file:
            cindex.Config.set_library_file(library_file.as_posix())
    return cindex.Index.create()

class UltimateHelpFormatter(
    argparse.RawTextHelpFormatter, argparse.ArgumentDefaultsHelpFormatter
):
    pass

def parse_args(args: list[str] | None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        prog="build_hierarchy.py",
        formatter_class=UltimateHelpFormatter,
        description=textwrap.dedent("""\
        Build class hierarchy to be used in Hierarchy.h.  Most likely call it with:
          build_hierarchy.py --mode file Types.h
        or:
          build_hierarchy.py --mode build build

        The latter is slower, but more thorough and may pick up some compiler that
        were not hardcoded in this script.
        """)
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
        help="directory where sources are; --file and --build are relative "
        "to this",
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

def main(args: list[str] | None = None) -> None:
    parsed = parse_args(args)

    index = get_index(parsed.llvm_prefix)
    nodes : dict[str, set[str]] = {parsed.root_class : set()}

    def update_nodes(tu : cindex.TranslationUnit) -> None:
        for cursor in tu.cursor.walk_preorder():
            if cursor.kind in (
                    cindex.CursorKind.CLASS_DECL,
                    cindex.CursorKind.STRUCT_DECL,
            ):
                if cursor.spelling not in nodes:
                    for child in cursor.get_children():
                        if child.kind == cindex.CursorKind.CXX_BASE_SPECIFIER \
                           and child.spelling in nodes:
                            nodes[cursor.spelling] = set()
                            nodes[child.spelling].add(cursor.spelling)

    options = cindex.TranslationUnit.PARSE_INCOMPLETE \
        | cindex.TranslationUnit.PARSE_SKIP_FUNCTION_BODIES

    if parsed.mode == "file":
        tu = index.parse(
            parsed.sources_dir.joinpath(parsed.path),
            get_clang_flags(parsed.llvm_prefix) + [
                "-x", "c++", "-std=gnu++2b",
                f"-I{parsed.sources_dir}",
                f"-I{parsed.readline_prefix.joinpath("include")}",
            ],
            None,
            options,
        )
        for diag in tu.diagnostics:
            print(f"[{diag.severity}]: {diag.spelling}", file=sys.stderr)
        update_nodes(tu)
    else:
        db = cindex.CompilationDatabase.fromDirectory(parsed.sources_dir.joinpath(parsed.path))
        for cmd in db.getAllCompileCommands():
            tu = index.parse(
                cmd.filename,
                get_clang_flags(parsed.llvm_prefix) + list(cmd.arguments)[2:-2],
                None,
                options,
            )
            for diag in tu.diagnostics:
                print(f"[{diag.severity}]: {diag.spelling}", file=sys.stderr)
            update_nodes(tu)

    def emit(name: str, indent: int = 1) -> str:
        children = nodes[name]
        prefix = "  " * indent + parsed.prefix_class
        if not children:
            return f"{prefix}<{name}>"
        inner = ",\n".join(emit(child, indent + 1) for child in children)
        return f"{prefix}<{name},\n{inner}>"

    print(f"using ValueHierarchy = \n{emit(parsed.root_class)}")

if __name__ == "__main__":
    main()

# Test cases:
# main(["--mode", "build", "build"])
# main(["--mode", "file", "Types.h"])
