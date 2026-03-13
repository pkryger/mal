#!/usr/bin/env python

import subprocess
import sys
import textwrap
import argparse
from clang.cindex import Index, TranslationUnit, Config, CursorKind, CompilationDatabase
from pathlib import PurePath, Path

top_level = subprocess.check_output(
    ["git", "rev-parse", "--show-toplevel"]
).decode().strip()

class UltimateHelpFormatter(
    argparse.RawTextHelpFormatter, argparse.ArgumentDefaultsHelpFormatter
):
    pass

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
    help="whether to scan a single file or whole compilation database"
)
parser.add_argument(
    "path",
    type=Path,
    help="directory to get compile_commands.json to scan for hierarchy"
)
parser.add_argument(
    "--sources-dir",
    default=PurePath(top_level, "impls", "cpp23"),
    help="directory where sources are; --file and --build are relative to this"
)
parser.add_argument(
    "--prefix-class",
    default="detail::DFSNode",
    help="class to use for hierarchy nodes; it should be a template<typename T, typename... CHILDREN>"
)
parser.add_argument(
    "--root-class",
    default="Value",
    help="the root class where the hierarchy starts"
)
args = parser.parse_args()


llvm_prefix = PurePath(
    subprocess.check_output(
        ["brew", "--prefix", "llvm"]
    ).decode().strip())
library_file = llvm_prefix.joinpath("lib", "libclang.dylib")
if library_file.as_posix() != Config.library_file:
    Config.set_library_file(library_file.as_posix())

sysroot = PurePath(
    subprocess.check_output(
        ["xcrun", "--show-sdk-path"]
    ).decode().strip())
resource_dir = PurePath(subprocess.check_output(
    [llvm_prefix.joinpath("bin", "clang++"), "-print-resource-dir"]
).decode().strip())

apple_clang_flags = [
    f"--sysroot={sysroot}",
    f"-resource-dir={resource_dir}",
]

index = Index.create()

nodes = {args.root_class : set()}

def update_nodes(tu):
    for cursor in tu.cursor.walk_preorder():
        if cursor.kind in (CursorKind.CLASS_DECL, CursorKind.STRUCT_DECL):
            if cursor.spelling not in nodes:
                for child in cursor.get_children():
                    if child.kind == CursorKind.CXX_BASE_SPECIFIER \
                       and child.spelling in nodes:
                        nodes[cursor.spelling] = set()
                        nodes[child.spelling].add(cursor.spelling)

def emit(name, indent=1):
    children = nodes[name]
    prefix = "  " * indent + args.prefix_class
    if not children:
        return f"{prefix}<{name}>"
    inner = ",\n".join(emit(child, indent + 1) for child in children)
    return f"{prefix}<{name},\n{inner}>"

options = TranslationUnit.PARSE_INCOMPLETE | TranslationUnit.PARSE_SKIP_FUNCTION_BODIES

if args.mode == "file":
    readline_prefix = PurePath(
        subprocess.check_output(
            ["brew", "--prefix", "readline"]
        ).decode().strip())
    tu = index.parse(
        args.sources_dir.joinpath(args.path),
        apple_clang_flags + [
            "-x", "c++", "-std=gnu++2b",
            f"-I{args.sources_dir}",
            f"-I{readline_prefix.joinpath("include")}",
        ],
        None,
        options,
    )
    for diag in tu.diagnostics:
        print(f"[{diag.severity}]: {diag.spelling}", file=sys.stderr)
    update_nodes(tu)
else:
    db = CompilationDatabase.fromDirectory(args.sources_dir.joinpath(args.path))
    for cmd in db.getAllCompileCommands():
        tu = index.parse(
            cmd.filename,
            apple_clang_flags +
            list(cmd.arguments)[2:-2],
            None,
            options,
        )
        for diag in tu.diagnostics:
            print(f"[{diag.severity}]: {diag.spelling}", file=sys.stderr)
        update_nodes(tu)

print(f"using ValueHierarchy = \n{emit(args.root_class)}")
