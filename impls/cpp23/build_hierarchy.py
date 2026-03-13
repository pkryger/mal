#!/usr/bin/env python

import subprocess
import sys
from clang.cindex import Index, TranslationUnit, Config, CursorKind

top_level = subprocess.check_output(
    ["git", "rev-parse", "--show-toplevel"]
).decode().strip()

sources_dir = f"{top_level}/impls/cpp23"
file = f"{sources_dir}/Types.h"
prefix_class = "detail::DFSNode"
root_class = "Value"

readline_prefix = subprocess.check_output(
    ["brew", "--prefix", "readline"]
).decode().strip()
includes = [
    f"-I{sources_dir}",
    f"-I{readline_prefix}/include",
]

llvm_prefix = subprocess.check_output(
    ["brew", "--prefix", "llvm"]
).decode().strip()
library_file = f"{llvm_prefix}/lib/libclang.dylib"
if library_file != Config.library_file:
    Config.set_library_file(library_file)

index = Index.create()

sysroot = subprocess.check_output(
    ["xcrun", "--show-sdk-path"]
).decode().strip()
resource_dir = subprocess.check_output(
    [f"{llvm_prefix}/bin/clang++", "-print-resource-dir"]
).decode().strip()

nodes = {root_class : set()}

def update_nodes(nodes, tu):
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
    prefix = "  " * indent + prefix_class
    if not children:
        return f"{prefix}<{name}>"
    inner = ",\n".join(emit(child, indent + 1) for child in children)
    return f"{prefix}<{name},\n{inner}>"


tu = index.parse(
    file,
    [
        "-x", "c++", "-std=gnu++2b",
        f"--sysroot={sysroot}",
        f"-resource-dir={resource_dir}",
    ] + includes,
    None,
    TranslationUnit.PARSE_INCOMPLETE
    | TranslationUnit.PARSE_SKIP_FUNCTION_BODIES,
)
for diag in tu.diagnostics:
    print(f"[{diag.severity}]: {diag.spelling}", file=sys.stderr)
update_nodes(tu)

# # Alternative implementation: use compile-commands.json
# from clang.cindex import CompilationDatabase
# db = CompilationDatabase.fromDirectory(f"{sources_dir}/build")
# for cmd in db.getAllCompileCommands():
#     tu = index.parse(
#         cmd.filename,
#         [
#             "-x", "c++", "-std=gnu++2b",
#             f"--sysroot={sysroot}",
#             f"-resource-dir={resource_dir}",
#         ] + includes,
#         None,
#         TranslationUnit.PARSE_INCOMPLETE
#         | TranslationUnit.PARSE_SKIP_FUNCTION_BODIES,
#     )
#     for diag in tu.diagnostics:
#         print(f"[{diag.severity}]: {diag.spelling}", file=sys.stderr)
#     update_nodes(tu)

print(f"using ValueHierarchy = \n{emit(root_class)}")
