import os
import argparse
import subprocess

from .utils import (
    clang_tool_path,
    clang_version_req,
    find_symlinked_dir,
    get_docker_image_name
)

def command(
        files,
        include_paths,
        file_base,
        out_dir,
        clang_tool_verbose, 
        plugin_loc,
        plugin_name,
        recursion_level, 
        clang,
        force_out_in_src,
        prettify):

    for _file in files:
  
        file_dirname = os.path.dirname(os.path.join(file_base, _file))
        out_dirname = out_dir if out_dir else file_dirname

        out_filename = os.path.basename(_file).split(".")[0] + "-clang.json"

        if not os.path.exists(out_dirname):
            if clang_tool_verbose:
                print(f"Creating output directory: {out_dirname}")
            os.mkdir(out_dirname)
  
        if force_out_in_src:
            out_filename = os.path.join(file_dirname, out_filename)
        else:
            out_filename = os.path.join(out_dirname, out_filename)

        include = ""
        for inc_path in include_paths:
            include += f" -Xclang -I{inc_path}"

        inv = f"clang-{clang_version_req} -fsyntax-only -Xpreprocessor"
        inv += " -detailed-preprocessing-record"
        inv += include
        for clang_arg in clang:
            inv += f" -Xclang {clang_arg}"
        inv += " -Xclang -load"
        inv += f" -Xclang {os.path.join(plugin_loc, plugin_name)}"
        inv += " -Xclang -plugin"
        inv += " -Xclang JsonASTExporter"
        inv += " -Xclang -plugin-arg-JsonASTExporter"
        inv += f" -Xclang RECURSION_LEVEL={recursion_level}"
        inv += " -Xclang -plugin-arg-JsonASTExporter"
        inv += f" -Xclang PRETTIFY_JSON={int(prettify)}"
        inv += " -Xclang -plugin-arg-JsonASTExporter"
        inv += f" -Xclang {out_filename} -c {os.path.join(file_base, _file)}"

        if clang_tool_verbose:
            print("clang-parse")
            print(f"Processing file: {_file}")
            print(f"Output at: {os.path.join(out_dirname, out_filename)}")
            print(f"{inv}")

        stream = os.popen(inv)
        out = stream.read()
        if out:
            print(out)

    return

def docker_command(
        files,
        include_paths,
        file_base,
        out_dir,
        clang_tool_verbose,
        recursion_level,
        clang,
        force_out_in_src,
        prettify):

    call_dir = os.getcwd()
    inv = ""
    mt_in = ""
    mt_out = ""

    mt_in = f" -v {file_base}:/src"
    inv += " --file-base /src"

    src_symlinks = {}
    for psymlink in files:
        find_symlinked_dir(file_base, psymlink, src_symlinks)
    inc_symlinks = {}
    for psymlink in include_paths:
        find_symlinked_dir("", psymlink, inc_symlinks)

    mt_symlinks = ""
    for mount_to, target in src_symlinks.items():
        mt_symlinks += f" -v {target}:{os.path.join('/src', mount_to)}"
    for mount_to, target in inc_symlinks.items():
        mt_symlinks += f" -v {target}:{mount_to}"

    mt_includes = ""
    for inc_path in include_paths:
        mt_includes += f" -v {inc_path}:{inc_path}"


    inv += " --files"
    for _file in files:
        inv += f" {_file}"

    if out_dir:
        if not os.path.exists(os.path.join(call_dir, out_dir)):
            os.mkdir(os.path.join(call_dir, out_dir))
        mt_out = f" -v {out_dir}:/out"
    else:
        mt_out = f" -v {file_base}:/out"
        force_out_in_src = True

    inv += " --out-dir /out"

    if clang_tool_verbose:
        inv += " --clang-tool-verbose"

    if recursion_level == 0 or recursion_level is None:
        inv += " --no-recursion"
    elif recursion_level == 1:
        inv += " --recurse-inheritance"
    elif recursion_level == 2:
        inv += " --recurse-all"

    if force_out_in_src:
        inv += " --force-out-in-src"

    if prettify:
        inv += " --prettify"

    for clang_arg in clang:
        inv += f" {clang_arg}"

    docker_inv = "docker run -it"
    docker_inv += " --rm"
    docker_inv += f" {mt_in}"
    docker_inv += f" {mt_out}"
    docker_inv += f" {mt_includes}"
    docker_inv += f" {mt_symlinks}"
    docker_inv += f" {get_docker_image_name()}"
    docker_inv += f" {inv}"

    stream = os.popen(docker_inv)
    out = stream.read()
    if out:
        print(out)

    return

def run_clang_parse():

    aparse = argparse.ArgumentParser(
            prog="clang-parse",
            description=(
                "clang-parse invocation. Clang arguments fall through" +
                "the argument parser"
                )
            )
    aparse.add_argument(
            "--abspath",
            help="Interpret file path as absolute",
            action="store_true",
            default=False
            )
    aparse.add_argument(
            "--prettify",
            "-p",
            help="Prettify JSON output",
            action="store_true",
            default=False
            )
    aparse.add_argument(
            "--file-base",
            help="Interpret file name as being relative to this",
            default=os.getcwd()
            )
    aparse.add_argument(
            "--files",
            help="Headers to be parsed",
            nargs="+",
            default=None
           )
    aparse.add_argument(
            "-I",
            "--includes",
            nargs="?"
            )
    aparse.add_argument(
            "--out-dir",
            help="Parse JSON out directory",
            default=None
           )
    aparse.add_argument(
            "--plugin-loc",
            help="Path to clang plugin",
            default=os.path.join(clang_tool_path, "libtooling"))
    aparse.add_argument(
            "--plugin-name",
            help="Name of plugin dylib",
            default="clang_tool.dylib"
            )
    aparse.add_argument(
            "--clang-tool-verbose",
            help="clang-tool verbose output",
            action="store_true",
            default=False
            )
    aparse.add_argument(
            "--docker",
            "-dc",
            help="Forward call to a docker container",
            action="store_true",
            default=False
            )
    aparse.add_argument(
            "--force-out-in-src",
            help="Force output to be placed in the file source directories",
            action="store_true",
            default=False
            )
    aparse.add_argument(
            "--no-recursion",
            help="Don't recurse into any referenced declarations",
            action="store_true"
            )
    aparse.add_argument(
            "--recurse-inherited",
            help="Recurse into declarations referenced via class inheritance",
            action="store_true"
            )
    aparse.add_argument(
            "--recurse-all",
            help="Recurse into all declarations & types referenced",
            action="store_true"
            )


    known, unknown = aparse.parse_known_args()
    if not known.files or len(known.files) == 0:
        raise RuntimeError("No input files provided")

    recursion_level = 0
    if known.recurse_inherited:
        recursion_level = 1
    if known.recurse_all:
        recursion_level = 2
    if known.no_recursion:
        recursion_level = 0

    includes = []
    if known.includes:
        includes = known.includes

    if known.docker:
        docker_command(
                known.files,
                includes,
                known.file_base,
                known.out_dir,
                known.clang_tool_verbose,
                recursion_level,
                unknown,
                known.force_out_in_src,
                known.prettify
                )
    else:
        command(
                known.files,
                includes,
                known.file_base,
                known.out_dir,
                known.clang_tool_verbose,
                known.plugin_loc,
                known.plugin_name,
                recursion_level,
                unknown,
                known.force_out_in_src,
                known.prettify
                )

    return

if __name__ == "__main__":
    run_clang_parse()
