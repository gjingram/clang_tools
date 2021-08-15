import os
import argparse
import subprocess

from .utils import (
    clang_tool_path
)

def command(files, file_base, out_dir, clang_tool_verbose, 
        plugin_loc, plugin_name, clang, force_out_in_src):

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

        inv = "clang -fsyntax-only -Xpreprocessor -detailed-preprocessing-record"
        for clang_arg in clang:
            inv += f" -Xclang {clang_arg}"
        inv += " -Xclang -load"
        inv += f" -Xclang {os.path.join(plugin_loc, plugin_name)}"
        inv += " -Xclang -plugin"
        inv += " -Xclang JsonASTExporter"
        inv += " -Xclang -plugin-arg-JsonASTExporter"
        inv += f" -Xclang {out_filename} -c {os.path.join(file_base, _file)}"

        if clang_tool_verbose:
            print("clang-parse")
            print(f"Processing file: {_file}")
            print(f"Output at: {os.path.join(out_dirname, out_filename)}")
            print(f"{inv}")

        stream = os.popen(inv)
        out = stream.read()
        print(out)

        return

def docker_command(files, file_base, out_dir, clang_tool_verbose, clang, force_out_in_src):

    call_dir = os.getcwd()
    inv = ""
    mt_in = ""
    mt_out = ""

    mt_in = f" -v {file_base}:/src"
    inv += " --file-base /src"

    inv += " --files"
    for _file in files:
        inv += f" {_file}"

    if out_dir:
        if not os.path.exists(os.path.join(call_dir, out_dir)):
            os.mkdir(os.path.join(call_dir, out_dir))
        mt_out = f" -v {out_dir}:/out"
    else:
        mt_out = f" -v {file_base}:/out"
    inv += " --out-dir /out"

    if clang_tool_verbose:
        inv += " --clang-tool-verbose"

    if force_out_in_src:
        inv += " --force-out-in-src"

    for clang_arg in clang:
        inv += f" {clang_arg}"

    docker_inv = "docker run -it"
    docker_inv += f" {mt_in}"
    docker_inv += f" {mt_out}"
    docker_inv += " gjingram/ccm-clang-tools:latest"
    docker_inv += f" {inv}"

    stream = os.popen(docker_inv)
    out = stream.read()
    print(out)

    return

def run_clang_parse():

    aparse = argparse.ArgumentParser(
        description="clang-tool invocation. Clang arguments fall through the argument parser")
    aparse.add_argument(
            "--abspath",
            help="Interpret file path as absolute",
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


    known, unknown = aparse.parse_known_args()
    if not known.files or len(known.files) == 0:
        raise RuntimeError("No input files provided")

    if known.docker:
        docker_command(
                known.files,
                known.file_base,
                known.out_dir,
                known.clang_tool_verbose,
                unknown,
                known.force_out_in_src
                )
    else:
        command(
                known.files,
                known.file_base,
                known.out_dir,
                known.clang_tool_verbose,
                known.plugin_loc,
                known.plugin_name,
                unknown,
                known.force_out_in_src
                )

    return

if __name__ == "__main__":
    run_clang_parse()
