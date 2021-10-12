import os
import shutil
import argparse
import subprocess
from pathlib import Path
from typing import List

from .utils import (
    clang_tool_path,
    clang_version_req,
    find_symlinked_dir,
    get_docker_image_name
)

def command(
        files,
        include_paths,
        out_dir,
        clang_tool_verbose, 
        plugin_loc,
        plugin_name,
        recursion_level, 
        clang,
        prettify):

    build_out_dir(out_dir, files)
    for file_ in files:
        file_for_out = file_
        if file_for_out.startswith(os.sep + "host"):
            file_for_out = os.path.relpath(
                    file_for_out,
                    os.sep + "host"
                    )
        out_dirname = os.path.dirname(
                os.path.join(
                    out_dir,
                    file_for_out.lstrip(os.sep)
                )
                )
        out_filename = os.path.basename(file_).split(".")[0] + "-clang.json"
        out_file = os.path.join(out_dirname, out_filename)

        include = ""
        for inc_path in include_paths:
            include += f" -Xclang -I{inc_path}"

        inv = f"clang-{clang_version_req} -xc++ -fsyntax-only -Xpreprocessor"
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
        inv += f" -Xclang {out_file} -c {file_}"

        if clang_tool_verbose:
            print("clang-parse")
            print(f"Processing file: {file_}")
            print(f"Output at: {out_file}")
            print(f"{inv}")

        stream = os.popen(inv)
        out = stream.read()
        if out:
            print(out)

    return

def docker_command(
        files,
        include_paths,
        out_dir,
        clang_tool_verbose,
        recursion_level,
        clang,
        prettify):

    call_dir = os.getcwd()
    inv = ""
    mt = " -v /:/host"

    file_base = os.path.join("/host", call_dir)

    src_symlinks = {}
    for psymlink in files:
        find_symlinked_dir(psymlink, src_symlinks)
    inc_symlinks = {}
    for psymlink in include_paths:
        find_symlinked_dir(psymlink, inc_symlinks)

    mt_symlinks = ""
    for mount_to, target in src_symlinks.items():
        mount_to = mount_to.lstrip(os.sep)
        mt_symlinks += f" -v {target}:{os.path.join('/host', mount_to)}"
    for mount_to, target in inc_symlinks.items():
        mount_to = mount_to.lstrip(os.sep)
        mt_symlinks += f" -v {target}:{os.path.join('/host', mount_to)}"

    inv += " --files"
    for _file in files:
        file_path = os.path.join(
                os.sep,
                "host",
                _file.lstrip(os.sep)
                )
        inv += f" {file_path}"

    out_dir = os.path.join("/host", out_dir.lstrip(os.sep))

    inv += f" --out-dir {out_dir}"

    if clang_tool_verbose:
        inv += " --clang-tool-verbose"

    if recursion_level == 0 or recursion_level is None:
        inv += " --no-recursion"
    elif recursion_level == 1:
        inv += " --recurse-inherited"
    elif recursion_level == 2:
        inv += " --recurse-all"

    if prettify:
        inv += " --prettify"

    for clang_arg in clang:
        inv += f" {clang_arg}"

    docker_inv = "docker run -it"
    docker_inv += " --rm"
    docker_inv += f" {mt}"
    docker_inv += f" {mt_symlinks}"
    docker_inv += " --user $(id -u):$(id -g)"
    docker_inv += f" {get_docker_image_name()}"
    docker_inv += f" {inv}"

    stream = os.popen(docker_inv)
    out = stream.read()
    if out:
        print(out)

    return

def build_out_dir(out_dir: str, files: List[str]) -> None:
    if not out_dir.startswith(os.sep):
        out_dir = os.sep + out_dir
    if os.path.exists(out_dir) and not os.path.isdir(out_dir):
        raise RuntimeError("A file exists at the specified output path")
    elif os.path.exists(out_dir) and os.path.isdir(out_dir):
        shutil.rmtree(out_dir)
    for file_ in files:
        full_file = file_
        if full_file.startswith(os.sep + "host"):
            full_file = os.path.relpath(
                    full_file,
                    os.sep + "host"
                    )
        if not full_file.startswith(os.sep):
            full_file = os.sep + full_file
        out_filename = os.path.join(
                out_dir,
                full_file.lstrip(os.sep)
                )
        out_filepath = Path(out_filename)
        out_dirpath = out_filepath.parent
        if not os.path.exists(out_dirpath):
            out_dirpath.mkdir(parents=True)
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
            "--prettify",
            "-p",
            help="Prettify JSON output",
            action="store_true",
            default=False
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
            default=os.path.join(os.getcwd(), "clang_out")
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

    call_dir = os.getcwd()
    use_files = []
    for file_ in known.files:
        if file_.startswith(os.sep):
            use_files.append(file_)
            continue
        use_files.append(os.path.join(call_dir, file_))

    use_includes = []
    if known.includes:
        for path in known.includes:
            if path.startswith(os.sep):
                use_includes.append(path)
                continue
            use_includes.append(os.path.join(call_dir, path))

    recursion_level = 0
    if known.recurse_inherited:
        recursion_level = 1
    if known.recurse_all:
        recursion_level = 2
    if known.no_recursion:
        recursion_level = 0

    if known.docker:
        docker_command(
                use_files,
                use_includes,
                known.out_dir,
                known.clang_tool_verbose,
                recursion_level,
                unknown,
                known.prettify
                )
    else:
        command(
                use_files,
                use_includes,
                known.out_dir,
                known.clang_tool_verbose,
                known.plugin_loc,
                known.plugin_name,
                recursion_level,
                unknown,
                known.prettify
                )

    return

if __name__ == "__main__":
    run_clang_parse()
