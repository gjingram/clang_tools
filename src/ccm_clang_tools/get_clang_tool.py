import os
import argparse

from .utils import (
    clang_tool_path
)

def docker_pull_clang_tool():
    stream = os.popen(
            "docker pull gjingram/clang_tools:latest"
            )
    print(stream.read())
    return

def docker_build_clang_tool():
    stream = os.popen(
            f"docker build -t gjingram/clang_tools:latest {clang_tool_path}"
            )
    print(stream.read())
    return

if __name__ == "__main__":

    aparse = argparse.ArgumentParser(
            description="A utility to pull or build the clang-tool docker container"
            )
    aparse.add_argument(
            "--build",
            action="store_true",
            default=False,
            help="Perform a local build of the clang-tool docker container"
            )

    args = aparse.parse_args()

    if args.build:
        docker_build_clang_tool()
    else:
        docker_pull_clang_tool()
