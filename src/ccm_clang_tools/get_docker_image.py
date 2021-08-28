import os
import argparse
import docker
from typing import (
    Tuple,
    List,
    Union
)
from .utils import (
    clang_tool_path,
    check_docker_exists,
    check_image_exists,
    check_source_exists,
    check_dylib_exists,
    docker_image,
    docker_tag
)

def docker_pull_clang_tool(
        inform: bool = True
        ) -> docker.models.images.Image:

    image = check_image_exists(raise_=False)
    if image is not None:
        if inform:
            print("Docker image {docker_image}:{docker_tag} already exists")
        return image

    dclient = docker.from_env()
    image = dclient.images.pull(
            docker_image,
            tag=docker_tag
            )

    return image

def docker_build_clang_tool(
        inform: bool = True
        ) -> docker.models.images.Image:

    image = check_image_exists(raise_=False)
    if image is not None:
        if inform:
            print("Docker image {docker_image}:{docker_tag} already exists")
        return image
    check_source_exists(raise_=True)
    dclient = docker.from_env()
    image = dclient.images.build(
            path=clang_tool_path,
            tag=f"{docker_image}:{docker_tag}",
            pull=True
            )
    dclient.images.prune()
    return image

if __name__ == "__main__":

    aparse = argparse.ArgumentParser(
            description="A utility to pull or build the clang-tool docker container"
            )
    aparse.add_argument(
            "method",
            choices=("build", "pull"),
            help=(
                "Acquire docker image by building locally or pulling a repository"
                ),
            )

    args = aparse.parse_args()

    if args.method == "build":
        docker_build_clang_tool()
    else:
        docker_pull_clang_tool()
