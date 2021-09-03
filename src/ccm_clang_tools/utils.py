import os
import shutil
from typing import Dict

clang_version_req = "10"
clang_tool_path = os.path.dirname(os.path.abspath(__file__))

def get_docker_image_name() -> str:
    import importlib.metadata
    tag = importlib.metadata.version("ccm_clang_tools")
    image = "gjingram/ccm-clang-tools"
    return f"{image}:{tag}"

def check_clang_version(raise_: bool = True) -> bool:
    good_clang = shutil.which(f"clang-{clang_version_req}") is not None
    llvm_config_exists = (
            shutil.which(f"llvm-config-{clang_version_req}") is not None
            )
    if not good_clang and raise_:
        raise RuntimeError(
                f"ccm_clang_tools requires clang-{clang_version_req} " +
                "to build a plugin"
                )
    return good_clang

def check_clang_dev_exists(raise_: bool = True) -> bool:
    llvm_config_exists = (
            shutil.which(f"llvm-config-{clang_version_req}") is not None
            )
    if not llvm_config_exists and raise_:
        raise RuntimeError(
                "ccm_clang_tools requires clang development tools " +
                "to build a plugin"
                )
    return llvm_config_exists

def check_dylib_exists(raise_: bool = True) -> bool:
    dylib_exists = os.path.exists(
            os.path.join(
                clang_tool_path,
                "libtooling",
                "clang_tool.dylib"
                )
            )
    if not dylib_exists and raise_:
        raise RuntimeError(
                "clang_tool.dylib does not exist."
                )
    return dylib_exists

def check_source_exists(raise_: bool = True) -> bool:
    src_exists =  os.path.exists(
            os.path.join(
                clang_tool_path,
                "libtooling",
                "ASTExporter.h"
                )
            )
    if not src_exists and raise_:
        raise RuntimeError(
                "ccm_clang_tools pluging source code does not exist."
                )
    return src_exists

def check_make_exists(raise_: bool = True) -> bool:
    make_exists = shutil.which("make") is not None
    if not make_exists and raise_:
        raise RuntimeError(
                "'make' not found on the system path"
                )
    return make_exists

def check_docker_exists(raise_: bool = True) -> bool:
    docker_exists = shutil.which("docker") is not None
    if not docker_exists and raise_:
        raise RuntimeError(
                "'docker' not found on the system path"
                )
    return docker_exists

def check_image_exists(raise_: bool = True) -> "docker.models.images.Image":
    import docker

    dclient = docker.from_env()
    image_name = get_docker_image_name()
    image_exists = False
    image = None
    try:
        image = dclient.images.get(image_name)
    except docker.errors.ImageNotFound:
        if raise_:
            raise docker.errors.ImageNotFound

    return image

def find_symlinked_dir(
        root: str,
        rel_path: str,
        rel_path_to_tgt: dict) -> Dict[str, str]:
    rel_path_parts = rel_path.split(os.sep)
    check_rel_path = ""
    for part in rel_path_parts:
       check_rel_path = os.path.join(check_rel_path, part)
       check_path = os.path.join(root, check_rel_path)
       if os.path.islink(check_path):
           target = os.path.abspath(
                   os.readlink(check_path)
                   )
           if not os.path.isdir(target):
               continue
           rel_path_to_tgt[check_rel_path] = target
    return
