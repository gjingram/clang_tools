import os
import sys
import subprocess

from .utils import (
    clang_tool_path,
    check_clang_version,
    check_clang_dev_exists,
    check_source_exists,
    check_make_exists
)

def _can_build_plugin() -> None:
    check_clang_version()
    check_clang_dev_exists()
    if not check_source_exists():
        raise RuntimeError(
                "Plugin cannot be built because source code does not exist"
                )
    if not check_make_exists():
        raise RuntimeError(
                "Plugin cannot be built because the build tool (make) " +
                "cannot be found."
                )
    return

def build_plugin() -> None:
    _can_build_plugin()

    print("Building the ccm_clang_tools plugin...")
    wd = os.getcwd()
    os.chdir(clang_tool_path)
    args = " ".join(sys.argv[1:])
    os.system(f"make {args}")
    os.chdir(wd)

    return

if __name__ == "__main__":
    build_plugin()
