import os
import sys
import subprocess
import pdb

from .utils import clang_tool_path

def build_plugin():
    wd = os.getcwd()
    os.chdir(clang_tool_path)
    args = " ".join(sys.argv[1:])
    os.system(f"make {args}")
    os.chdir(wd)
    return

if __name__ == "__main__":
    build_plugin()
