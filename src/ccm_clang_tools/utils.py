import os
import subprocess

clang_version_req = "10.0.0"
py_tool_path = os.path.dirname(os.path.abspath(__file__))
clang_tool_path = os.sep + os.path.join(*py_tool_path.split(os.sep)[:-1])

def check_clang_version():
    success = subprocess.run(["llvm-config", "--version"],
            stdout=subprocess.PIPE,
            text=True)
    if success.stdout != f"{clang_version_req}\n":
        raise RuntimeError("Clang tool requires clang-10")
    return
