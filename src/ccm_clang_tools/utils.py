import os
import subprocess

clang_version_req = "10.0.0"
clang_tool_path = os.path.dirname(os.path.abspath(__file__))

def check_clang_version(llvm_conf: str = "llvm-config"):
    success = subprocess.run([llvm_conf, "--version"],
            stdout=subprocess.PIPE,
            text=True)
    if success.stdout != f"{clang_version_req}\n":
        raise RuntimeError("Clang tool requires clang-10")
    return
