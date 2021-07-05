import os

def docker_get_clang_tool():
    stream = os.popen(
            "docker pull gjingram/clang_tools:latest"
            )
    print(stream.read())
    return

if __name__ == "__main__":
    docker_get_clang_tool()
