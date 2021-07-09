ccm-clang-tools
======================

[ccm-clang-tools](https://github.com/gjingram/clang_tools) is a project in development as a clang libtooling plugin frontend to the python project [ccmodel](https://github.com/gjingram/ccmodel). The goal of ccm-clang-tools is to provide ccmodel with substantial information about the content of C/C++ declarations so that they can be used downstream in code generation tools, documentation tools, etc.

ccm-clang-tools is a derived work of [facebook-clang-plugins](https://github.com/facebook/facebook-clang-plugins), originally developed as a the clang front end to the Facebook [Infer](https://github.com/facebook/infer) tool. At the time of the ccm-clang-tools for, the original repository had been stale for a few years, and substantial modifications have been made to the underlying plugin to remove unnecessary JSON output and add dumps of nodes that are particularly interesting for use in the kinds of tools ccmodel is intended to drive.

This project is organized as a python package using [PDM](https://pypi.org/project/pdm/) as the package manager; however, it is generally light on python and only uses it to provide a convenient CLI to the clang plugin.

clang libtooling does not provide a stable API, which can be problematic. One modification to the facebook-clang-plugins tools is that the plugin has been updated to build and operate with clang 10. This version was chosen because it is provided as the standard for Ubuntu 20.04-- the latest LTS release. Moreover, the original tool was packaged with clang-9, which was required to be built from source. This strategy has been abandoned in favor of a [docker](https://www.docker.com/) container, with an image provided at [gjingram/ccm-clang-tools](https://hub.docker.com/repository/docker/gjingram/ccm-clang-tools). Alternatively, scripts are provided in this project that can be used to build the tool locally (requires clang 10 & make) if you have an allergy to docker, or to fetch dependencies and build the container locally.

Structure of the repository
---------------------------

- [`src/ccm_clang_tools`](https://github.com/gjingram/clang_tools/tree/master/src/ccm_clang_tools) : the root directory of the python project
- [`src/ccm_clang_tools/libtooling`](https://github.com/gjingram/clang_tools/tree/master/src/ccm_clang_tools/libtooling) : The clang plugin source code, with `ASTExporter.h` being, by far, the most interesting file

Quick start
-----------

Running `pdm install` in the project root directory will install all package dependencies and ccm\_clang\_tools into `__pypackages__` located in the project root. This will make all the python tools available to any compatible python interpreter. Make sure that PEP 582 is [enabled globally](https://pdm.fming.dev/index.html#enable-pep-582-globally).

To obtain a workable plugin, there are three options.

Building locally requires clang 10 (in particular, `llvm-config` needs to be an alias for `llvm-config-10`, although I hope to change this). The command to build the plugin locally is:

    pdm run python -m ccm_clang_tools.build_plugin

To fetch the docker image, run:

    pdm run python -m ccm_clang_tools.get_clang_tool

And to build a docker image locally, run:

    pdm run python -m ccm_clang_tools.get_clang_tool --build

The tool itself is about as straightforward to use once one of the above has been run:

    pdm run clang-parse --help

for the command line options. Note that the `--docker` argument has to be supplied on the command line to forward arguments to a container.

Licence
-------

The plugins are MIT-licensed.
