FROM teeks99/clang-ubuntu:10 AS build
WORKDIR /build

COPY Makefile* ./
COPY libtooling/*.cpp libtooling/
COPY libtooling/*.h libtooling/
COPY libtooling/Makefile* libtooling/
COPY libtooling/jsonlib/*.cpp libtooling/jsonlib/
COPY libtooling/jsonlib/*.h libtooling/jsonlib/
COPY libtooling/jsonlib/Makefile* libtooling/jsonlib
COPY clang-parse ./

RUN printf '#!/bin/bash\nclang-10 "$@"' > /usr/bin/clang && chmod +x /usr/bin/clang
RUN printf '#!/bin/bash\nllvm-config-10 "$@"' > /usr/bin/llvm-config && chmod +x /usr/bin/llvm-config
RUN printf '#!/bin/bash\nclang++-10 "$@"' > /usr/bin/clang++ && chmod +x /usr/bin/clang++
RUN apt-get install -y zlib1g-dev && apt-get update
RUN make clean && make

FROM teeks99/clang-ubuntu:10 AS clangtool
WORKDIR /clang_tool
LABEL maintainer="gabriel.ingram@colorado.edu"

ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1

ENV PATH="/clang_tool:$PATH"

COPY --from=build /build/libtooling/clang_tool.dylib /clang_tool/libtooling/clang_tool.dylib
COPY --from=build /build/clang-parse /clang_tool
RUN chmod +x /clang_tool/clang-parse

COPY --from=build /usr/bin/clang /usr/bin/clang
RUN chmod +x /usr/bin/clang

COPY --from=build /usr/bin/llvm-config /usr/bin/llvm-config
RUN chmod +x /usr/bin/llvm-config