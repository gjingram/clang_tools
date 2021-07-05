FROM teeks99/clang-ubuntu:10 AS build
WORKDIR /build

COPY src/Makefile* ./
COPY src/libtooling/*.cpp libtooling/
COPY src/libtooling/*.h libtooling/
COPY src/libtooling/Makefile* libtooling/
COPY src/libtooling/jsonlib/*.cpp libtooling/jsonlib/
COPY src/libtooling/jsonlib/*.h libtooling/jsonlib/
COPY src/libtooling/jsonlib/Makefile* libtooling/jsonlib

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

ENV PATH="/clang_tool/ccm_clang_tools:$PATH"

COPY --from=build /build/libtooling/clang_tool.dylib /clang_tool/ccm_clang_tools/libtooling/clang_tool.dylib
COPY src/ccm_clang_tools /clang_tool/ccm_clang_tools
RUN chmod +x /clang_tool/ccm_clang_tools/clang_parse.py

COPY --from=build /usr/bin/clang /usr/bin/clang
RUN chmod +x /usr/bin/clang

COPY --from=build /usr/bin/llvm-config /usr/bin/llvm-config
RUN chmod +x /usr/bin/llvm-config

CMD ["clang_parse.py", "--help"]
ENTRYPOINT ["clang_parse.py"]
