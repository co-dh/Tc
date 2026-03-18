# === Build + test ===
FROM ubuntu:24.04 AS build

ENV DEBIAN_FRONTEND=noninteractive
ENV PATH="/root/.elan/bin:${PATH}"

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl ca-certificates git gcc make unzip \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /root/Tc
COPY Makefile .
RUN make duckdb elan prqlc

COPY . .
RUN make test

# === Runtime ===
FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    fzf findutils bat less gnuplot-nox \
    coreutils trash-cli curl ca-certificates \
    && rm -rf /var/lib/apt/lists/*

COPY --from=build /usr/local/lib/libduckdb.so /usr/local/lib/
RUN ldconfig
COPY --from=build /usr/local/bin/prqlc /usr/local/bin/

RUN curl -fsSL https://github.com/atanunq/viu/releases/download/v1.5.1/viu-x86_64-unknown-linux-musl \
    -o /usr/local/bin/viu && chmod +x /usr/local/bin/viu

WORKDIR /root/Tc
COPY --from=build /root/Tc/.lake/build/bin/tv .lake/build/bin/tv
RUN mkdir -p tmp

ENTRYPOINT [".lake/build/bin/tv"]
