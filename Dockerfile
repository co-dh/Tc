# === Build + test ===
FROM ubuntu:24.04 AS build

ENV DEBIAN_FRONTEND=noninteractive
ENV PATH="/root/.elan/bin:${PATH}"

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl ca-certificates git gcc make unzip \
    && rm -rf /var/lib/apt/lists/*

RUN curl -fsSL https://github.com/duckdb/duckdb/releases/download/v1.2.2/libduckdb-linux-amd64.zip \
    -o /tmp/duckdb.zip && unzip /tmp/duckdb.zip -d /tmp/duckdb \
    && cp /tmp/duckdb/libduckdb.so /usr/local/lib/ \
    && ldconfig && rm -rf /tmp/duckdb /tmp/duckdb.zip

RUN curl -fsSL https://github.com/PRQL/prql/releases/download/0.13.2/prqlc-0.13.2-x86_64-unknown-linux-musl.tar.gz \
    | tar xz -C /usr/local/bin --strip-components=0 ./prqlc

RUN curl -sSf https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh \
    | bash -s -- -y --default-toolchain leanprover/lean4:v4.28.0

WORKDIR /root/Tc
COPY . .
RUN lake build tc test && mkdir -p tmp && .lake/build/bin/test

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
COPY --from=build /root/Tc/.lake/build/bin/tc .lake/build/bin/tc
RUN mkdir -p tmp

ENTRYPOINT [".lake/build/bin/tc"]
