FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive
ENV PATH="/root/.elan/bin:${PATH}"

# System dependencies + external tools
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl ca-certificates git gcc make unzip \
    fzf findutils \
    bat less gnuplot-nox \
    python3-pip python3 jq \
    coreutils trash-cli \
    && rm -rf /var/lib/apt/lists/*

# DuckDB shared library (required - loaded via dlopen at runtime)
RUN curl -fsSL https://github.com/duckdb/duckdb/releases/download/v1.2.2/libduckdb-linux-amd64.zip \
    -o /tmp/duckdb.zip && unzip /tmp/duckdb.zip -d /tmp/duckdb \
    && cp /tmp/duckdb/libduckdb.so /usr/local/lib/ \
    && ldconfig && rm -rf /tmp/duckdb /tmp/duckdb.zip

# awscli (optional - S3 browsing)
RUN pip3 install --break-system-packages awscli

# prqlc (required - SQL compiler)
RUN curl -fsSL https://github.com/PRQL/prql/releases/download/0.13.2/prqlc-0.13.2-x86_64-unknown-linux-musl.tar.gz \
    | tar xz -C /usr/local/bin --strip-components=0 ./prqlc

# viu (optional - terminal image viewer for plots)
RUN curl -fsSL https://github.com/atanunq/viu/releases/download/v1.5.1/viu-x86_64-unknown-linux-musl \
    -o /usr/local/bin/viu && chmod +x /usr/local/bin/viu

# Lean 4 toolchain
RUN curl -sSf https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh \
    | bash -s -- -y --default-toolchain leanprover/lean4:v4.28.0

WORKDIR /root/Tc
COPY . .

# Build tc and test executables
RUN lake build tc test

# Run tests by default (osquery/HF tests auto-skip when tools missing)
RUN mkdir -p tmp
CMD [".lake/build/bin/test"]
