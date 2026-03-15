S3_PATH ?= s3://overturemaps-us-west-2/release/
PERF_OUT := /tmp/tc-perf.data
PRQLC_VER := 0.13.10
DUCKDB_VER := 1.4.4
LEAN_VER := v4.28.0

.PHONY: build test deps duckdb elan prqlc ci docker docker-dev dtest perf mem

build:
	lake build tc test
	cp Tc/Data/ADBC/funcs.prql .lake/build/bin/

ARGS ?=

test: build
	.lake/build/bin/test $(ARGS) || (cat test.log; exit 1)

# CI: install Ubuntu packages
deps:
	apt-get update && apt-get install -y --no-install-recommends curl ca-certificates git gcc make unzip

# Install official DuckDB release (distro packages lack ADBC support)
duckdb:
	curl -fsSL https://github.com/duckdb/duckdb/releases/download/v$(DUCKDB_VER)/libduckdb-linux-amd64.zip -o /tmp/duckdb.zip
	unzip -o /tmp/duckdb.zip -d /tmp/duckdb
	cp /tmp/duckdb/libduckdb.so /usr/local/lib/
	ldconfig

elan:
	curl -sSf https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh | sh -s -- -y --default-toolchain leanprover/lean4:$(LEAN_VER)

prqlc:
	curl -fsSL https://github.com/PRQL/prql/releases/download/$(PRQLC_VER)/prqlc-$(PRQLC_VER)-x86_64-unknown-linux-musl.tar.gz | tar xz -C /usr/local/bin ./prqlc

ci: deps duckdb elan prqlc build test

# Production image
docker:
	docker build -t tc .

# Dev container: build once, then use dtest for builds
docker-dev:
	docker compose build dev

# Run make test inside dev container (source bind-mounted)
dtest:
	docker compose run --rm dev make test

perf:
	.lake/build/bin/tc +n $(S3_PATH) &  PID=$$!; \
	sleep 2; \
	perf record -g -p $$PID -o $(PERF_OUT) -- sleep 10; \
	kill $$PID 2>/dev/null; wait $$PID 2>/dev/null; \
	perf report -i $(PERF_OUT) --stdio --no-children | head -60

mem:
	.lake/build/bin/tc +n $(S3_PATH) &  PID=$$!; \
	sleep 3; \
	ps -p $$PID -o pid,%cpu,%mem,rss,vsz; \
	kill $$PID 2>/dev/null; wait $$PID 2>/dev/null
