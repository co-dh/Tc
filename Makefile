S3_PATH ?= s3://overturemaps-us-west-2/release/
PERF_OUT := /tmp/tc-perf.data
PRQLC_VER := 0.13.10
LEAN_VER := v4.28.0

.PHONY: build test deps prqlc docker perf mem

build:
	lake build tc test
	cp Tc/Data/ADBC/funcs.prql .lake/build/bin/

test: build
	.lake/build/bin/test || (cat test.log; exit 1)

# CI: install Arch packages, elan, prqlc
deps:
	pacman -Syu --noconfirm base-devel git duckdb curl unzip

elan:
	curl -sSf https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh | sh -s -- -y --default-toolchain leanprover/lean4:$(LEAN_VER)

prqlc:
	curl -fsSL https://github.com/PRQL/prql/releases/download/$(PRQLC_VER)/prqlc-$(PRQLC_VER)-x86_64-unknown-linux-musl.tar.gz | tar xz -C /usr/local/bin ./prqlc

ci: deps elan prqlc build test

docker:
	docker build -t tc .

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
