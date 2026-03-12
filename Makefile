S3_PATH ?= s3://overturemaps-us-west-2/release/
PERF_OUT := /tmp/tc-perf.data

.PHONY: build test docker docker-test perf mem

build:
	lake build tc test

test:
	lake run runtest

docker:
	docker build -t tc .

docker-test:
	docker run --rm tc

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
