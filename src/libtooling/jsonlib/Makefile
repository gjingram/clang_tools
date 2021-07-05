# Copyright (c) 2014-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.PHONY: clean all test

LEVEL=../..
include $(LEVEL)/Makefile.common

all: build/jsontest

build/jsontest: tests/jsontest.cpp JSONWriter.h
	@mkdir -p build
	$(CXX) $(CFLAGS) $< -o $@

test: build/jsontest
	@$(RUNTEST) tests/jsontest build/jsontest
	@! hash bdump 2>/dev/null
	@if [ ! $$KEEP_TEST_OUTPUTS ]; then rm -f tests/*.out; fi

record-test-outputs:
	@rm -f tests/*.out
	@$(MAKE) DEBUG=1 KEEP_TEST_OUTPUTS=1 test || true
	@for F in tests/*.out; do cp $$F $${F%.out}.exp; done
	@rm -f tests/*.out

clean:
	@rm -rf build/* tests/*.o tests/*.out
