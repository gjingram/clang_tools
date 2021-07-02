# Copyright (c) 2014-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.PHONY: clean all test all_ast_samples

LEVEL=..
include $(LEVEL)/Makefile.common
include $(LEVEL)/Makefile.rules

HEADERS+=SimplePluginASTAction.h FileUtils.h AttrParameterVectorStream.h
OBJS+=SimplePluginASTAction.o FileUtils.o AttrParameterVectorStream.o

# ASTExporter
HEADERS+=jsonlib/JSONWriter.h ASTExporter.h NamePrinter.h
OBJS+=ASTExporter.o

# Json
PLUGINS+=JsonASTExporter
EXTS+=.json

.cpp.o :
	@mkdir -p $$(dirname $@)
	$(CXX) -c $(CFLAGS) $(CXXFLAGS) -o $@ $<

all: clang_tool.dylib ast_exporter_bin

CLANG_FRONTEND=$(CLANG) -fsyntax-only -Xpreprocessor -detailed-preprocessing-record -Xclang -load -Xclang $(shell pwd)/clang_tool.dylib

clang_tool.dylib: $(OBJS) $(HEADERS)
	$(CXX) -stdlib=libstdc++ $(LDFLAGS_DYLIB) -o $@ $(OBJS) -lz -lpthread -lm -lstdc++

# Experimental tool. This requires clang static libraries.
AST_EXPORTER_OBJS=ast_exporter_bin.o SimplePluginASTAction.o FileUtils.o AttrParameterVectorStream.o
ast_exporter_bin: $(AST_EXPORTER_OBJS) $(HEADERS)
	$(CXX) -stdlib=libstdc++ -o $@ $(AST_EXPORTER_OBJS) $(LDFLAGS) $(LLVM_CXXFLAGS) $(CLANG_TOOL_LIBS) $(LLVM_LDFLAGS) -lz -lpthread -lm -lstdc++


TEST_DIRS=tests
OUT_TEST_FILES=${TEST_DIRS:%=%/*/*.out}

# To make sharing of test files easier, each source file should be
# found in 'tests'. A plugin will only use the source files for which
# a .exp file exists in the corresponding subdirectory.
EXPFILES_FORMULA=tests/$$P/*.exp
SRCFILE_FORMULA=tests/$$(basename $$TEST)
FILTERFILE_FORMULA=tests/$${P}/filter.sh

test: clang_tool.dylib
	@for P in $(PLUGINS); do                                                        \
	   echo "-- $$P --";                                                            \
	   export CLANG_FRONTEND_PLUGIN__AST_WITH_POINTERS=0;                           \
	   rm -rf build/tmp_$$P;                                                        \
	   mkdir -p build/tmp_$$P;                                                      \
	   for EXPFILE in $(EXPFILES_FORMULA); do                                       \
	     TEST=$${EXPFILE%.exp};                                                     \
	     EXTRA_FLAGS="";                                                            \
	     case "$(SRCFILE_FORMULA)" in                                               \
	     *.m )                                                                      \
	       if [ "$(HAS_OBJC)" = "no" ]; then                                        \
	         printf "[~] %s skipped (no Objective-C support)\n"                     \
	           "$$(basename $(SRCFILE_FORMULA))";                                   \
	         continue;                                                              \
	       fi;                                                                      \
	       EXTRA_FLAGS="-ObjC -fblocks $(IOSFLAGS)";                                \
	       ;;                                                                       \
	     *.cpp )                                                                    \
	       EXTRA_FLAGS="--std=c++14";                                               \
	       ;;                                                                       \
	     *.mm )                                                                     \
	       EXTRA_FLAGS="--std=c++14 -ObjC++ -fblocks $(IOSFLAGS)";                  \
	       ;;                                                                       \
	     esac;                                                                      \
	     $(RUNTEST) "$$TEST" $(FILTERFILE_FORMULA)                                  \
	       $(CLANG_FRONTEND) $$EXTRA_FLAGS -Xclang -plugin -Xclang $$P              \
	       -Xclang -plugin-arg-$$P -Xclang -                                        \
	       -Xclang -plugin-arg-$$P -Xclang USE_TEMP_DIR_FOR_DEDUPLICATION=build/tmp_$$P \
	       -c $(SRCFILE_FORMULA);                                                   \
	   done;                                                                        \
	done
	@if [ ! $$KEEP_TEST_OUTPUTS ]; then rm -f $(OUT_TEST_FILES); fi

record-test-outputs:
	@$(MAKE) DEBUG=1 KEEP_TEST_OUTPUTS=1 test || true
	@for F in $(OUT_TEST_FILES); do cp $$F $${F%.out}.exp; done
	@rm -f $(OUT_TEST_FILES)

clean:
	@rm -rf *.o *.dylib ast_exporter_bin $(OUT_TEST_FILES)

# -- AST samples for specific end-to-end tests --

REGULAR_SOURCES=$(wildcard tests/*.m) $(wildcard tests/*.c) $(wildcard tests/*.cpp) $(wildcard tests/*.mm)
AST_SAMPLE_FILES=ASTExporter.cpp $(REGULAR_SOURCES:tests/%=%)
all_ast_samples: $(AST_SAMPLE_FILES:%=build/ast_samples/%.json.gz) $(AST_SAMPLE_FILES:%=build/ast_samples/%.yjson.gz)

J_DUMPER_ARGS=-Xclang -plugin -Xclang JsonASTExporter -Xclang -plugin-arg-JsonASTExporter -Xclang

build/ast_samples/%.cpp.json: %.cpp clang_tool.dylib
	@mkdir -p build/ast_samples
	$(CLANG_FRONTEND) $(CFLAGS) -Wno-ignored-qualifiers -I. $(J_DUMPER_ARGS) $@ -c $<

build/ast_samples/%.cpp.json: tests/%.cpp clang_tool.dylib
	@mkdir -p build/ast_samples
	$(CLANG_FRONTEND) --std=c++14 $(J_DUMPER_ARGS) $@ -c $<

build/ast_samples/%.c.json: tests/%.c clang_tool.dylib
	@mkdir -p build/ast_samples
	$(CLANG_FRONTEND) $(J_DUMPER_ARGS) $@ -c $<

build/ast_samples/%.m.json: tests/%.m clang_tool.dylib
	@mkdir -p build/ast_samples
	$(CLANG_FRONTEND) $(IOSFLAGS) $(J_DUMPER_ARGS) $@ -c $<
