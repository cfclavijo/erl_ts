# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)])." -s init stop)
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, include)])." -s init stop)
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)])." -s init stop)

C_SRC_DIR = $(CURDIR)
C_SRC_OUTPUT ?= $(CURDIR)/../priv/$(PROJECT).so

# For mere debugging, just pass the flag -g, the other flags are to catch segfaults.
# Erlang/OTP must be compiled with the same targets as well as tree-sitter
# DEBUG_FLAGS += -g -fsanitize=address -fno-common -fno-omit-frame-pointer

# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -Wall
	LDFLAGS ?= -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -Wall
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -Wall
endif

TS_INCLUDE_DIR ?= $(BASEDIR)/tree-sitter/lib/include/
TS_ERLANG_INCLUDE_DIR ?= $(BASEDIR)/tree-sitter-langs/tree-sitter-erlang/bindings/c/
CFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -I $(TS_INCLUDE_DIR) -I $(TS_ERLANG_INCLUDE_DIR)
CXXFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -I $(TS_INCLUDE_DIR) -I $(TS_ERLANG_INCLUDE_DIR)

# $ ld -shared -o libshared.so -L. --exclude-libs ALL --start-group -lstatic shared.o --end-group
LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -L $(BASEDIR)/tree-sitter/ -L $(BASEDIR)/tree-sitter-langs/
LDLIBS_TS_STATIC += -l:libtree-sitter.a  -l:libtree-sitter-erlang.a
LDLIBS_TS_DYNAMIC += -ltree-sitter  -ltree-sitter-erlang
LDFLAGS += -shared

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

SOURCES := $(shell find $(C_SRC_DIR) -type f \( -name "*.c" -o -name "*.C" -o -name "*.cc" -o -name "*.cpp" \))
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -cxsn

# Defaulting to static linking for the tree-sitter* libraries
command_verbose = @echo "Static Library linking for tree-sitter dependencies"
ifeq ($(ERL_TS_LINKING), dynamic)
	command_verbose = @echo "Dynamic Library linking for tree-sitter dependencies"
	COMMAND = $(link_verbose) $(CC) $(OBJECTS) $(LDFLAGS) $(LDLIBS) $(LDLIBS_TS_DYNAMIC) -o $(C_SRC_OUTPUT)
else
	COMMAND = $(link_verbose) ld $(LDFLAGS) $(LDLIBS) --exclude-libs ALL --start-group $(OBJECTS) $(LDLIBS_TS_STATIC) --end-group -o $(C_SRC_OUTPUT)

endif

$(C_SRC_OUTPUT): $(OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
	$(command_verbose)
	$(COMMAND)

%.o: %.c
	$(COMPILE_C) $(OUTPUT_OPTION) $<

%.o: %.cc
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.C
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.cpp
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

clean:
	@rm -f $(C_SRC_OUTPUT) $(OBJECTS)
