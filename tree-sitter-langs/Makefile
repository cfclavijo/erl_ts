ifeq ($(OS),Windows_NT)
$(error Windows is not supported)
endif

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

SRC_DIR := $(CURDIR)/tree-sitter-erlang/src

# System type and compiler
UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	SOEXT = dylib
	LINKSHARED = -dynamiclib -Wl,-install_name,$(LIBDIR)/libtree-sitter-erlang.$(SOEXT)
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	SOEXT = so
	LINKSHARED = -shared -Wl,-soname,libtree-sitter-erlang.so
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	SOEXT = so
	LINKSHARED = -shared -Wl,-soname,libtree-sitter-erlang.so
endif

# define default flags, and override to append mandatory flags
ARFLAGS := rcs
CFLAGS ?= -O3 -Wall -Wextra -Wshadow -Wpedantic -Werror=incompatible-pointer-types
override CFLAGS += -std=c11 -fPIC
override CFLAGS += -I$(SRC_DIR)/.

# collect sources
SRC := $(wildcard $(SRC_DIR)/*.c)
OBJ := $(SRC:.c=.o)

.PHONY: build clean

build: libtree-sitter-erlang.a libtree-sitter-erlang.$(SOEXT)

libtree-sitter-erlang.a: $(OBJ)
	$(AR) $(ARFLAGS) $@ $^

libtree-sitter-erlang.$(SOEXT): $(OBJ)
	$(CC) $(CFLAGS) $(LINKSHARED) $^ $(LDLIBS) -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	$(RM) $(OBJ) libtree-sitter-erlang.a libtree-sitter-erlang.$(SOEXT)

