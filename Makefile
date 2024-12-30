ifeq ($(OS),Windows_NT)
$(error Windows is not supported)
endif

.PHONY: all clean tree-sitter c_src

all: tree-sitter c_src

tree-sitter:
	$(MAKE) -C tree-sitter

c_src:
	$(MAKE) -C c_src

clean:
	$(MAKE) -C c_src clean
	$(MAKE) -C tree-sitter clean
