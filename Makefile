ifeq ($(OS),Windows_NT)
$(error Windows is not supported)
endif

.PHONY: all clean tree-sitter tree-sitter-langs c_src

all: tree-sitter tree-sitter-langs c_src

tree-sitter:
	$(MAKE) -C tree-sitter

tree-sitter-langs:
	$(MAKE) -C tree-sitter-langs

c_src:
	$(MAKE) -C c_src

clean:
	$(MAKE) -C c_src clean
	$(MAKE) -C tree-sitter clean
	$(MAKE) -C tree-sitter-langs clean
