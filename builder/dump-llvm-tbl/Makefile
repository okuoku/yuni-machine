include tools.example.mk
-include tools.mk

INCLUDE = $(LLVM_ROOT)/include
TARGET= $(LLVM_ROOT)/lib/Target
VPATH = $(TARGET)
TBLGEN= $(TBLGEN_BIN) 

.PHONY: clean srcdir destdir
.SUFFIXES: .td .txt


include srcs.mk

DEST = $(addsuffix .txt, $(basename $(SRC)))

.td.txt:
	$(TBLGEN) -I$(INCLUDE) -I$(dir $<) $< > $@.temp
	mv $@.temp $@

all: destdir $(DEST)

destdir: 
	mkdir -p ARM
	mkdir -p CellSPU
	mkdir -p Mips
	mkdir -p PowerPC
	mkdir -p X86

clean:
	rm -f */*.txt
