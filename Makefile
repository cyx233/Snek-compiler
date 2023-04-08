.PRECIOUS: test/%.s
SNEK_FILES := $(wildcard test/*.snek)
RUN_TARGETS := $(patsubst test/%.snek,test/%.run,$(SNEK_FILES))

.PHONY: all
all: $(RUN_TARGETS)

test/%.s: test/%.snek src/main.rs
	cargo run -- $< test/$*.s

test/%.run: test/%.s runtime/start.rs
	nasm -f elf64 test/$*.s -o runtime/our_code.o
	ar rcs runtime/libour_code.a runtime/our_code.o
	rustc -L runtime/ runtime/start.rs -o test/$*.run

.PHONY: clean
clean:
	rm -rf test/*.run test/*.s runtime/*.a runtime/*.o