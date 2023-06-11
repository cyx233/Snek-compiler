UNAME := $(shell uname)

ifeq ($(UNAME), Linux)
ARCH := elf64
CFLAG := -L
endif
ifeq ($(UNAME), Darwin)
ARCH := macho64
CFLAG := --target x86_64-apple-darwin -L
endif

tests/%.s: tests/%.snek src/main.rs
	cargo run -- $< tests/$*.s

tests/%.s: tests/green/%.snek src/main.rs
	cargo run -- $< tests/green/$*.s

tests/green/%.run: tests/green/%.s runtime/start.rs
	nasm -f $(ARCH) tests/green/$*.s -o tests/green/$*.o
	ar rcs tests/green/lib$*.a tests/green/$*.o
	rustc $(CFLAG) tests/green/ -lour_code:$* runtime/start.rs -o tests/green/$*.run

tests/%.run: tests/%.s runtime/start.rs
	nasm -f $(ARCH) tests/$*.s -o tests/$*.o
	ar rcs tests/lib$*.a tests/$*.o
	rustc $(CFLAG) tests/ -lour_code:$* runtime/start.rs -o tests/$*.run

.PHONY: test
test:
	cargo build
	cargo test

clean:
	rm -f tests/*.a tests/*.s tests/*.run tests/*.o
	rm -f tests/green/*.a tests/green/*.s tests/green/*.run tests/green/*.o
