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

tests/%.run: tests/%.s runtime/start.rs
	nasm -f $(ARCH) tests/$*.s -o tests/$*.o
	ar rcs tests/lib$*.a tests/$*.o
	rustc $(CFLAG) tests/ -lour_code:$* runtime/start.rs -o tests/$*.run

tests/egg_eater/%.s: tests/egg_eater/%.boa src/main.rs
	cargo run -- $< tests/$*.s

tests/egg_eater/%.run: tests/egg_eater/%.s runtime/start.rs
	nasm -f $(ARCH) tests/egg_eater/$*.s -o tests/egg_eater/$*.o
	ar rcs tests/egg_eater/lib$*.a tests/egg_eater/$*.o
	rustc $(CFLAG) tests/egg_eater/ -lour_code:$* runtime/start.rs -o tests/egg_eater/$*.run

.PHONY: test
test:
	cargo build
	cargo test

clean:
	rm -f tests/*.a tests/*.s tests/*.run tests/*.o
