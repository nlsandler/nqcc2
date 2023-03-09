# NQCC2, the Not-Quite-C Compiler

**CAUTION: FREQUENT FORCE PUSHES**

This is the reference implementation for the upcoming book [Writing a C Compiler](https://nostarch.com/writing-c-compiler), a hands-on guide to writing your own compiler for a big chunk of C.

Each commit corresponds to one chapter in the book, to illustrate what needs to change at each step. Because commits are structured this way, this repository sees frequent rebases and force pushes - fork with caution!

This implementation is still a work in progress - the functionality is complete, but needs more work on the readability front.

# Building the Compiler

This compiler is written in OCaml. Building it requires opam, the OCaml package manager (installation instructions [here](https://opam.ocaml.org/doc/Install.html)).

Then do:

```
git clone https://github.com/nlsandler/nqcc2.git
cd nqcc2
opam install . --deps-only
dune build
```

This puts the executable at `_build/default/bin/main.exe`.

# Usage Example

Assume we have this source file at ~/hello_world.c:
```c
int puts(char *c);

int main(void) {
    puts("Hello, world!");
}
```

To compile and run it:
```
$ _build/default/bin/main.exe ~/hello_world.c
$ ~/hello_world
Hello, World!
```






