# NQCC2, the Not-Quite-C Compiler

**CAUTION: FREQUENT FORCE PUSHES**

This is the reference implementation for the book [Writing a C Compiler](https://nostarch.com/writing-c-compiler), a hands-on guide to writing your own compiler for a big chunk of C.

Each commit corresponds to one chapter in the book, to illustrate what needs to change at each step. Because commits are structured this way, this repository sees frequent rebases and force pushes - fork with caution!

This implementation is still a work in progress - the functionality is complete, but needs more work on the readability front.

# Installation

NQCC2 requires OCaml version 4.14.0 or later, and depends on several OCaml libraries. You can use opam, the OCaml package manager, to install a recent version of OCaml and all the dependencies.

**Note for macOS ARM users:** do _not_ perform these setup instructions under Rosetta 2. You should _build_ NQCC2 for ARM, but _invoke_ it from an x86-64 shell under Rosetta 2, and everything should just work. One possible exception: if you're using an x86-64 Homebrew installation (under the `/usr/local/` prefix) you may need to perform all these steps under Rosetta 2; however, I haven't tested this.

## Installing opam and OCaml

You can skip this section if you already have a recent-enough OCaml installation.

1. Install opam through your system's package manager.

   ### On Ubuntu:

   ```
   sudo apt install opam
   ```

   **Note:** The Ubuntu `opam` package is currently a few versions behind the latest release. If you want the very latest version of opam (not required to build nqcc2), follow [these instructions](https://opam.ocaml.org/doc/Install.html), but be aware that this may require some [manual troubleshooting](https://github.com/ocaml/opam/issues/5968).

   ### On macOS:
   ```
   brew install opam
   ```

2. Initialize opam and check the installed compiler version.
   ```
   opam init
   eval $(opam env) # activate default OCaml installation in the current shell
   ocaml --version
   ```

   If the ocaml version is below 4.14.0 and/or you want a dedicated sandbox for this project and its dependencies, go to step 3. Otherwise you can go ahead and [build nqcc2](#building-nqcc2).

3. Install the latest compiler version in its own sandbox.
   ```
   opam switch create nqcc2-env 5.2.0
   eval $(opam env --switch=nqcc2-env) # activate switch in current shell
   ```

## Building NQCC2

```
git clone https://github.com/nlsandler/nqcc2.git # check out the repo
cd nqcc2
opam install . --deps-only # install dependencies through opam
dune build # build it
```

This puts the executable at `_build/default/bin/main.exe`.

**Note:** the commits for chapters 11 and 12 introduce new dependencies.
If you have an earlier commit checked out when you run `opam install . --deps-only`, these dependencies won't be installed.
If `dune build` ever gives you an error like this:

```
File "lib/dune", line 48, characters 45-55:
48 |  (libraries re camlp-streams zarith integers)
                                         ^^^^^^^^
Error: Library "integers" not found.
```

just rerun `opam install . --deps-only` to install missing dependencies.

## Running the Tests

NQCC2 has a handful of unit tests, which you can run with this command:

```
dune runtest
```

However, the primary way to test NQCC2 is the [writing a C compiler test suite](https://github.com/nlsandler/writing-a-c-compiler-tests).

macOS ARM users should run `dune runtest` under ARM, but should run the separate test suite under Rosetta 2.

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






