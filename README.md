# Compiler
A MIPS compiler for the Tiger language

## Pre-requisite:
This compiler is written in SML (Standard ML of New Jersey) v110.99. Get the latest version here: https://www.smlnj.org/.

SPIM can be used to test the generated MIPS assembly, which can be downloaded here: http://spimsimulator.sourceforge.net/.

## The Tiger Language:
The target language of this compiler, Tiger, is a small language with nested functions, record values with implicit pointers, arrays, integer and string variables, and a few simple structured control constructs. A detailed manual can be found in the PDF file `tiger.pdf` in this repo.

## Instructions:
To use the compiler, first type `sml` in terminal window to start the repl.

Then, type `CM.make "sources.cm"` to compiler the modules.

Now, assembly for the Tiger language can be generated using the function `Main.compile`. For example, to compiler the program `queens.tig` which solves the 8 Queens Problem, simply type `Main.compile queens.tig`, and an MIPS assembly file `queens.s` will be generated in the same directory.

