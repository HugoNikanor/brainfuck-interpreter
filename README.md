# Scheme Brainfuck
A simple Brainfuck interpreter written in Scheme.
It currently depends slightly on Guiles module system, but that should
be possible to write out without to much trouble.

## Usage
	./bf < file.bf

## Issues
+ Starting brackets (`[`) doesn't check if the current value is zero.

