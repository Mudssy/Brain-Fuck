# Brainfuck Interpreter and Compiler
This program is written in Scala and provides two different components for executing Brainfuck programs: an interpreter and a compiler.

# Requirements
Scala 2

# Interpreter Usage
To run a Brainfuck program using the interpreter, call the run function in bf_interpreter.scala and pass in the filename of the program:
eg. run(load_bff("mandelbrot.bf"))


# Compiler Usage
To compile a Brainfuck program using the compiler, call the run function in bf_compiler.scala and pass in the filename of the program:
eg. run(load_bff("mandelbrot.bf"))


# Optimizations
The compiler performs several optimizations to improve the performance of Brainfuck programs.
