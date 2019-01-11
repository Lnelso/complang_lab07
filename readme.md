# Inlining and Local function definition

This repository contains the extension of our compiler done for the Compiler Language Processing (CS-320) course taught by Viktor Kuncak at Ecole Polytechnique Fédéral de Lausanne.

This extension consists in the implemention of the inlining optimization (including constant folding) and the ability to define local functions.

## Run our examples
The use of our optimizations are best shown when executing the examples contained in the examples/ folder.

To have a better visibility on the rewriting process of the AST, the rewritten program is systematically printed on the stdout output.

- Inlining.scala

This example demonstrates the use of the inlining implementation whith constant folding applied on a If-statement, on a unary operation (Neg) and on several binary operations.
- NoInlining.scala

This example demonstates the use of the local function definition.

- InliningAndLocal.scala

In this example, automatic inlining is applied on a local function. This is the example that was presented in the project description.

As for the last labs, the examples can be compiled through the use of "run library/Std.scala examples/[NAME_OF_EXAMPLE].scala" and run on the terminal with "node wasmout/[NAME_OF_EXAMPLE].js"
