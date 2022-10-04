# Brainfuhs
Brainfuck interpreter in Haskell, simple as that. I'm learning Functional Programming and
haskell out of pure fascination and wanted to try my hand at it with a simple project.

P.S.: Yes, I know I could have just used `ExceptT` as opposed to implementing my own
`EitherT` monad transformer but I'm currently learning about monad transformers and wanted
to try implementing my own just for kicks.

### Running with Cabal
```bash
cabal run brainfuhs-exe -- ./examples/hello_world.bf
```

### TODO
Right now the implementation seems to be performing quite slow, especially on Mandelbrot.
I wonder if I can optimize it somehow.
