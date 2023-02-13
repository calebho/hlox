# hlox
A Haskell implementation of [Lox](https://craftinginterpreters.com/).

# Development
Requirements:
* GHC 9.2.5
* Cabal

One can grab these requirements using [`ghcup`](https://www.haskell.org/ghcup/)
```shell
$ ghcup install ghc 9.2.5
```

## Tests
```shell
$ cabal test
Build profile: -w ghc-9.2.5 -O1
In order, the following will be built (use -v for more details):
 - hlox-0.1.0.0 (lib) (file README.md changed)
 - hlox-0.1.0.0 (test:test-hlox) (dependency rebuilt)
Preprocessing library for hlox-0.1.0.0..
Building library for hlox-0.1.0.0..
[1 of 1] Compiling Hlox.Lex         ( src/Hlox/Lex.hs, /home/calebh/projects/hlox/dist-newstyle/build/x86_64-linux/ghc-9.2.5/hlox-0.1.0.0/build/Hlox/Lex.o, /home/calebh/projects/hlox/dist-newstyle/build/x86_64-linux/ghc-9.2.5/hlox-0.1.0.0/build/Hlox/Lex.dyn_o )
Preprocessing test suite 'test-hlox' for hlox-0.1.0.0..
Building test suite 'test-hlox' for hlox-0.1.0.0..
[1 of 2] Compiling Hlox.LexSpec     ( test/Hlox/LexSpec.hs, /home/calebh/projects/hlox/dist-newstyle/build/x86_64-linux/ghc-9.2.5/hlox-0.1.0.0/t/test-hlox/build/test-hlox/test-hlox-tmp/Hlox/LexSpec.o, /home/calebh/projects/hlox/dist-newstyle/build/x86_64-linux/ghc-9.2.5/hlox-0.1.0.0/t/test-hlox/build/test-hlox/test-hlox-tmp/Hlox/LexSpec.dyn_o )
Linking /home/calebh/projects/hlox/dist-newstyle/build/x86_64-linux/ghc-9.2.5/hlox-0.1.0.0/t/test-hlox/build/test-hlox/test-hlox ...
Running 1 test suites...
Test suite test-hlox: RUNNING...
Test suite test-hlox: PASS
Test suite logged to:
/home/calebh/projects/hlox/dist-newstyle/build/x86_64-linux/ghc-9.2.5/hlox-0.1.0.0/t/test-hlox/test/hlox-0.1.0.0-test-hlox.log
1 of 1 test suites (1 of 1 test cases) passed.
```
