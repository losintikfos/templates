# Simple GLUT Loop Template for Haskell

To get up and running

```
$ touch LICENSE
$ cabal init
$ cabal sandbox init
```
You need to reference "GLUT", so in your cabal file ensure that you have something like this in your "build-depends"

```
build-depends:    base >= 4.6 && < 4.7 -- stock with project
                 ,GLUT >= 2.5.1.1
```

From there, you should get a black window with the following

```
$ cabal install
$ cabal run
```
