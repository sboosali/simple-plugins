# simple-plugins




# References

<https://parenz.wordpress.com/2013/08/17/ghc-api-interpreted-compiled-and-package-modules/>

<https://github.com/lukexi/halive/blob/master/exec/Halive.hs>


# Limitations 

* no executable profiling 

if you see either of these error messages:

    "You can't use Template Haskell with a profiled compiler"

    "You can't call hscCompileCoreExpr in a profiled compiler"

you have to disable executable profiling, with:

    cabal configure --disable-executable-profiling

related issues about interactions between the interpreter, profiling, and Template Haskell: 

* https://downloads.haskell.org/~ghc/7.10.2/docs/html/users_guide/template-haskell.html
* https://github.com/gelisam/hawk/issues/130
* https://www.mail-archive.com/cvs-ghc@haskell.org/msg23576.html

