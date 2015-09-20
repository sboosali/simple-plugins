# simple-plugins




# References

<https://parenz.wordpress.com/2013/08/17/ghc-api-interpreted-compiled-and-package-modules/>

<https://github.com/lukexi/halive/blob/master/exec/Halive.hs>


# Limitations 

* no Template Haskell in plug-in
* no executable profiling 

if you this error message:

    "You can't use Template Haskell with a profiled compiler"

you must remove any Template Haskell from your plug-in. 

if you see this error message:

    "You can't call hscCompileCoreExpr in a profiled compiler"

the following command should fix it (run from your plugin's sandbox's directory):

    cabal configure --disable-executable-profiling

related issues about interactions between the interpreter, profiling, and Template Haskell: 

* https://downloads.haskell.org/~ghc/7.10.2/docs/html/users_guide/template-haskell.html
* https://github.com/gelisam/hawk/issues/130
* https://www.mail-archive.com/cvs-ghc@haskell.org/msg23576.html

