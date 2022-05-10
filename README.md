# RNetica
An R Package that provides a binding for the Netica (R) API

Provides support parameterized tables for Bayesian networks, particularly the IRT-like DiBello tables. 
Also, provides some tools for visualing the networks.

To install the latest verison of this package on your machine, you can use:
```
install.packages("RNetica",repos=c(ralmond="https://ralmond.r-universe.dev", cran="https://cloud.r-project.org/"))
```
## Not (completely) Free and Open Source

This provides interconnection between the proprietary Netica Bayesian network engine and R. 
The glue code (R and c code on github) is free and open source; however, it downloads and installs the Netica Bayesian network engine from Norsys, LLC.  

Netica is the property of Norsys, LLC (https://norsys.com/) and is subject to the terms of their license.  In particular, the version installed by 
RNetica is a limited version.  It will run the test suite and allow you to evaluate the software.  However, to unlock full sized models, you will need 
purchase an API license from Norsys.

## More Information

Many of the algorithms in the package are documented in 

Almond, Mislevy, Steinberg, Yan and Williamson (1995).  _Bayesian Networks in Educational Assessment._  Springer.

Project information is available at https://pluto.coe.fsu.edu/RNetica/RNetica.html

## Acknowledgements

Work on RNetica, CPTtools and Peanut has been sponsored in part by the
following grants:

* Bill & Melinda Gates Foundation grant "Games as Learning/Assessment:
Stealth Assessment" (#0PP1035331, Val Shute, PI)

* National Science Foundation grant "DIP:
Game-based Assessment and Support of STEM-related Competencies"
(#1628937, Val Shute, PI).

* National Science Foundation grant "Mathematical Learning via
Architectual Design and Modeling Using E-Rebuild." (#1720533,
Fengfeng Ke, PI)

* Intitute of Educational Statistics grant "Exploring Adaptive
  Cognitive and Affective Learning Support for Next-Generation STEM
  Learning Games", (R305A170376,Russell Almond, PI)
  
