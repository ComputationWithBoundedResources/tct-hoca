tct-hoca
========
This executable is part of the _Tyrolean Complexity Tool (TcT)_ and provides
automatic complexity analysis of _higher-order-systems_. It bundles
  * [hoca](https://github.com/ComputationWithBoundedResources/hoca)
  * [tct-trs](https://github.com/ComputationWithBoundedResources/hoca)
to a single executable.

Requirements
------------
  * [Glasgow Haskell Compiler, version 7.10](http://www.haskell.org/ghc/) 
  * [minismt, version 0.6](http://cl-informatik.uibk.ac.at/software/minismt/)

The tool is only tested under GNU/Linux.

Install
-------
For building, you need [ghc](http://www.haskell.org/ghc/) and
[stack](https://github.com/commercialhaskell/stack). Execute `stack build`.

Usage
-----
The installation provides an executable `tct-hoca`. For full options, run
`tct-hoca --help`.

