## Update from 0.2.1 to 0.3 (Resubmission)

an existing function was extended and one new function added.  
No API breaking changes

Fixed issues with non existing urls and changed http to https. 

## Update from 0.2.0 to 0.2.1 (Resubmission)

This is an updated version of the package which contains bug fixes.

The resubmission was necessary due to the following Note:
```
Check: use of SHLIB_OPENMP_*FLAGS in Makefiles 
Result: NOTE 
     src/Makevars.win: SHLIB_OPENMP_CXXFLAGS is included in PKG_CXXFLAGS but not in PKG_LIBS
     src/Makevars.win: SHLIB_OPENMP_CFLAGS is included in PKG_LIBS but not in PKG_CFLAGS
    Use of these macros is discussed in sect 1.2.1.1 of ‘Writing R
    Extensions’. The macros for different languages may differ so the
    matching macro must be used in PKG_CXXFLAGS (etc) and match that used
    in PKG_LIBS (except for F77: see the manual). 
```

I fixed the Makefiles and run a test with `rhub::check_for_cran()` and the Note seems 
to be gone.

---

## Test environments

* Ubuntu 17.10, R 3.4.2
* win-builder (devel and release)
* macOS 10.11 El Capitan, R 3.4.1 

## R CMD check results

There were no ERRORs or WARNINGs

**NOTES**

```
checking installed package size ... NOTE
  installed size is  7.1Mb
  sub-directories of 1Mb or more:
    libs   5.9Mb
```

This note does not appear when using the win-builder or on mac.

```
Maintainer: ‘David Schoch <david.schoch@manchester.ac.uk>’

New maintainer:
  David Schoch <david.schoch@manchester.ac.uk>
Old maintainer(s):
  David Schoch <david.schoch@gess.ethz.ch>
```

I changed my University affiliation from ETH Zurich to University of Manchester and my old Email address
is not valid anymore.