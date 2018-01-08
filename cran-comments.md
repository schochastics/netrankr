## Resubmission

This is a resubmission of the initial submission. In this version I have
added an appropriate reference for the method in the 'Description' field of the 
DESCRIPTION file. 

Note: This is the second try of the resubmission since the first apparently failed.
If you receive the submission twice, I appologize for any inconvenience.

---

## Test environments

* Ubuntu 17.10, R 3.4.2
* win-builder (devel and release)
* macOS 10.11 El Capitan, R 3.4.1 

## R CMD check results

There were no ERRORs or WARNINGs

**NOTEs on Ubuntu:**

```
checking installed package size ... NOTE
  installed size is  7.1Mb
  sub-directories of 1Mb or more:
    libs   5.9Mb
```

I think the size of the libs subdirectory is due to the use of Rcpp, since many functions
of the netrankr package have been written in C++ using Rcpp and Armadillo. The
implementations are needed due to the speed up of C++ code vs plain R, especially
for the function `exact_rank_prob()`. If you think that the size can be reduced in any way, I am more than
happy to resubmit (after all, I am very new to C++).

This note does not appear when using the win-builder or on mac.

```
* checking top-level files ... NOTE
Files ‘README.md’ or ‘NEWS.md’ cannot be checked without ‘pandoc’ being installed.
```

I feel like this should not be a problem. But I may be mistaken.

