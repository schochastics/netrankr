## Update from 0.2.0 to 0.2.1

This is an updated version of the package which contains bug fixes.

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

This note does not appear when using the win-builder or on mac.

```
* checking top-level files ... NOTE
Files ‘README.md’ or ‘NEWS.md’ cannot be checked without ‘pandoc’ being installed.
```

I feel like this should not be a problem. But I may be mistaken.

