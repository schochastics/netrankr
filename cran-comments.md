# Update from 1.2.4 to 2.0.0

Major release fixing a large number of bugs found in a code review. Several fixes change
numerical results, see NEWS.md.

## Test environments
* macOS (aarch64), R 4.5.3 (local)
* GitHub Actions: macOS-latest (release), windows-latest (release),
  ubuntu-latest (devel, release, oldrel-1)

## R CMD check results

0 errors | 0 warnings | 0 notes

(apart from the version/maintainer information of the incoming feasibility check)

## Reverse dependencies

We checked 2 reverse dependencies (parsec, tidygraph), comparing R CMD check results across
CRAN and dev versions of this package.

* We saw 0 new problems
* We failed to check 0 packages
