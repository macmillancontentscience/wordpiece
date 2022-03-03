## v. 2.1.3
* Submitting update 2022-03-03
* Implemented significant speed and memory improvements.


## Test environments
* local R installation, R 4.0.3
* win-builder (devel)
* Windows Server 2022, R-devel, 64 bit (rhub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub)
* Fedora Linux, R-devel, clang, gfortran (rhub)

There is a NOTE when testing for Windows Server, which apparently has been showing up for all packages lately:

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```

## R CMD check results

0 errors | 0 warnings | 0 notes 
