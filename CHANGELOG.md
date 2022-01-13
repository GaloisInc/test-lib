# Revision history for test-lib

## 0.3.1

* Make it work with GHC 9

## 0.3

* Add --version flag to report current version
* Add OS specific gold files.  If there is a file named TEST.stdout.OS
  where OS matches the current OS, then this file will be used instead of
  the TEST.stdout.   The names of the OS are:
    darwin            (for Mac)
    freebsd
    linux
    linux-android
    mingw32           (for Windows)
    netbsd
    openbsd

## 0.2.1 -- 2018-01-21

* Only changes to versions of package dependencies.

## 0.1.0.0 -- 2018-12-11

* First version. Released on an unsuspecting world.
