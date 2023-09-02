@echo off
::
:: See build.cmd for usage instructions!
:: (And keep the var names in sync with build.cmd!)
::

set CC=g++
set CC_OPTIONS=-Wall --std=c++23

::set USE_LATEST_SOURCE_AS_DEFAULT=1
::set DEFAULT_MODULE=main
::set DEFAULT_SOURCE_SUFFIX=.cpp
set SOURCE_DIR=test\
set TMP_DIR=tmp\
::set EXE_DIR=.\

call %~dp0build.cmd %*
