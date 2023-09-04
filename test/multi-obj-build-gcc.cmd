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
set SOURCE_DIR=%~dp0
set TMP_DIR=%~dp0..\tmp\
set EXE_DIR=%TMP_DIR%
set APPEND_CC_OPTIONS=-Wall

call %~dp0..\build.cmd multi-obj-1.cpp multi-obj-2.cpp %*
