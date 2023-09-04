@echo off
::
:: See build.cmd for usage instructions!
:: (And keep the var names in sync with build.cmd!)
::

::set USE_LATEST_SOURCE_AS_DEFAULT=1
::set DEFAULT_MODULE=main
::set DEFAULT_SOURCE_SUFFIX=.cpp
set SOURCE_DIR=test\
set TMP_DIR=tmp\
::set EXE_DIR=.\
set APPEND_CC_OPTIONS=-Wall -wd4464

call %~dp0build.cmd %*
