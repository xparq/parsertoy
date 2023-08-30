@echo off
::
:: See build.cmd for usage instructions!
::

set CC=cl
set DEFAULT_CC_OPTIONS=/nologo /W4 /EHsc /std:c++latest

:: Sync these with build.cmd:
set USE_LATEST_SOURCE_AS_DEFAULT=1
set DEFAULT_MODULE=main
set DEFAULT_SOURCE_SUFFIX=.cpp

call %~dp0build.cmd :skip_config %*
