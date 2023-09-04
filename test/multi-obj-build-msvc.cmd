@echo off
::
:: See build.cmd for usage instructions!
:: (And keep the var names in sync with build.cmd!)
::

::set USE_LATEST_SOURCE_AS_DEFAULT=1
::set DEFAULT_MODULE=main
::set DEFAULT_SOURCE_SUFFIX=.cpp
set SOURCE_DIR=%~dp0
set TMP_DIR=%~dp0..\tmp\
set EXE_DIR=%TMP_DIR%
set APPEND_CC_OPTIONS=-Wall -wd4464

:: Amazingly, pushd will alter (and fuck up!) %~dp0 (appending the dir part
:: of the script invokation path yet again), so we need to use a copy! :-o
::echo DP0 BEFORE: %~dp0
set saved_script_path=%~dp0
::echo %SOURCE_DIR%
pushd %SOURCE_DIR%
::echo %CD%
::echo DP0 AFTER: %~dp0
::echo saved_script_path: %saved_script_path%
call %saved_script_path%..\build.cmd multi-obj-1.cpp multi-obj-2.cpp %*
popd
