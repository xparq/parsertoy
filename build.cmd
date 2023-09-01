@echo off
if _%1_ == _:skip_config_ ( shift /1 & goto :skip_config )

::
:: Use: build [option...] [file...] [option...]
::
:: E.g.:
::
::	build  <-- Will compile & link the latest source in ./ by default.
::                 (See/change the cfg. below!)
::
::	build *.cpp /Fe:my.exe
::	build /Zi main.cpp
::	build /GL /Ox main.cpp /link /ltcg /genprofile
::	build /GL main.cpp /link /ltcg:pgupdate /useprofile
::

::---------------------------------------------------------------------------
:: Change these as applicable... (Comment out "bools" to set to "false"!)
::---------------------------------------------------------------------------

set USE_LATEST_SOURCE_AS_DEFAULT=1

:: If not using the latest, use this:
set DEFAULT_MODULE=main
set DEFAULT_SOURCE_SUFFIX=.cpp

set CC=cl
set DEFAULT_CC_OPTIONS=/nologo /W4 /EHsc /std:c++latest /Zc:preprocessor


::---------------------------------------------------------------------------
:: No edits needed below.
::---------------------------------------------------------------------------
:skip_config


if "" == "%USE_LATEST_SOURCE_AS_DEFAULT%" (
	set default_source=%DEFAULT_MODULE%%DEFAULT_SOURCE_SUFFIX%
) else (

	for /f %%f in ('dir /B /O-D *%DEFAULT_SOURCE_SUFFIX%') do (
		set default_source=%%f
		goto :break
	)
)
:break


if xx == x%1x (
	set file=%default_source%
) else (
	set file=%1
	shift /1
)

rem %* is not affected by shift, but %0 might (ss64.com/nt/shift.html)?! Congratulations!...
echo %CC% %DEFAULT_CC_OPTIONS% %file% %1 %2 %3 %4 %5 %6 %7 %8 %9
     %CC% %DEFAULT_CC_OPTIONS% %file% %1 %2 %3 %4 %5 %6 %7 %8 %9
