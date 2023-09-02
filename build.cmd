@echo off
setlocal enableextensions
setlocal enabledelayedexpansion

rem if _%1_ == _:skip_config_ ( shift /1 & goto :skip_config )

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
:: Change these as applicable...
::
:: Unfortunately, if e.g. a caller script wants to *unset* one, it still must
:: set it to something valid, and only "kinda empty", like "", or .\ for dirs),
:: due to the CMD limitation of empty variables also getting deleted; so you
:: can't really create an empty one.
::---------------------------------------------------------------------------

if __==_%USE_LATEST_SOURCE_AS_DEFAULT%_ set USE_LATEST_SOURCE_AS_DEFAULT=1
if __==_%DEFAULT_SOURCE_SUFFIX%_        set DEFAULT_SOURCE_SUFFIX=.cpp
:: If you want to build a fixed default module, not the latest:
if __==_%DEFAULT_MODULE%_               set DEFAULT_MODULE=test
:: Set dirs to either empty, or *backslash-terminated* paths!
if __==_%SOURCE_DIR%_                   set SOURCE_DIR=
:: These must not even be empty actually, for related CL options to not fail...:
if __==_%TMP_DIR%_                      set TMP_DIR=.\
if __==_%EXE_DIR%_                      set EXE_DIR=.\
:: APPEND_CC_OPTIONS can be used in caller scripts to selectively override options
:: (Because that's how most compilers handle repeated parameters.)
if __==_%CC%_                           set CC=cl
if __==_%PREPEND_CC_OPTIONS%_           set PREPEND_CC_OPTIONS=
if __==_%APPEND_CC_OPTIONS%_            set APPEND_CC_OPTIONS=
if __==_%CC_OPTIONS%_                   set CC_OPTIONS=^
-nologo -W4 -EHsc -std:c++latest -Zc:preprocessor -Fe:%EXE_DIR% -Fo:%TMP_DIR% -Fd:%EXE_DIR%


::---------------------------------------------------------------------------
:: No edits needed below.
::---------------------------------------------------------------------------
rem :skip_config

if __ == _%USE_LATEST_SOURCE_AS_DEFAULT%_ (
	set default_source=%DEFAULT_SOURCE_DIR%%DEFAULT_MODULE%%DEFAULT_SOURCE_SUFFIX%
) else (
	set src_dir=%SOURCE_DIR%
	for /f %%f in ('dir /B /O-D !src_dir!*%DEFAULT_SOURCE_SUFFIX%') do (
		set default_source=!src_dir!%%f
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


set cc_options=%PREPEND_CC_OPTIONS% %CC_OPTIONS% %APPEND_CC_OPTIONS%

rem %* is not affected by shift, but %0 might (ss64.com/nt/shift.html)?! Congratulations!...
echo %CC% %cc_options% %file% %1 %2 %3 %4 %5 %6 %7 %8 %9
     %CC% %cc_options% %file% %1 %2 %3 %4 %5 %6 %7 %8 %9
