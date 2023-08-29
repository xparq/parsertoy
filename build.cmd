@echo off

if xx == x%1x (
	set file=parser.cpp
) else (
	set file=%1
)

cl /nologo /W4 /EHsc /std:c++latest /Zi %file%
