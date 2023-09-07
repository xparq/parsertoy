#define DOCTEST_CONFIG_IMPLEMENT // Can be overridden by predefining DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include "doctest-customized.h" //! Dir-of-this-file is non-standard, but supported widely enough

#include <iostream>
	using std::cerr;

/*==========================================================================\
  TESTING:
  ...
\===========================================================================*/
#define _STRINGIFY_2_(a) #a
#define _STRINGIFY_(a) _STRINGIFY_2_(a)
#define _CONCAT_2_(a, b) a##b
#define _CONCAT_(a, b) _CONCAT_2_(a, b)
#define _FIRST_OF_(x, ...) x

#ifndef _Sz_CONFORMANT_PREPROCESSOR
#error Conformant C++ preprocessor is required for optional test case names!
#endif
#define CASE(...) DOCTEST_TEST_CASE( _FIRST_OF_(__VA_ARGS__ __VA_OPT__(,) _STRINGIFY_(_CONCAT_(test_case_,__COUNTER__))) )

#define ____ std::cerr << "-------------------------------------------------------------------------------" << std::endl;
