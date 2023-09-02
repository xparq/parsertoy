#include "../parser.hpp"

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
	#undef CONST
	#undef OUT
	#undef ERROR
	#undef ATOM // Sigh, "ambiguous symbol", for Windows's typedef WORD...

#include "../extern/doctest.h"

/*==========================================================================\
  TESTING:
  ...
\===========================================================================*/
#define STRINGIFY_2(a) #a
#define STRINGIFY(a) STRINGIFY_2(a)
#define CONCAT_2(a, b) a##b
#define CONCAT(a, b) CONCAT_2(a, b)
#define _FIRST_ARG_(x, ...) x

#ifndef _Sz_CONFORMANT_PREPROCESSOR
#error Conformant C++ preprocessor is required for optional test case names!
#endif
#define CASE(...) DOCTEST_TEST_CASE( _FIRST_ARG_(__VA_ARGS__ __VA_OPT__(,) STRINGIFY(CONCAT(test_case_,__COUNTER__))) )


CASE("DBG_TRIM") {
	//!! MAAAN, C++... Just can't pass DBG_TRIM() to format(), as it can't handle temporaries!
	//!! Have to actually create a var for that. :-/
	string src = DBG_TRIM("short");
	DBG("full string: [{}]", src);
	src = DBG_TRIM("this is a long text that triggers trimming with the default length");
	DBG("trimmed: [{}]", src);
}

CASE("DBG_, _DBG_, _DBG") {
	DBG_("Line starter...");
	_DBG_(", and a line fragment");
	_DBG_(" -- and then another line fragment --,");
	_DBG(" and a line end.");

	DBG("This should be a new line now.");
}
