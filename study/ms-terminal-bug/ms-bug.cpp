#include "ms-bug.hpp"
#define DOCTEST_CONFIG_IMPLEMENT
#include "extern/doctest.h"

#include <iostream>

using namespace Parsing;

DOCTEST_TEST_CASE("")
{
	auto prod = RULE::PRODUCTION{"_EMPTY", "x"};
	auto rule = RULE{prod};
	     rule.DUMP();
	auto dummy2 = Parser(rule);
	CHECK(false);
}

int main()
{
	RULE dummy = _NIL;
	Parser(""s); //!!Just to setup the static lookup table(s)!... :)

	doctest::Context TEST; TEST.run();

	std::cerr << "\n";
}
