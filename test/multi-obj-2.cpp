#include "../parser.hpp"

//---------------------------------------------------------------------------
// SMOKE TESTING FOR ACCIDENTALLY DUPLICATED "SHADHOW" OBJECTS, WHICH
// DID HAPPEND EARLIER (due to mistaken use of `static` for shared data)!
//---------------------------------------------------------------------------
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "./fw/doctest-setup.hpp"

using namespace Parsing;

CASE("error-no-init") {
	// init() must be called before doing anything other than creating a Parser!
	Rule sp = _{"_WHITESPACE"};
	Parser p(sp);
// These watches only make sense, if also printed from parser.hpp, i.e. from prod_handler():
DBG("OPERATORS in the test case: {}", (void*)&OPERATORS);
DBG("OPERATORS.size in the test case: {}", OPERATORS.size());
	p.parse(" ");
	CHECK(p.parse(" "));
}

CASE("explicit init, then named-pattern Rule, too") {
	init(); // Must be called before doing anything other than creating an ATOM Rule or a Parser!
	Rule r = _{"_WHITESPACE"}; // No longer ERROR without explicit init()...
	Parser p(r);
	CHECK(p.parse(" "));
}

CASE("implicit init by first ATOM Rule") {
	Rule r = _{"_WHITESPACE"}; // No longer ERROR without explicit init()...
	Parser p(r);
	CHECK(p.parse(" "));
}

CASE("implicit init by (tmp) Parser(T) one-liner") {
	CHECK(Parser(_{_T}).parse(""));
}

CASE("implicit init by (tmp) Parser(NIL) one-liner") {
	CHECK(!Parser(_{_NIL}).parse(""));
}
