#include "../parser.hpp"

//---------------------------------------------------------------------------
// SMOKE TESTING FOR ACCIDENTALLY DUPLICATED "SHADHOW" OBJECTS, WHICH
// DID HAPPEND EARLIER (due to mistaken use of `static` for shared data)!
//---------------------------------------------------------------------------
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "./fw/doctest-setup.hpp"

using namespace Parsing;
CASE("explicit init, then named-pattern RULE, too") {
	init(); // Must be called before doing anything other than creating an ATOM RULE or a Parser!
	RULE r = _{"_WHITESPACE"}; // No longer ERROR without explicit init()...
	Parser p(r);
	CHECK(p.parse(" "));
}

CASE("implicit init by first ATOM RULE") {
	RULE r = _{"_WHITESPACE"}; // No longer ERROR without explicit init()...
	Parser p(r);
	CHECK(p.parse(" "));
}

CASE("implicit init by (tmp) Parser(T) one-liner") {
	CHECK(Parser(_{_T}).parse(""));
}

CASE("implicit init by (tmp) Parser(NIL) one-liner") {
	CHECK(!Parser(_{_NIL}).parse(""));
}
