#include "../parser.hpp"

//---------------------------------------------------------------------------
// TEST CASES
//---------------------------------------------------------------------------
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "./fw/doctest-setup.hpp"

//
// These cases cannot be run both at once, in the same process, as the static
// init cannot be undone after the 1st one! Run them separately, one at a time:
//
//	test.exe -tc=error-no-init
//	test.exe -tc=init-no-error
//	test.exe -tc=implicit-init
//

using namespace Parsing;

/* NO LONGER AN ERROR: init() is called implicitly when creating the first ATOM rule!
CASE("error-no-init") {
	// init() must be called before doing anything other than creating a Parser!
	RULE nil = _NIL; // This still works, but is not future-proof!
	RULE wrong = _{"_WHITESPACE"}; // ERROR without init()!... (No patterns, will be a literal!)
	Parser p(wrong);
	p.parse(" "); // Checks for the failure!
	CHECK(!p.parse(" ")); // Checks for the failure!
}
*/

/* This won't fail either, as long as Parser() also calls init()!
CASE("error-no-init-op") {
	// init() must be called before doing anything other than creating an ATOM RULE or a Parser!
	RULE nil = _NIL; // This does not call init()!
	RULE wrong = _{nil, nil}; wrong.DUMP();
	Parser p(wrong);
	// ERROR without init(): The implicit SEQ rule has no handler!
	CHECK(!p.parse(" ")); // Checks for the failure!
}
*/

CASE("implicit init by Parser, after no init by OP rules") {
	// init() must be called before doing anything other than creating an ATOM RULE or a Parser!
	RULE t = _T; // This does not call init()!
	RULE r = _{t, t}; r.DUMP(); // Neither does this!
	Parser p(r); // But this one does.
	CHECK(p.parse(""));
}

CASE("explicit init, then Parser(), too (before SEQ_IMPL op call)") {
	init();
	RULE t = _T;
	RULE r = _{t, t};
	Parser p(r);
	CHECK(p.parse(""));
}

CASE("explicit init, then named-pattern RULE, too") {
	init(); // Must be called before doing anything other than creating an ATOM RULE or a Parser!
	RULE r = _{"_WHITESPACE"}; // No longer ERROR without explicit init().
	Parser p(r);
	CHECK(p.parse(" "));
}

CASE("implicit init by first ATOM RULE") {
	RULE r = _{"_WHITESPACE"}; // ERROR without init()!... (No patterns, will be a literal!)
	Parser p(r);
	CHECK(p.parse(" "));
}

CASE("implicit init by tmp Parser one-liner") {
	auto res = Parser(_{_NIL}).parse("");
	DBG("NIL PARSE: {}", res);
	CHECK(!res);
}

CASE("implicit init by tmp Parser one-liner") {
	auto res = Parser(_{_T}).parse("");
	DBG("TRUE-PARSE: {}", res);
	CHECK(res);
}

