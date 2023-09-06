#include "../parser.hpp"

//---------------------------------------------------------------------------
// TEST CASES
//---------------------------------------------------------------------------
#include "./fw/doctest-setup.hpp"

// Global env. for the test cases...
using namespace Parsing;

/*
RULE r = _NIL; //!!RENAME TO GET IT OUTTA WAY; e.g.: rule = _NIL;

// Constr. from atom, RULE copy-constr.
CASE() {
	r = RULE("my atom"); // ATOM is implied!
	auto r_copy = r;
}
CASE() { using Ss = std::vector<string>; Ss s = {"a"}; }

CASE() { using Rs = std::vector<RULE>; Rs r = {RULE("a")}; }
// Not needed:
CASE() { using Rs = std::vector<RULE>; Rs r = initializer_list<RULE>({RULE("a")}); }

CASE() { r = {RULE("a")}; }

CASE() { r = _{RULE("a"), RULE("b")}; }

//!!FAIL: ...but no wonder; I have no idea, what initializer_list really does ;)
//RULE r = initializer_list<RULE>({RULE("a"), RULE("b")});

CASE() { r = _{RULE("a"), RULE(" "), RULE("b")}; }

CASE() { r = _{_MANY, RULE("x")}; }

CASE() { r = _{_ANY, RULE("x")}; }

CASE() {
	r = _{
		RULE("a"),
		_{RULE(_NIL)},
	};
}

CASE() {
	r = _{
		RULE("a"),
		_{_ANY, RULE(" ")},
		RULE("b"),
	};
}

CASE() {
	r = _{
		_{_OR, RULE("one"), RULE("twoe"), RULE("*") },
	};
}

CASE() {
	r = _{
		_{_NOT, RULE(" x ")},
	};
}

CASE() {
	r = _{
		_{_OR, RULE("one"), RULE("two"), RULE("*") },
		_{_NOT, RULE("x")},
	};
}

//!! Regex not yet...:
//!!{ RULE r = _{RULE("a"), RULE("_WHITESPACES"), RULE("b")} }

CASE() {	r = _{"x"}; }
CASE() {	r = RULE(_{"x"}); }
CASE() {	r = RULE(_{RULE("x")}); }

CASE() {	r = _{RULE("a"), RULE("b")}; }
CASE() {	r = _{"a"s, "b"s}; }
CASE() {	r = _{"a"s}; } // Seems to be the same: RULE r{ ... }

CASE() {	RULE rule("x"); auto p = _{rule}; r = rule; }

//!!---------------------------------------------------------
//!! FAIL: compiles, but "vector too long"!... :-o
//!! RULE r = _{"a", "b"}
//!! auto p         = _{"a", "b"}
//!!
//!! BUT: these don't even compile!...:
//auto p = _{"a", "b", "c"}
//auto p = _{"a", "b"s}
//auto p = _{"a", RULE("b")}
//auto p = _{RULE("a"), "b"}
//!!---------------------------------------------------------

CASE() {	r = _{ _{"a", " ", "b"} }; }

CASE() {
	auto sp = RULE(" "); //!!RULE("_WHITESPACE"); //!!NEED REGEX... until that, it's the literal "/[\\p{Z}]/u"
	r = _{
		_{"a"s, sp, "b"s},
	};
}

CASE() {	r = _{" ", _{"a", "b"}, " "}; }
CASE() {	r = _{"_WHITESPACE", _{"a", "b"}, "_WHITESPACE"}; }

*/

//!! "Ambiguous"...:
//	RULE g1 = RULE({RULE("_WHITESPACES"), RULE("bingo"), RULE("_WHITESPACES"});
//	RULE g2 = {Parser::_SEQ, "one", RULE(Parser::_NIL)}

//CASE() {	RULE copy_from_PROD_ctor = _{_NIL}; }
//CASE() {	RULE PROD_ctor(_{_NIL}); }


CASE("regex smoke test") {
	assert(Parser(_{"  ", "x"}).parse("  x")); // Verify that non-regex still works...
	auto prod = _{"_WHITESPACES", "x"};
	auto rule = RULE{prod};
	     rule.DUMP();
	auto p = Parser(rule);
	auto result = p.parse("  x");

	CHECK(result);
}

CASE("regex curated backslash") {
	auto p = Parser(_{"_BACKSLASH"});
	CHECK((p.parse("\\") && !p.parse("/") && !p.parse(" \\")));
}

CASE("regex curated tab") { // Used to be finicky, for no good reason
	CHECK(Parser(_{"_TAB"}).parse("\t"));
}

CASE("regex curated single chars") {
	auto p = Parser(_{
		"_TAB"        ,
		"_SPACE"      ,
		"_QUOTE"      ,
		"_APOSTROPHE" ,
		"_SLASH"      ,
		"_BACKSLASH"  ,
	});
	CHECK(p.parse("\t \"'/\\"));
}
CASE("regex curated ID") {
	Parser p(_{"_ID"});
	CHECK(!p.parse(""));
	CHECK(!p.parse(" "));
	CHECK(!p.parse("1"));
	CHECK(p.parse("_"));
	CHECK(p.parse("_1"));
	CHECK(p.parse("a"));
	CHECK(p.parse("a1"));
	CHECK(p.parse("a1_"));
}


CASE("_OR: more than 2") {
	Parser p(_{_OR, "_ID", "=", "_DIGITS", ";" });
	p.syntax.DUMP();

	CHECK(p.parse("some_id"));
	CHECK(p.parse("="));
	CHECK(p.parse("1"));
	CHECK(p.parse("190"));
	CHECK(p.parse(";"));
	CHECK(!p.parse("!"));
}
CASE("_OR: mixed with PROD arg") {
	Parser p(_{
		_{_OR, "_DIGITS", _{"a", "b"}},
		";" // ; just for anchoring
	});
	p.syntax.DUMP();

	CHECK(p.parse("1;"));
	CHECK(p.parse("ab;"));
	CHECK(!p.parse(";"));
	CHECK(!p.parse("1 ab;"));
	CHECK(!p.parse("1 a b;"));
}
CASE("_OR: wtf original") {
	RULE x = _{"x"};
	Parser good{_{_OR, x, '$', "++++++++SENTINEL++++++++"}};
	//good.syntax.DUMP();
	CHECK(good.parse("x -> This is correct actually, nothing to see here."));

	Parser bad{_{_{_OR, x, '$', "++++++++SENTINEL++++++++"}, "DUMMY ANCHOR"}};
	//bad.syntax.DUMP();
	CHECK(!bad.parse("x -> But not with an anchor."));
}
CASE("_OR: wtf opcode error") {
	try {
	RULE code = _{_MANY, _{_OR, "_ID", "=", "_DIGITS", ";", "_WHITESPACES"} };
	// '$' is a nonexistent opcode, jus to trigger an error:
	RULE bad  = _{_{_{_OR, code, '$', "++++++++SENTINEL++++++++"}}};
	// "$" is just an ordinary literal, no problems:
	RULE good = _{_{_{_OR, code, "$", "++++++++SENTINEL++++++++"}}};
//	Parser p(good); p.syntax.DUMP();
	Parser p(bad); p.syntax.DUMP();
	p.parse("...check the logs, look for the SENTINEL in the OR loop!");
	} catch (std::runtime_error&) {
		CHECK("OK, got the exception.");
	}
}
CASE("_NOT") {
	Parser p(_{_NOT, "a"});
	p.syntax.DUMP();
	CHECK(!p.parse("a"));
	CHECK(p.parse("b"));
	CHECK(p.parse(" "));
	CHECK(p.parse(""));
}
CASE("_NOT: PROD arg") {
	Parser p(_{_NOT, _{"a", "b"}});
	p.syntax.DUMP();
	CHECK(!p.parse("ab"));
	CHECK(!p.parse("abc"));
	CHECK(p.parse("a"));
	CHECK(p.parse("b"));
	CHECK(p.parse("aab"));
	CHECK(p.parse("ba"));
	CHECK(p.parse(" "));
	CHECK(p.parse(""));
}


CASE("set int - ANY") {
	RULE r = _{
		_{_ANY, "_WHITESPACE"},
		"_ID",
		_{_ANY, "_WHITESPACE"},
		"=",
		_{_ANY, "_WHITESPACE"},
		"_DIGITS"
	}; r.DUMP();
	Parser p(r);
	CHECK(p.parse("option = 1"));
	CHECK(p.parse("option = 12"));
	CHECK(p.parse("option=12"));
	CHECK(p.parse("option   =   12"));
	CHECK(p.parse(" option = 1"));
	CHECK(!p.parse("option = "));
	CHECK(!p.parse("= 12"));
	CHECK(!p.parse(" = 12"));
}

CASE("set int - OPT") {
	RULE r = _{
		_{_OPT, "_WHITESPACES"},
		"_ID",
		_{_OPT, "_WHITESPACES"},
		"=",
		_{_OPT, "_WHITESPACES"},
		"_DIGITS"
	}; r.DUMP();
	Parser p(r);
	CHECK(p.parse("option = 1"));
	CHECK(p.parse("option = 12"));
	CHECK(p.parse("option=12"));
	CHECK(p.parse("option   =   12"));
	CHECK(p.parse("  option = 1"));
	CHECK(!p.parse("option = "));
	CHECK(!p.parse("= 12"));
	CHECK(!p.parse(" = 12"));
}


//===========================================================================
int main(int argc, char** argv)
//===========================================================================
{
	doctest::Context TEST;
	TEST.applyCommandLine(argc, argv);

	try {
		Parsing::init();

		TEST.run();

/*
#ifdef COPYLESS_GRAMMAR
//! The local objects used in the test cases to reconfigure any globals have
//! all died by now, so COPYLESS_GRAMMAR can't work with that!...
//! (Albeit, when/if ready, it should bail out automatically!)
#pragma message("Warning: COPYLESS_GRAMMAR requires RULE objects that outlive their access cases!...")
#endif
*/

	} catch(std::runtime_error& x) {
		cerr << x.what() << "\n";
		exit(-1);
	} catch(std::exception& x) {
		cerr << "- C++ runtime error: " << x.what() << "\n";
		exit(-2);
	} catch(...) {
		cerr << "- UNKNOWN ERROR(S)!...\n";
		exit(-9);
	}
}
