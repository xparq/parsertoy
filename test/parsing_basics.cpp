#include "../parser.hpp"

//---------------------------------------------------------------------------
// TEST CASES
//---------------------------------------------------------------------------
#include "./fw/doctest-setup.hpp"

// Global env. for the test cases...
using namespace Parsing;

/*
Rule r = _NIL; //!!RENAME TO GET IT OUTTA WAY; e.g.: rule = _NIL;

// Constr. from atom, Rule copy-constr.
CASE() {
	r = Rule("my atom"); // Atom is implied!
	auto r_copy = r;
}
CASE() { using Ss = std::vector<string>; Ss s = {"a"}; }

CASE() { using Rs = std::vector<Rule>; Rs r = {Rule("a")}; }
// Not needed:
CASE() { using Rs = std::vector<Rule>; Rs r = initializer_list<Rule>({Rule("a")}); }

CASE() { r = {Rule("a")}; }

CASE() { r = _{Rule("a"), Rule("b")}; }

//!!FAIL: ...but no wonder; I have no idea, what initializer_list really does ;)
//Rule r = initializer_list<Rule>({Rule("a"), Rule("b")});

CASE() { r = _{Rule("a"), Rule(" "), Rule("b")}; }

CASE() { r = _{_MANY, Rule("x")}; }

CASE() { r = _{_ANY, Rule("x")}; }

CASE() {
	r = _{
		Rule("a"),
		_{Rule(_NIL)},
	};
}

CASE() {
	r = _{
		Rule("a"),
		_{_ANY, Rule(" ")},
		Rule("b"),
	};
}

CASE() {
	r = _{
		_{_OR, Rule("one"), Rule("twoe"), Rule("*") },
	};
}

CASE() {
	r = _{
		_{_NOT, Rule(" x ")},
	};
}

CASE() {
	r = _{
		_{_OR, Rule("one"), Rule("two"), Rule("*") },
		_{_NOT, Rule("x")},
	};
}

//!! Regex not yet...:
//!!{ Rule r = _{Rule("a"), Rule("_WHITESPACES"), Rule("b")} }

CASE() {	r = _{"x"}; }
CASE() {	r = Rule(_{"x"}); }
CASE() {	r = Rule(_{Rule("x")}); }

CASE() {	r = _{Rule("a"), Rule("b")}; }
CASE() {	r = _{"a"s, "b"s}; }
CASE() {	r = _{"a"s}; } // Seems to be the same: Rule r{ ... }

CASE() {	Rule rule("x"); auto p = _{rule}; r = rule; }

//!!---------------------------------------------------------
//!! FAIL: compiles, but "vector too long"!... :-o
//!! Rule r = _{"a", "b"}
//!! auto p         = _{"a", "b"}
//!!
//!! BUT: these don't even compile!...:
//auto p = _{"a", "b", "c"}
//auto p = _{"a", "b"s}
//auto p = _{"a", Rule("b")}
//auto p = _{Rule("a"), "b"}
//!!---------------------------------------------------------

CASE() {	r = _{ _{"a", " ", "b"} }; }

CASE() {
	auto sp = Rule(" "); //!!Rule("_WHITESPACE"); //!!NEED REGEX... until that, it's the literal "/[\\p{Z}]/u"
	r = _{
		_{"a"s, sp, "b"s},
	};
}

CASE() {	r = _{" ", _{"a", "b"}, " "}; }
CASE() {	r = _{"_WHITESPACE", _{"a", "b"}, "_WHITESPACE"}; }

*/

//!! "Ambiguous"...:
//	Rule g1 = Rule({Rule("_WHITESPACES"), Rule("bingo"), Rule("_WHITESPACES"});
//	Rule g2 = {Parser::_SEQ, "one", Rule(Parser::_NIL)}

//CASE() {	Rule copy_from_Prod_ctor = _{_NIL}; }
//CASE() {	Rule Prod_ctor(_{_NIL}); }

CASE("empty atom - user literal") {
	CHECK(Parser("").parse("anything"));
}
CASE("empty atom - user regex") {
	CHECK(Parser("//").parse("anything"));
}
CASE("empty atom - curated regex") {
	CHECK(Parser("_EMPTY").parse("anything"));
}
/* no such thing (yet?):
CASE("empty atom - cuarted literal") {}
*/
CASE("empty prod.") {
	CHECK(Parser(_{}).parse("anything"));
}


CASE("regex smoke test") {
	assert(Parser(_{"  ", "x"}).parse("  x")); // Verify that non-regex still works...
	auto prod = _{"_WHITESPACES", "x"};
	auto rule = Rule{prod};
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
CASE("_OR: mixed with Prod arg") {
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
	Rule x = _{"x"};
	Parser good{_{_OR, x, '$', "++++++++SENTINEL++++++++"}};
	//good.syntax.DUMP();
	CHECK(good.parse("x -> This is correct actually, nothing to see here."));

	Parser bad{_{_{_OR, x, '$', "++++++++SENTINEL++++++++"}, "DUMMY ANCHOR"}};
	//bad.syntax.DUMP();
	CHECK(!bad.parse("x -> But not with an anchor."));
}
CASE("_OR: wtf opcode error") {
	try {
	Rule code = _{_MANY, _{_OR, "_ID", "=", "_DIGITS", ";", "_WHITESPACES"} };
	// '$' is a nonexistent opcode, jus to trigger an error:
	Rule bad  = _{_{_{_OR, code, '$', "++++++++SENTINEL++++++++"}}};
	// "$" is just an ordinary literal, no problems:
	Rule good = _{_{_{_OR, code, "$", "++++++++SENTINEL++++++++"}}};
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
CASE("_NOT: Prod arg") {
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
	Rule r = _{
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
	Rule r = _{
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
#pragma message("Warning: COPYLESS_GRAMMAR requires Rule objects that outlive their access cases!...")
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
