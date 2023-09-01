//#define COPYLESS_GRAMMAR
#include "parser.hpp"

#define DOCTEST_CONFIG_IMPLEMENT
	//!!
	//!! My cute little macros conflict with the Windows headers (included by DocTest)!
	//!!
	#undef CONST
	#undef OUT
	#undef ERROR
	#undef ATOM // Sigh, "ambiguous symbol", for Windows's typedef WORD...
#include "extern/doctest.h"

#include <iostream>
	using std::cerr;

using namespace Parsing;

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
//OLD:
//#define CASE(...) DOCTEST_TEST_CASE( STRINGIFY(CONCAT(test_case_,__COUNTER__)) )
//OLDEST:
//#define CASE(...) void CONCAT(test_case_,__COUNTER__)()



//---------------------------------------------------------------------------
// TEST CASES
//---------------------------------------------------------------------------

#define ____ std::cerr << "-----------------------------------------------------------------------------" << std::endl;

// Globals for the test cases to reuse...
RULE r = _NIL;

CASE("PROD: directly from ATOM-RULE") {
	string atom = "atom 1";
	RULE atom_rule{atom};
	PROD prod{atom_rule}; //! Alas, copy by std::vector, no matter what!
	                      //!! BUT WHY TWO?!?!?!
//	RULE rule{prod};
//	rule.DUMP();
	____
	CHECK(!"Why two RULE copies?");
}

CASE("PROD: implicit from ATOM (-> RULE)") {
	string atom = "atom 2";
	PROD prod{atom}; //! Alas, copy by std::vector, no matter what!
	____
}

CASE("PROD: move auto-created RULE from cstring literal") {
	PROD prod{"cstr atom"};
	____
	CHECK(!"Why not moved?");
}

CASE("PROD: move auto-created RULE from std::string literal") {
	PROD prod{"std::string atom"s};
	____
	CHECK(!"Why not moved?");
}
/*!!NOT MOVED EVEN WITH EXPLICIT move()
CASE("PROD: move auto-created RULE from cstring literal") {
	PROD prod{std::move("cstr atom")};
	____
	CHECK(!"Why not moved?");
}

CASE("PROD: move auto-created RULE from std::string literal") {
	PROD prod{std::move("std::string atom"s)};
	____
	CHECK(!"Why not moved?");
}
!!*/
CASE("PROD: move explicit temporary RULE(NIL)") {
	PROD prod{RULE(_NIL)};
	____
	CHECK(!"Why not moved?");
}

CASE("PROD: emplace RULE(std::string literal)") {
	PROD prod; prod.emplace_back("std::string atom"s);
	____
//	CHECK(!"Why not moved?");
}

CASE("RULE: append - smoke test") {
	RULE rule{""};
	rule.append(_NOT, "x");
	____
//	CHECK(!"Why not moved?");
}



/*

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

//===========================================================================
int main([[maybe_unused]] int argc, [[maybe_unused]] char** argv)
//===========================================================================
{
	doctest::Context TEST;
	TEST.applyCommandLine(argc, argv);

	try {

	____
	Parser("! dummy parser for static init !"s); //!!Just to setup the static lookup table(s)!... :)
		//!! TO TEST OVERALL RULE COPYING, Parser::syntax could be
		//!! made a value, instead of the current const ref!

	____

	TEST.run(); // Run the cases!
	____

#ifdef COPYLESS_GRAMMAR
//!! The objects assigned to r have all died by now, so copyless can't work this way!...
#pragma message("Warning: COPYLESS_GRAMMAR requires RULE objects that outlive their access cases!...")
#endif

/*
		r.DUMP();
		Parser parser(r);

		auto text = argc < 2 ? "" : argv[1];

		size_t matched_len = 0xffffffff; auto res = parser.parse(text, matched_len);
		cerr << format("Result: {} (matched: {})", res, matched_len) << "\n";
		cerr << "\n                              >>>>> FINISHED. <<<<<\n\n" << endl;
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
