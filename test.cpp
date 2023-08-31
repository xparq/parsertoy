//#define COPYLESS_GRAMMAR
#include "parser.hpp"

#define DOCTEST_CONFIG_IMPLEMENT
	//!!
	//!! My cute little macros conflict with the Windows headers (included by DocTest)!
	//!!
	#undef CONST
	#undef OUT
	#undef ERROR
	#undef ATOM
#include "extern/doctest.h"

#include <iostream> // As long as I use it directly, in addition to DocTest

using namespace Parsing;

/*==========================================================================\
  TESTING:
  ...
\===========================================================================*/
#define STRINGIFY_2(a) #a
#define STRINGIFY(a) STRINGIFY_2(a)

#define CONCAT_2(a, b) a##b
#define CONTAT(a, b) CONCAT_2(a, b)

//#define CASE(...) void CONTAT(test_case_,__COUNTER__)()
#define CASE(...) DOCTEST_TEST_CASE( STRINGIFY(CONTAT(test_case_,__COUNTER__)) )


// "Global placeholder" for the test cases to "export" to, if they want:
RULE r = _NIL;

//---------------------------------------------------------------------------
// TEST CASES
//---------------------------------------------------------------------------

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

	Parser("! dummy parser for static init !"s); //!!Just to setup the static lookup table(s)!... :)
		//!! TO TEST OVERALL RULE COPYING, Parser::syntax could be
		//!! made a value, instead of the current const ref!

	TEST.run(); // Run the cases!

// "MANUAL" CASES:

#ifdef COPYLESS_GRAMMAR
//!! The objects assigned to r have all died by now, so copyless can't work this way!...
#error COPYLESS_GRAMMAR requires RULE objects that outlive their access cases!...
#endif

//		r.DUMP();

		RULE dummy{ _{"dummy-atom"} };
		dummy.DUMP();

/*
		Parser parser(r);

		auto text = argc < 2 ? "" : argv[1];

		size_t matched_len = 0xffffffff; auto res = parser.parse(text, matched_len);
		cerr << format("Result: {} (matched: {})", res, matched_len) << "\n";
*/
		cerr << "\n                              >>>>> FINISHED. <<<<<\n\n" << endl;

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
