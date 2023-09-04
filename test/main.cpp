//#define COPYLESS_GRAMMAR
#include "../parser.hpp"

//---------------------------------------------------------------------------
// TEST CASES
//---------------------------------------------------------------------------
#include "./fw/doctest-setup.hpp"

// Global env. for the test cases...
using namespace Parsing;

CASE("RULE: empty from empty str") {
	RULE r = ""; //!! This happens to work without a prior init(), but not future-proof!
	                //!! Also, it won't itself call init! 
	r.DUMP();
	CHECK(!r.prod().empty()); // Should've been converted to PROD{NIL}, so not empty
	____
}
CASE("RULE: empty from empty PROD 1") {
	//RULE rule = {}; //! This won't compile, _{} needed!
	RULE r = _{}; //!! This happens to work without a prior init(), but not future-proof!
	                 //!! Also, it won't itself call init! 
	r.DUMP();
	CHECK(!r.prod().empty()); // Should've been converted to PROD{NIL}, so not empty
	____
}
CASE("RULE: empty from empty PROD 2") {
	PROD prod{};
	RULE r{prod};
	r.DUMP();
	CHECK(!r.prod().empty()); // Should've been converted to PROD{NIL}, so not empty
	____
}


CASE("RULE: op = NIL - internal-only use case, but works") {
	RULE r{_NIL};
	CHECK((r.is_opcode() && !r.is_prod()));
	CHECK(r.type == RULE::OP);
	CHECK(r.opcode == _NIL);
	____
}
CASE("RULE: op = T") {
	RULE r{_T};
	CHECK((r.is_opcode() && !r.is_prod()));
	CHECK(r.type == RULE::OP);
	CHECK(r.opcode == _T);
	____
}

CASE("PROD: empty - TBD...") {
	PROD prod{};
}

CASE("PROD: NIL (with implicitly created RULE)") {
	PROD prod{_NIL};
	____
}
CASE("PROD: T (with implicitly created RULE)") {
	PROD prod{_T};
	____
}


CASE("PROD: directly from ATOM-RULE") {
	____
	string atom = "atom 1";
	RULE atom_rule{atom};
	atom_rule.DUMP();
}

CASE("PROD: directly from ATOM-RULE, plus prod from it") {
	____
	string atom = "atom 1";
	RULE atom_rule{atom};

	PROD prod{atom_rule}; //! Alas, a copy by std::vector, no matter what...
	                      //!! BUT WHY TWO?!?!?!
			      //!! There's not even a temporary here! :-o
			      //!! And even if there was one, I'd expect copy elision
			      //!! for that, or at least a move! :-o
	atom_rule.DUMP();
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


CASE("RULE: move from temp prod.") {
	RULE r{_{_NIL}};
	r.DUMP();
	____
	CHECK(!"Why not moved?");
}



/*!!
CASE("RULE: append - smoke test: CRASHING!...") {
	RULE r{""};
//!!!!	r.append(_NOT, "x");
	____
}
!!*/


/*
RULE rule = _{}; //!! This happens to work without a prior init(), but not legal/future-proof!
                 //!! This is NOT equivalend with creating an OP-type RULE directly with r = _NIL!
                 //!! This latter is not a supported use case.

//!! Rename r to rule below, to (or) sync with the gloal's actual name!

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
	auto r = RULE{prod};
	     r.DUMP();
	auto p = Parser(r);
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


//===========================================================================
int main(int argc, char** argv)
//===========================================================================
{
	doctest::Context TEST;
	TEST.applyCommandLine(argc, argv);

	try {
		init(); // Setup lookup table(s) etc.
		____

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
