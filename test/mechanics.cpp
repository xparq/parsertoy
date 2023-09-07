//#define COPYLESS_GRAMMAR
#include "../parser.hpp"

//---------------------------------------------------------------------------
// TEST CASES
//---------------------------------------------------------------------------
#include "./fw/doctest-setup.hpp"

// Global env. for the test cases...
using namespace Parsing;

CASE("diagnostic memo") {
	Rule r = _{"_IDCHAR", "_BACKSLASH"}; // named patterns
	r.DUMP();
}

/* This won't compile:
CASE("Rule: {}") {
	//Rule r{}; // "no default ctor" -- FFS, C++, this is an empty aggreg.!
	r.DUMP();
}
*/
CASE("Rule: {NIL}") {
	Rule r{_NIL};
	r.DUMP();
	CHECK((r.is_opcode() && !r.is_prod()));
	CHECK(r.type == Rule::OP);
	CHECK(r.opcode == _NIL);
}
CASE("Rule: ctor-op= NIL - internal-only use case, but should work") {
	Rule r = _NIL;
	CHECK((r.is_opcode() && !r.is_prod()));
	CHECK(r.type == Rule::OP);
	CHECK(r.opcode == _NIL);
	____
}
CASE("Rule: ctor-op= empty str") {
	Rule r = ""; //!! This happens to work without a prior init(), but not future-proof!
	                //!! Also, it won't itself call init! 
	r.DUMP();
	CHECK(!r.prod().empty()); // Should've been converted to Prod{NIL}, so not empty
	____
}
CASE("Rule: ctor-op= empty Prod") {
	//Rule rule = {}; //! This won't compile, _{} needed!
	Rule r = _{}; //!! This happens to work without a prior init(), but not future-proof!
	                 //!! Also, it won't itself call init! 
	r.DUMP();
	CHECK(!r.prod().empty()); // Should've been converted to Prod{NIL}, so not empty
	____
}
CASE("Rule: from empty Prod {}") {
	Prod prod{};
	Rule r{prod};
	r.DUMP();
	CHECK(!r.prod().empty()); // Should've been converted to Prod{NIL}, so not empty
	____
}
CASE("Rule: {T}") {
	Rule r{_T};
	CHECK((r.is_opcode() && !r.is_prod()));
	CHECK(r.type == Rule::OP);
	CHECK(r.opcode == _T);
	____
}

CASE("Prod: empty - TBD...") {
	Prod prod{};
}

CASE("Prod: NIL (with implicitly created Rule)") {
	Prod prod{_NIL};
	____
}
CASE("Prod: T (with implicitly created Rule)") {
	Prod prod{_T};
	____
}


CASE("Prod: directly from Atom-Rule") {
	____
	string atom = "atom 1";
	Rule atom_rule{atom};
	atom_rule.DUMP();
}

CASE("Prod: directly from Atom-Rule, plus Prod from it") {
	____
	string atom = "atom 1";
	Rule atom_rule{atom};

	Prod prod{atom_rule}; //! Alas, a copy by std::vector, no matter what...
	                      //!! BUT WHY TWO?!?!?!
			      //!! There's not even a temporary here! :-o
			      //!! And even if there was one, I'd expect copy elision
			      //!! for that, or at least a move! :-o
	atom_rule.DUMP();
	CHECK(!"Why two Rule copies?");
}

CASE("Prod: implicit from Atom (-> Rule)") {
	string atom = "atom 2";
	Prod prod{atom}; //! Alas, copy by std::vector, no matter what!
	____
}

CASE("Prod: move auto-created Rule from cstring literal") {
	Prod prod{"cstr atom"};
	____
	CHECK(!"Why not moved?");
}

CASE("Prod: move auto-created Rule from std::string literal") {
	Prod prod{"std::string atom"s};
	____
	CHECK(!"Why not moved?");
}
/*!!NOT MOVED EVEN WITH EXPLICIT move()
CASE("Prod: move auto-created Rule from cstring literal") {
	Prod prod{std::move("cstr atom")};
	____
	CHECK(!"Why not moved?");
}

CASE("Prod: move auto-created Rule from std::string literal") {
	Prod prod{std::move("std::string atom"s)};
	____
	CHECK(!"Why not moved?");
}
!!*/
CASE("Prod: move explicit temporary Rule(NIL)") {
	Prod prod{Rule(_NIL)};
	//!! This would be even worse, preventing copy elision (MSVC -Wall warned!):
	//!! Prod prod{std::move(Rule(_NIL))};
	____
	CHECK(!"Why not moved?");
}
CASE("Prod: emplace Rule(std::string literal)") {
	Prod prod; prod.emplace_back("std::string atom"s);
	____
//	CHECK(!"Why not moved?");
}


CASE("Rule: move from temp prod.") {
	Rule r{_{_NIL}};
	r.DUMP();
	____
	MESSAGE("Was it still moved? (1 copy is ok for the vector)");
}

CASE("Rule: move from temp Rule") {
	____
	Rule rfrom{_NIL};
	Rule rto{std::move(rfrom)};
	rto.DUMP();
	____
	MESSAGE("Was it still moved? (1 copy is ok for the vector)");
}


//---------------------------------------------------------------------------
CASE("relink parents after external vector copy") {
	Rule r = {_{_OPT, _{_T}}};
	r.DUMP();
	Rule q{r};
	q.DUMP();
}



/*!!
CASE("Rule: append - smoke test: CRASHING!...") {
	Rule r{""};
//!!!!	r.append(_NOT, "x");
	____
}
!!*/


/*
Rule rule = _{}; //!! This happens to work without a prior init(), but not legal/future-proof!
                 //!! This is NOT equivalend with creating an OP-type Rule directly with r = _NIL!
                 //!! This latter is not a supported use case.

//!! Rename r to rule below, to (or) sync with the gloal's actual name!

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



CASE("regex smoke test") {
	assert(Parser(_{"  ", "x"}).parse("  x")); // Verify that non-regex still works...
	auto prod = _{"_WHITESPACES", "x"};
	auto r = Rule{prod};
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

CASE("infinite recursion detected") {
	try {
		Parser p(_{
			_{_DEF, "self", _{_USE, "self"}},
			_{_USE, "self"}
		}); p.syntax.DUMP();
		p.parse("!");
		CHECK(false);
	} catch (std::runtime_error& x) {
		cerr << "\nThere should be an infinite recursion exception caught below:\n\n";
		cerr << x.what() <<  endl;
		CHECK("OK, got an error, but verify in the logs it's really the loop-guard!");
	}
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
