#include "../parser.hpp"

//---------------------------------------------------------------------------
// TEST CASES
//---------------------------------------------------------------------------
#include "./fw/doctest-setup.hpp"

// Global env. for the test cases...
using namespace Parsing;

auto _CAPTURE = '('; // See `OPERATORS[_CAPTURE] = ...` in main()!

CASE("CAPTURE: as prefix op") {
	Parser p(_{
		_CAPTURE,
		_{_OPT, "_WHITESPACES"},
		"_ID",
		_{_OPT, "_WHITESPACES"},
		"=",
		_{_OPT, "_WHITESPACES"},
		"_DIGITS"
	}); p.syntax.DUMP();

	CHECK(p.parse("option = 1"));
	CHECK(p.parse("  also_OK_with_leading_spaces = 0"));
	CHECK(p.parse("   also_OK_with_trailing_spaces = 12   "));
	CHECK(!p.parse(" this is not an option = 1"));
	CHECK(!p.parse(" neither is this 1"));
	CHECK(!p.parse("nor_this = one"));
}

// This one only captures the "net value", without surrounding whitespace,
// exploiting the fact that the last rule finishes before the trailing spaces!
CASE("CAPTURE: invalid in the middle of a SEQ, 'alone'!") {
	Parser p(_{
		_{_OPT, "_WHITESPACES"},
		_CAPTURE,
		"_ID",
		_{_OPT, "_WHITESPACES"},
		"=",
		"_DIGITS",
	}); p.syntax.DUMP();

	CHECK(p.parse("option = 1"));
	CHECK(p.parse("  also_OK_with_leading_spaces = 0"));
	CHECK(p.parse("   also_OK_with_trailing_spaces = 12   "));
	CHECK(!p.parse(" this is not an option = 1"));
	CHECK(!p.parse(" neither is this 1"));
	CHECK(!p.parse("nor_this = one"));

	// This should still succeed, but only as a happy side-effect of "partial full" matching!
	CHECK(p.parse("  or_this = 1 // Wow, comments!"));
}

CASE("CAPTURE: structured") {
	____
	Parser p(_{
		_{_OPT, "_WHITESPACES"},
		_{_CAPTURE, _{
				"_ID",
				_{_OPT, "_WHITESPACES"},
				"=",
				_{_OPT, "_WHITESPACES"},
				"_DIGITS"
			}
		},
		_{_ANY, "_WHITESPACES"},
		_{"////"} // Comment, exploiting the fact that matching this as the
		          // last rule, everything else that follows will be ignored...
		          // Alas, can't just put "//" there, as that would be
			  // mistaken with an empty regex! -> #6

	}); p.syntax.DUMP();

	CHECK(p.parse("  or_this = 1 // Wow, comment ignored! :) "));
	CHECK(!p.parse("nor_this = one"));
}

CASE("CAPTURE: structured with implicit SEQ shorthand") {
	____
	Parser p(_{
		_{_OPT, "_WHITESPACES"},
		_{_CAPTURE,
			"_ID",
			_{_OPT, "_WHITESPACES"},
			"=",
			_{_OPT, "_WHITESPACES"},
			"_DIGITS"
		},
		
		// Comment, exploiting the fact that after matching this as the last rule,
		// everything else that follows will be ignored
		_{_OPT, _{ _{_ANY, "_WHITESPACES"}, "////"}},
			// Can't just put "//" as the last ATOM, as that
			// would be mistaken with an empty regex! :) -> #6


	}); p.syntax.DUMP();

	CHECK(p.parse("  capture_this = 1  "));
	CHECK(p.parse("  or_this = 1 // Wow, comments!"));
	CHECK(!p.parse("nor_this = one"));
}



//===========================================================================
int main(int argc, char** argv)
//===========================================================================
{
	doctest::Context TEST;
	TEST.applyCommandLine(argc, argv);

	try {
		Parsing::init();

		OPERATORS[_CAPTURE] = [](Parser& p, size_t pos, const RULE& rule, OUT size_t& len) -> bool {
			// Shift off the CAPTURE prefix...
			//!! ...which, alas, currently means full cloning... :-/
			RULE target_rule(PROD(rule.prod().cbegin() + 1, rule.prod().cend()));

			if (p.match(pos, target_rule, len)) {
				cerr << "\n\n    SNAPSHOT: [" << string_view(p.text).substr(pos, len) << "]" << "\n\n";
				return true;
			}
			return false;
		};


		TEST.run();

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
