#include "../parser.hpp"

//---------------------------------------------------------------------------
// TEST CASES
//---------------------------------------------------------------------------
#include "./fw/doctest-setup.hpp"

// Global env. for the test cases...
using namespace Parsing;

CASE("CAPTURE: as prefix op") {
	Parser p(_{
		_SAVE,
		_{_OPT, "_WHITESPACES"},
		"_ID",
		_{_OPT, "_WHITESPACES"},
		"=",
		_{_OPT, "_WHITESPACES"},
		"_DIGITS"
	}); p.syntax.DUMP();

	CHECK(p.unnamed_captures.empty());

	CHECK(p.parse("option = 1"));
		CHECK(p.unnamed_captures.size() == 1);
	CHECK(p.parse("  also_OK_with_leading_spaces = 0"));
		CHECK(p.unnamed_captures.size() == 1);
	CHECK(p.parse("   also_OK_with_trailing_spaces = 12   "));
		CHECK(p.unnamed_captures.size() == 1);
	CHECK(!p.parse(" this is not an option = 1"));
	CHECK(!p.parse(" neither is this 1"));
	CHECK(!p.parse("nor_this = one"));
}

CASE("CAPTURE: invalid in the middle of a SEQ, 'alone'!") {
	bool result;
	try {
		Parser p(_{
			_{_OPT, "_WHITESPACES"},
			_SAVE,
			"_ID",
			_{_OPT, "_WHITESPACES"},
			"=",
			"_DIGITS",
		}); p.syntax.DUMP();
		result = !p.parse("option = 1"); // Mustn't succeed; that would mean the error was not catched!
	}
	catch(std::runtime_error& e)
	{
		cerr << e.what() << endl; // Hopefully it's not a different error!... :)
		result = true;
	}
	CHECK(result);

}

CASE("CAPTURE: structured") {
	____
	Parser p(_{
		_{_OPT, "_WHITESPACES"},
		_{_SAVE, _{
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
		CHECK(p.unnamed_captures.size() == 1);
	CHECK(!p.parse("nor_this = one"));
}

CASE("CAPTURE: structured with implicit SEQ shorthand") {
	____
	Parser p(_{
		_{_OPT, "_WHITESPACES"},
		_{_SAVE,
			"_ID",
			_{_OPT, "_WHITESPACES"},
			"=",
			_{_OPT, "_WHITESPACES"},
			"_DIGITS"
		},
/* _OPT being the last rule this is totally redundant:
		// Comment, exploiting the fact that after matching this as the last rule,
		// everything else that follows will be ignored
		_{_OPT, _{ _{_ANY, "_WHITESPACES"}, "////"}},
			// Can't just put "//" as the last ATOM, as that
			// would be mistaken with an empty regex! :) -> #6
*/
	}); p.syntax.DUMP();

	CHECK(p.parse("  capture_this = 1  "));
		CHECK(p.unnamed_captures.size() == 1);
	CHECK(p.parse("  or_this = 1 // Wow, fake comments! :) "));
		CHECK(p.unnamed_captures.size() == 1);
	CHECK(p.parse("  capture_this = 1And BTW, this shouldn't matter here either! ;)"));
	CHECK(!p.parse("not_this = one"));
	CHECK(!p.parse("nor = this"));
}


CASE("CAPTURE: > 1") {
	Parser p(_{
		_{_OPT, "_WHITESPACES"},
		_{_SAVE, "_ID"},
		_{_OPT, "_WHITESPACES"},
		"=",
		_{_OPT, "_WHITESPACES"},
		_{_SAVE, "_DIGITS"}
	}); p.syntax.DUMP();

	CHECK(p.parse("  capture_this = 1  "));
		CHECK(p.unnamed_captures.size() == 2);
		CHECK(p.named_captures.empty());
		for (auto [key, txt] : p.unnamed_captures) { cerr << key << " -> \"" << txt << "\"" << endl; }
	CHECK(p.parse("  or_this = 22 // Wow, fake comments! :) "));
		CHECK(p.unnamed_captures.size() == 2);
		CHECK(p.named_captures.empty());
		for (auto [key, txt] : p.unnamed_captures) { cerr << key << " -> \"" << txt << "\"" << endl; }

	CHECK(!p.parse("nor_this = one"));
}

CASE("CAPTURE: named") {
	Parser p(_{
		_{_OPT, "_WHITESPACES"},
		_{_SAVE_AS, "id", "_ID"},
		_{_OPT, "_WHITESPACES"},
		"=",
		_{_OPT, "_WHITESPACES"},
		_{_SAVE_AS, "val", "_DIGITS"}
	}); p.syntax.DUMP();

	CHECK(p.parse("  capture_this = 1  "));
		CHECK(p.unnamed_captures.empty());
		CHECK(p.named_captures.size() == 2);
		CHECK(p["id"] == "capture_this");
		CHECK(p["val"] == "1");
	CHECK(p.parse("  or_this = 22 // Wow, fake comments! :) "));
		CHECK(p.unnamed_captures.empty());
		CHECK(p.named_captures.size() == 2);
		CHECK(p["id"] == "or_this");
		CHECK(p["val"] == "22");

	CHECK(!p.parse("not this = one"));
}

CASE("CAPTURE: nested") {
	RULE code = _{_MANY, _{_OR, "_ID", "=", "_DIGITS", ";", "_WHITESPACES"} };
	RULE block_in = _{"<", _{_SAVE_AS, "inner", code}, ">"};
	RULE block_out = _{"<", _{_SAVE_AS, "outer",
				_{ _{_OPT, code}, _{_OPT, block_in}, _{_OPT, code} },
			}, ">"};

	Parser p(block_out); p.syntax.DUMP();

	CHECK(p.parse("<outer <inner> block>"));
	CHECK(p["inner"] == "inner");
	CHECK(p["outer"] == "outer <inner> block");

	CHECK(p.parse("<outer < x = 1; y = 22; > block>"));
	CHECK(p["inner"] == " x = 1; y = 22; "); //! Mind the spaces...
	CHECK(p["inner"] !=  "x = 1; y = 22;" ); //! Mind the spaces...
	CHECK(p["outer"] == "outer < x = 1; y = 22; > block");
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
