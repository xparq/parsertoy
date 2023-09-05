#include "../parser.hpp"

//---------------------------------------------------------------------------
// TEST CASES
//---------------------------------------------------------------------------
#include "./fw/doctest-setup.hpp"

// Global env. for the test cases...
using namespace Parsing;

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

		auto _NAME = OPCODE(':');

		OPERATORS[_NAME] = [](Parser& p, size_t pos, const RULE& rule, OUT size_t& len) -> bool {
/*			// Shift off the CAPTURE prefix...
			//!! ...which, alas, currently means full cloning... :-/
			RULE target_rule(PROD(rule.prod().cbegin() + 1, rule.prod().cend()));

			if (p.match(pos, target_rule, len)) {
				cerr << "\n\n    SNAPSHOT: [" << string_view(p.text).substr(pos, len) << "]" << "\n\n";
				return true;
			}
*/			return false;
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
