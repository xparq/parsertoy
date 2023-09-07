#include "../parser.hpp"

//---------------------------------------------------------------------------
// TEST CASES
//---------------------------------------------------------------------------
#include "./fw/doctest-setup.hpp"

// Global env. for the test cases...
using namespace Parsing;


CASE("def basic") {
	Parser p(_{_DEF, "crap", _{_T, _{"subrule"}}});
	p.syntax.DUMP();
	p.run();
	p.syntax.DUMP();
}

CASE("def in a seq") {
	Parser p(_{
		_{_DEF, "crap", _{_T, _{"subrule"}}},
		"anchor"
	});
	p.syntax.DUMP();
	p.run();
	p.syntax.DUMP();
}

CASE("def atom alias") {
	Parser p(_{
		_{_DEF, "begin", "{"},
		_{_DEF, "end", "}"},

		_{_USE, "begin"}, "_WHITESPACES", _{_USE, "end"}
	});
	p.syntax.DUMP();
	CHECK(p.parse("{ }"));
	CHECK(!p.parse("}{"));
//	p.syntax.DUMP();
}

CASE("call") {
	Parser p(_{
		_{_DEF, "crap", "_ID"},
		"<",
		_{_USE, "crap"},
		">"
	});
	p.syntax.DUMP();
	____
	p.parse("x");
	____
	p.syntax.DUMP();
	____
	CHECK(p.parse("<match_this_please>"));
	____
}

/*
CASE(": nested") {
	Rule code = _{_MANY, _{_OR, "_ID", "=", "_DIGITS", ";", "_WHITESPACES"} };
	Rule block = _{_DEF, "BLOCK", _{"<", code, ">"}};
	Rule block_out = _{"<", _{_SAVE_AS, "outer",
				_{ _{_OPT, code}, _{_OPT, block_in}, _{_OPT, code} },
			}, ">"};

	Parser p(block); p.syntax.DUMP();

	CHECK(p.parse("<outer <inner> block>"));
	CHECK(p["inner"] == "inner");
	CHECK(p["outer"] == "outer <inner> block");

	CHECK(p.parse("<outer < x = 1; y = 22; > block>"));
	CHECK(p["inner"] == " x = 1; y = 22; "); //! Mind the spaces...
	CHECK(p["inner"] !=  "x = 1; y = 22;" ); //! Mind the spaces...
	CHECK(p["outer"] == "outer < x = 1; y = 22; > block");

}
*/

/*
CASE("recursive basic") {
	Rule block = _{_{_OR, "x", _{_SELF}}}, ">"}};
	Parser p(block); p.syntax.DUMP();

//	CHECK(p.parse("x = 1"));
//	CHECK(p.parse("<x = 1>"));
	CHECK(p.parse("<x = 1 <z = 2>>"));
//	CHECK(!p.parse("<x = 1 <z = 2>"));
}
*/

CASE("recursion: nested blocks") {
	Rule code = _{_MANY, _{_OR, "_ID", "=", "_DIGITS", ";", "_WHITESPACES"} };
	Rule def_block = _{_DEF, "block", _{"<", _{_ANY, _{_OR, code, _{_USE, "block"}}}, ">"}};
	Parser p(_{def_block, _{_USE, "block"}}); p.syntax.DUMP();

	CHECK(!p.parse("x = 1")); // -> CASE "nested mixed code & blocks"
	CHECK(p.parse("<x = 1>"));
	CHECK(p.parse("<x = 1 <z = 2>>"));
	CHECK(!p.parse("<x = 1 <z = 2>")); // missing close tag
}
CASE("recursion: bad nesting, should fail!") {
	Rule code = _{_MANY, _{_OR, "_ID", "=", "_DIGITS", ";", "_WHITESPACES"} };
	Rule def_block = _{_DEF, "block", _{"<", _{_ANY, _{_OR, code, _{_USE, "block"}}}, ">"}};
	Parser p(_{def_block, _{_USE, "block"}}); p.syntax.DUMP();

	CHECK(!p.parse("< <x>")); // missing close tag
}
CASE("recursion: nested mixed code & blocks") {
	Rule code = _{_MANY, _{_OR, "_ID", "=", "_DIGITS", ";", "_WHITESPACES"} };
	Rule def_block = _{_DEF, "block", _{"<", _{_ANY, _{_OR, code, _{_USE, "block"}}}, ">"}};
	Parser p(_{def_block,
		_{_OR, code, _{_USE, "block"}}
	}); p.syntax.DUMP();

	CHECK(p.parse("x = 1"));
	CHECK(p.parse("<x = 1>"));
	CHECK(p.parse("<x = 1 <z = 2>>"));
	CHECK(!p.parse("<x = 1 <z = 2>")); // missing close tag
	CHECK(p.parse("< x = 1; y = 2; <z = 3; <and even this> or <this = 0;>> >"));
}


//===========================================================================
int main(int argc, char** argv)
//===========================================================================
{
	doctest::Context TEST;
	TEST.applyCommandLine(argc, argv);

	try {
		Parsing::init();
/*!!
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! CONST VIOLATION!
		//!!OPERATORS[_NAME] = [](Parser& p, size_t pos, Rule& rule, OUT size_t& len) -> bool {
		CONST_OPERATORS[_DEF] = [](Parser& p, size_t pos, const Rule& rule, OUT size_t& len) -> bool {
			assert(rule.prod().size() == 3);
			assert(rule.prod()[1].is_atom());
			auto name = rule.prod()[1].atom;
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! CONST VIOLATION HERE! :-/
			auto target_rule = const_cast<Rule&>(rule).prod().begin() + 2;
			target_rule->name = name;
//			auto& r = const_cast<Rule&>(rule);
//			r.prod().erase(r.prod().begin(), r.prod().begin() + 2);
DBG("_DEF: '{}', this: {}, parent: {}, lookup: {}", name, (const void*)&rule, (void*)rule._parent, (void*)p.syntax._lookup(name));
			return true;
		};

		CONST_OPERATORS[_USE] = [](Parser& p, size_t pos, const Rule& rule, OUT size_t& len) -> bool {
			assert(rule.prod().size() == 2);
			assert(rule.prod()[1].is_atom());
			auto name = rule.prod()[1].atom;
			auto target_rule = p.syntax._lookup(name);
			if (!target_rule) {
DBG("_USE: '{}' was not found!", name);
				return false;
			}
DBG("_USE: trying rule [{}] at pos {}...", (void*)target_rule, pos);
			return p.match(pos, *target_rule, len);
		};
!!*/
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
