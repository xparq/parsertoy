#include "../parser.hpp"

#include "./fw/doctest-setup.hpp"

//===========================================================================
int main([[maybe_unused]] int argc, [[maybe_unused]] char** argv)
//===========================================================================
{
//	doctest::Context TEST;
//	TEST.applyCommandLine(argc, argv);

	try {
		using namespace Parsing;

		init();
		//!! Parser("! dummy parser for static init !"s); //!!Just to setup the static lookup table(s)!... :)
			//!! TO TEST OVERALL RULE COPYING, Parser::syntax could be
			//!! made a value, instead of the current const ref!
		Rule rule = _NIL;
		____
		rule.DUMP();

		Parser parser(r);
		auto text = argc < 2 ? "" : argv[1];

		size_t matched_len = 0xffffffff; auto res = parser.parse(text, matched_len);
		cerr << format("Result: {} (matched: {})", res, matched_len) << "\n";
		cerr << "\n                              >>>>> FINISHED. <<<<<\n\n" << endl;

	}
	catch(std::runtime_error& x)
	{
		cerr << x.what() << "\n";
		exit(-1);
	}
	catch(std::exception& x)
	{
		cerr << "- C++ runtime error: " << x.what() << "\n";
		exit(-2);
	}
	catch(...)
	{
		cerr << "- UNKNOWN ERROR(S)!...\n";
		exit(-9);
	}
}
