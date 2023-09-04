#include "../parser.hpp"

//---------------------------------------------------------------------------
// TEST CASES
//---------------------------------------------------------------------------

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "./fw/doctest-setup.hpp"

CASE("DBG_TRIM") {
	//!! MAAAN, C++... Just can't pass DBG_TRIM() to format(), as it can't handle temporaries!
	//!! Have to actually create a var for that. :-/
	string src = DBG_TRIM("short");
	DBG("full string: [{}]", src);
	src = DBG_TRIM("this is a long text that triggers trimming with the default length");
	DBG("trimmed: [{}]", src);
}

CASE("DBG_, _DBG_, _DBG") {
	DBG_("Line starter...");
	_DBG_(", and a line fragment");
	_DBG_(" -- and then another line fragment --,");
	_DBG(" and a line end.");

	DBG("This should be a new line now.");
}

// I've seen a strange warning from DOCTEST about the CHECK expr. can't be "too complex"... :-o
// And I've seen a !false result once reported false, too, but couldn't reproduce it.
CASE("VERIFY DOCTEST CHECK - 1") {
	using namespace Parsing;
	auto res = Parser(_{_NIL}).parse("");
	DBG("NIL PARSE: {}", res);
	CHECK(!res);
}

CASE("VERIFY DOCTEST CHECK - 2") {
	using namespace Parsing;
	auto res = Parser(_{_NIL}).parse("");
	DBG("NIL PARSE: {}", res);
	CHECK( !Parser(_{_NIL}).parse("") );
}

CASE("VERIFY DOCTEST CHECK - 3") {
	using namespace Parsing;
	auto res = Parser(_{_NIL}).parse("");
	DBG("NIL PARSE: {}", res);
	CHECK( !(Parser(_{_NIL}).parse("")) ); // in parens
}
