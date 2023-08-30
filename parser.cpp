/*****************************************************************************
  A simplistic recursive descent parser for simple, ad-hoc tasks
  0.5++

  NOTE:

  - This is currrently just a funny, inconvenient and crippled "reimplementation"
    of dumbed-down regexes, using regexes! ;)

    I guess the main purpose should be actually building an AST, and/or calling
    user callbacks on matching constructs...

  - Decided to copy the source text and be clean and robust (e.g. for threads),
    instead of trying to be copyless (and be kinda brittle and ugly, with a
    string pointer). This is still a toy, not for huge texts, anyway.
 
 -----------------------------------------------------------------------------
  TODO:
 
  - Still crashes on setting up basic constructs!
  - regex
 *****************************************************************************/

/*!! REMINDER for pcre2: https://stackoverflow.com/questions/32580066/using-pcre2-in-a-c-project
  !! - but there's also that C++ wrapper, jpcre2, or somwthing!...

#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

...
PCRE2_SPTR subject = (PCRE2_SPTR) "this"s.c_str();
PCRE2_SPTR pattern = (PCRE2_SPTR) "([a-z]+)|\\s"s.c_str();
...
int errorcode; PCRE2_SIZE erroroffset;
pcre2_code *re = pcre2_compile(pattern, PCRE2_ZERO_TERMINATED, 
                                PCRE2_ANCHORED | PCRE2_UTF, &errorcode,
                                &erroroffset, NULL);
!!*/


//---------------------------------------------------------------------------
#define CONST constexpr static auto
#define OUT

#define ERROR(msg, ...) throw std::runtime_error( \
	std::format("- ERROR: {}", format(msg, __VA_ARGS__)))

#define DBG(msg, ...) cerr << format("DBG> {}", format(msg, __VA_ARGS__)) << endl

	//!  MSVC suppresses the extra , when no args...
	//!! ...BUT THEN, THIS FAILS THERE: ... (str) __VA_OPT__(,) __VA_ARGS__))
	//!! NOT SURE WHAT GCC WOULD DO!


//---------------------------------------------------------------------------

#include <cassert>
#include <exception>
#include <stdexcept>
#include <string>
	using std::string;
#include <string_view>
	using std::string_view;
#include <regex>
	using std::regex;
	using namespace std::regex_constants; //!! refine (filter)
#include <functional>
#include <optional>
	using std::optional;
//!!??#include <variant> // for uniformly AST nodes
#include <vector>
#include <unordered_map>
#include <initializer_list>
	using std::initializer_list;
#include <format>
	using std::format;
//!!Sigh... 2023 Sep: still not yet.
//#include <print>
//	using std::print;
#include <iostream>
	using std::cerr, std::cout, std::endl;

//---------------------------------------------------------------------------
namespace Parsing {

class Parser
{
public:
	//-------------------------------------------------------------------
	// "Curated atoms" (named terminal pattens) -- "metasyntactic sugar" only,
	// as they could as well be just literal patterns. But fancy random regex
	// literals could confuse the parsing, so these "officially" nicely behaving
	// ones are just named & groomed here.
	// (BTW, a user pattern that's not anchored to the left is guaranteed to 
	// fail, as the relevant preg_match() call only returns the match length.
	// It could be extended though, but I'm not sure about multibyte support,
	// apart from my suspicion that mbstring *doesn't* have a position-capturing
	// preg_match (only ereg_... crap). [Wow, checking in from 2023: yep, still
	// that's the case. However, according to the Git log, this thing doesn't
	// even use mbstring any more!])
	// NOTE: PCRE *is* UNICODE-aware! --> http://pcre.org/original/doc/html/pcreunicode.html

	using STRING_MAP = std::unordered_map<string, string>;
//!!	using REGEX_MAP = std::unordered_map<string, regex>;
	static STRING_MAP NAMED_PATTERN; //!! Alas, no constexpr init for dynamic containers... :-/ = {
	                                 //!! And then it's also problematic to make it static... See the ctor!


	//-------------------------------------------------------------------
	// Built-in meta-grammar operators (terminal rules)...

	using ATOM = string; // direct literal or "terminal regex"

	// Opcodes... (regex-inspired)
	//
	// Can be referred to either as Parser::_SOME_OP, or as the 'literal' names.
	// Can be freely extended by users (in sync with the ::$OP map below).
	using OPCODE = int;

	CONST _ATOM        = OPCODE('#');  // Fake "opcode" for atoms, which are not opeators; only defined for a cleaner Parser::match() impl.
	CONST _NIL         = OPCODE('0');  // NOOP
	CONST _SEQ         = OPCODE(',');
	CONST _SEQ_IMPLIED = OPCODE(';');  // SEQ can be omitted; it will be implied for a list of rules that don't start with an opcode
	                                   // (This is to further unify processing: PROD rules all uniformly start with an opcode _internally_.)
	CONST _OR          = OPCODE('|');  // Expects 2 or more arguments
	                                   // - note: adding _AND, too, would make little sense ;)
	CONST _MANY        = OPCODE('+');  // 1 or more (greedy!); expects 1 argument
	CONST _ANY         = OPCODE('*');  // 0 or more (greedy!); shortcut to [_OR [_MANY X] EMPTY]; expects 1 argument
	                                   // - note: "greedy" above means that [A...]A will never match! Be careful!
	CONST _OPT         = OPCODE('?');  // 0 or 1; expects 1 argument
	CONST _NOT         = OPCODE('!');  // Expects 1 argument
	                                   //!!EXPERIMENTAL ONLY! This is tricky with pattern matching... ;)

	// Operator functions...
	struct RULE;
	using OPERATION = std::function<bool(Parser&, size_t src_pos, const RULE&, OUT size_t& matched_len)>;

	// Operator lookup table...
	using OP_MAP = std::unordered_map<OPCODE, OPERATION>;
  	OP_MAP ops = {}; //!! See also NAMED_PATTERN, why not CONST (or at least static)
	// Will be populated later, as:
	// ops[RULE::opcode] = [](Parser&, input_pos, rule&) { ... return match-length or 0; }
	// Can be freely extended by users (respecting the opcode list above).


	//-------------------------------------------------------------------
	// Grammar rules...
	struct RULE
	{
		// User Grammar rule expression, as a recursive RULE tree
		using PRODUCTION = std::vector<RULE>;

		enum {
			NIL,
			CURATED_REGEX,   // built-in "atomic" regex pattern
			CURATED_LITERAL, // built-in "atomic" literal
			USER_REGEX,
			USER_LITERAL,
			OP,
			PROD,

			_DESTROYED_, // See _copy() and _destruct()! :-o
		} type;
		union { //!! variant<ATOM, OPCODE, PRODUCTION> val;
			ATOM       atom; //!! Should be extended later to support optional name + regex pairs! (See d_name, currently!)
			                 //!! Or, actually, better to have patterns as non-atomic types instead (finally)?
					 //!! Also: extend to support precompiled regexes!
			OPCODE     opcode;
			PRODUCTION prod;
		};

		string d_name; // symbolic name, if any (e.g. for named patterns) for diagnostics only

		//-----------------------------------------------------------
		// Queries...
		bool is_atom() const { return type == CURATED_REGEX || type == CURATED_LITERAL
		                           || type == USER_LITERAL || type == USER_REGEX; }
		bool is_prod() const { return type == PROD && !prod.empty(); }
		bool is_opcode() const { return type == OP; }

		//-----------------------------------------------------------
		// Construction...
		RULE(const ATOM& atom);
		RULE(OPCODE opcode): type(OP), opcode(opcode) {}
		RULE(const PRODUCTION& expr): type(PROD), prod(expr) {
			assert(prod.size() == expr.size());
			if (prod.size()) assert(expr[0].type == prod[0].type);
		}
		// Copy & desctuction...
		RULE(const RULE& other): type(_DESTROYED_) { _copy(other); }
		RULE& operator=(const RULE& other) { if (&other != this) { _destruct(); _copy(other); } return *this; }
		RULE(const RULE&& other) = delete; //!! Shouldn't be deleted, but I'm just tired of it...
		~RULE() {
			_destruct();
		}
	private:
		void _set_nil() { type = NIL; d_name = "NIL"; }
		void _destruct() {
//DBG("~RULE: BEGIN (type == {})", (int)type);
			assert (type != _DESTROYED_);
			if      (is_atom()) atom.~string();
			else if (type == PROD) prod.~PRODUCTION(); //! Can't use is_prod() here: it's false if empty()!
			type = _DESTROYED_;
//DBG("~RULE: END");
		}
		void _copy(const RULE& other) {
//DBG("RULE::_copy: BEGIN)");
			//!! Assumes not being constructed already!
			assert(type == _DESTROYED_);
			type = other.type;
			if      (is_atom()) new (&atom) ATOM(other.atom);
			else if (type == PROD) new (&prod) PRODUCTION(other.prod); //! Can't use is_prod() here: it's false if empty()!
			else                opcode = other.opcode; // just a number...
//DBG("RULE::_copy: END (type == {})", (int)type);
		}
	};


	//-------------------------------------------------------------------
	// Parser state...
	//
	// Input:
	RULE syntax;
	string text;
	size_t text_length;
	// Diagnostics:
	int loopguard;
	int depth_reached;
	int rules_tried;
	int terminals_tried;

	CONST DEFAULT_RECURSION_LIMIT = 500;

	void _reset()
	{
		// Sync with the ctor!
		loopguard = DEFAULT_RECURSION_LIMIT;
		depth_reached = DEFAULT_RECURSION_LIMIT;
		rules_tried = 0;
		terminals_tried = 0;
	}
	
	void _reset_text(const string& txt)
	{
		text = txt;
		text_length = txt.length();
		_reset();
	}


	//-------------------------------------------------------------------
	Parser(const RULE& syntax, int maxnest = DEFAULT_RECURSION_LIMIT);

	//-------------------------------------------------------------------
	// Convenience front-ends to match(...)
	bool parse(const string& txt)
	{
		_reset_text(txt);
		size_t matched_length_ignored;
		return match(0, syntax, matched_length_ignored);
	}
	bool parse(const string& txt, OUT size_t& matched_length)
	{
		_reset_text(txt);
		return match(0, syntax, matched_length);
	}

	//-------------------------------------------------------------------
	bool match(size_t pos, const RULE& rule, OUT size_t& len)
	// pos is the source position
	// rule is a syntax rule (tree node)
	// If matches, returns the length of the matched input, otherwise 0.
	//-------------------------------------------------------------------
	{
		--loopguard;
		if (depth_reached > loopguard)
		    depth_reached = loopguard;
		if (!loopguard) {
			ERROR("Infinite loop (in 'match()')?!\n");
		}

		OPERATION f;

		//++rules_tried; //!! #_of_tried_matches, FFS

		if (rule.is_atom()) // "curated regex", literal (or user regex, if still supported...)
		{
			f = ops[_ATOM];
			//!! Should be dispatched further across the various atom types, instead:
			//!!f = atom_handler(rule);
		}
		else if (rule.is_prod())
		{
			f = prod_handler(rule); // First item is the op. of the rule, or else _SEQ is implied:
		}
		else
		{
			ERROR("Invalid grammar at rule: {}", rule.d_name); //!!uniform printable string repr.
		}

		auto res = f(*this, pos, rule, len); //! Remember: `len` is OUT!

		++loopguard;

		return res;
	}

private:
	const OPERATION& prod_handler(const RULE& rule) const
	{
		assert(rule.type == RULE::PROD); //! Never asking an opcode-type RULE object directly (it's just an opcode, makes no sense alone)
		assert(!rule.prod.empty());

		OPCODE opcode;

 		// First item of a "compound" RULE is the op., or else _SEQ is implied.
 		if (!rule.prod[0].is_opcode()) { // Not an opcode, imply a sequence
			opcode = _SEQ_IMPLIED; //!! Will expect a headless rule!
				//!!OPTIM: Shouldn't even be lookup, just return the handler directly!
		} else {
			opcode = rule.prod[0].opcode;
		}
			
		if (auto it = ops.find(opcode); it != ops.end()) {
			decltype(ops.cbegin()) cit = it; //!!?? better one-liner for iter -> const-iter?
			return cit->second;
		} else {
			ERROR("Unimplemented opcode: {} ('{}')", opcode, (char)opcode);
		}
	}
};

Parser::STRING_MAP Parser::NAMED_PATTERN = {}; // Initialize at least to {}, to avoid silent crashes when creating rules without a parser!


//===========================================================================

//---------------------------------------------------------------------------
Parser::RULE::RULE(const string& s)
// A `string` arg. can mean:
//   a) symbol: the name of a curated item (either regex or literal)
//   b) direct ("user") string literal
//   c) direct ("user") regex
// For efficiency, the actual type (`type`) and it's "actual value" (e.g. the
// regex of a named pattern) is resolved and recorded (cached) here.
{
	if (s.empty()) { _set_nil(); return; }

	d_name = s; // Save it as name for diagnostics (even though it's the same as it's value for literals)

	if (auto it = Parser::NAMED_PATTERN.find(s); it != Parser::NAMED_PATTERN.end()) {
		new (&atom) ATOM(it->second); // Replace the atom name with the actual pattern (that's what that lame `second` is)
		type = (atom.length() > 1 && atom[0] == '/' and atom[atom.length()-1] == '/')
			? CURATED_REGEX : CURATED_LITERAL;
DBG("RULE init with named pattern '{}' (->'{}')", d_name, atom);
	} else {
//DBG("- named pattern '{}' not found; using it as literal...", s);
		new (&atom) ATOM(s);
		type = (atom.length() > 1 && atom[0] == '/' and atom[atom.length()-1] == '/')
			? USER_REGEX : USER_LITERAL;
DBG("RULE init with literal '{}'", atom);
	}

	if (type == CURATED_REGEX || type == USER_REGEX) {
		//!! atom = ...; // compile it!
	}
}

/*!!
OPERATION Parser::RULE::op(OPCODE code) const
{
	assert(type == PROD); //! Never asking the opcode directly! :)
	auto   it  = ops.find(code);
	return it != ops.end() ? *it : false; //!!?? ops[NIL] // -- but that can't be (!op)'ed... :-/
}
!!*/


//===========================================================================

//---------------------------------------------------------------------------
Parser::Parser(const RULE& syntax, int maxnest/* = DEFAULT_RECURSION_LIMIT*/):
	// Sync with _reset()!
	syntax(syntax),
	loopguard(maxnest),
	depth_reached(maxnest),
	rules_tried(0),
	terminals_tried(0)
{
	static auto statics_initialized = false;
	if (!statics_initialized)
	{
		assert(NAMED_PATTERN.empty()); // This won't prevent the C++ static init fiasco, but at least we can have a spectacle... ;)
		NAMED_PATTERN = { //!! Alas, no constexpr init for dynamic containers; have to do it here...
			//!!
			//!!CONVERT FROM PHP+PCRE2:
			//!!
			{"_EMPTY"      , R"(//)"},
			{"_SPACE"      , R"(/\\s/)"},
			{"_TAB"        , R"(/\\t/)"},
			{"_QUOTE"      , R"(/\"/)"},
			{"_APOSTROPHE" , R"(/'/)"},
			{"_SLASH"      , R"(/\\//)"},
			{"_IDCHAR"     , R"(/[\\w]/)"}, // [a-zA-Z0-9_], I guess
			{"_ID"         , R"(/[\\w]+/)"},
			{"_HEX"        , R"(/[\\0-9a-fA-F]/)"},
			// UNICODE-safe:
			{"_DIGIT"      , R"(/[\\p{N}]/u)"},
			{"_DIGITS"     , R"(/[\\p{N}]+/u)"},
			{"_LETTER"     , R"(/[\\p{L}]/u)"},
			{"_LETTERS"    , R"(/[\\p{L}]+/u)"},
			{"_ALNUM"      , R"(/[[:alnum:]]/u)"},
			{"_ALNUMS"     , R"(/[[:alnum:]]+/u)"},
			{"_WHITESPACE" , R"(/[\\p{Z}]/u)"},
			{"_WHITESPACES", R"(/[\\p{Z}]+/u)"},
		};

DBG("static init done");
		statics_initialized = true;
	}


	//-------------------------------------------------------------------
	ops[_NIL] = [](Parser&, size_t, const RULE&, OUT size_t&) -> bool
	{
		return false;
	};

	//-------------------------------------------------------------------
	ops[_ATOM] = [](Parser& p, size_t pos, const RULE& rule, OUT size_t& len) -> bool
	{
		assert(rule.is_atom());

		++p.terminals_tried;

		//$m = []; // <-- No need: PHP will create one without notice.
		/*!!
		if (rule.type == RULE::CURATED_REGEX)
		{	
			if (preg_match(Parser::$ATOM[$rule], $p->text, $m, PREG_OFFSET_CAPTURE, $pos)
				&& $m[0][1] == $pos) {
				return strlen($m[0][0]);
			} else	return 0;

		}
		else if (rule.type == RULE::USER_REGEX)
		{
			if (preg_match($rule, $p->text, $m, PREG_OFFSET_CAPTURE, $pos)
				&& $m[0][1] == $pos) {
				return strlen($m[0][0]);
			} else	return 0;
		}
		else // literal non-regex pattern
		!!*/
		{
			len = rule.atom.length();
			//! Case-insensitivity=true will fail for UNICODE chars!
			//! So we just go case-sensitive. All the $ATOMs are like that anyway, and
			//! the user still has control to change it, but won't over a failing match...
			if (p.text_length - pos >= len //! needed to silence a PHP warning...
				&& string_view(p.text).substr(pos, len) == rule.atom)  {
				return true;
			} else	return false;
		}
	};

	//-------------------------------------------------------------------
	ops[_SEQ] = [](Parser& p, size_t pos, const RULE& rule, OUT size_t& len) -> bool
	{
		assert(rule.is_prod());
		assert(rule.prod.size() >= 2);

		len = 0;

		for (auto r = rule.prod.cbegin() + 1; r != rule.prod.cend(); ++r)
		{
			size_t len_add;
			if (!p.match(pos + len, *r, len_add)) return false;
			else len += len_add;
		}
		return true;
	};

	//-------------------------------------------------------------------
	ops[_SEQ_IMPLIED] = [](Parser& p, size_t pos, const RULE& rule, OUT size_t& len) -> bool
	{
		assert(rule.is_prod());
		assert(rule.prod.size() >= 1);

		len = 0;
		for (auto r = rule.prod.cbegin(); !(r == rule.prod.cend()); ++r)
		{
			size_t len_add;
			if (!p.match(pos + len, *r, len_add)) return false;
			else len += len_add;
		}
		return len;
	};

	//-------------------------------------------------------------------
	ops[_OR]  = [](Parser& p, size_t pos, const RULE& rule, OUT size_t& len) -> bool
	{
		assert(rule.prod.size() >= 3);

		for (auto r = rule.prod.cbegin() + 1; r != rule.prod.cend(); ++r)
		{
			if (p.match(pos, *r, len)) {
				return true;
			} else {
				continue;
			}
		}
		return false;
	};

	//-------------------------------------------------------------------
	// Could also be _ANY{1}
	ops[_OPT] = [](Parser& p, size_t pos, const RULE& rule, OUT size_t& len) -> bool
	{
		assert(rule.prod.size() == 2);

		if (!p.match(pos, rule.prod[1], len)) {
			len = 0;
		}
		return true;
	};

	//-------------------------------------------------------------------
	ops[_ANY] = [](Parser& p, size_t pos, const RULE& rule, OUT size_t& len) -> bool
	{
		assert(rule.prod.size() == 2);

		len = 0;
		auto const& r = rule.prod[1];
		do {
			size_t len_add;
			if (!p.match(pos + len, r, len_add)) {
				return true;
			} else {
				len += len_add;

				if (!len) { // We're stuck forever if not progressing!
					ERROR("Infinite loop in `_ANY`!");
				}
			}
		} while (pos + len < p.text_length);

		return true;
	};

	//-------------------------------------------------------------------
	ops[_MANY] = [](Parser& p, size_t pos, const RULE& rule, OUT size_t& len) -> bool
	{
		assert(rule.prod.size() == 2);

		len = 0;
		auto const& r = rule.prod[1];
		bool at_least_one_match = false;
		do {
			size_t len_add;
			if (!p.match(pos + len, r, len_add)) {
				break;
			} else {
				at_least_one_match = true;
				len += len_add;

				if (!len) { // We're stuck forever if not progressing!
					ERROR("Infinite loop in `_MANY`!");
				}
			}
		} while (pos + len < p.text_length);

		return at_least_one_match;
	};

	//-------------------------------------------------------------------
	ops[_NOT] = [](Parser& p, size_t pos, const RULE& rule, OUT size_t& len) -> bool
	{
		assert(rule.prod.size() == 2);

		if (p.match(pos, rule.prod[1], len)) {
			return false;
		} else {
			return true;
		}
	};

} // ctor

} // namespace


//===========================================================================
int main(int argc, char** argv)
{argc, argv;

	using namespace Parsing;

	try {
Parser dummy_for_init({""}); //!!Just to setup the static lookup table(s)!... :)

// OK:
//Parser::RULE r = Parser::ATOM("my atom");
// OK:
//auto r_copy = r;
// OK:
//using Ss = std::vector<string>; Ss s = {"a"};
// OK:
//using Rs = std::vector<Parser::RULE>; Rs r = {Parser::RULE("a")};
// Not needed:
//using Rs = std::vector<Parser::RULE>; Rs r = initializer_list<Parser::RULE>({Parser::RULE("a")});
// OK:
//Parser::RULE r = {Parser::RULE("a")};

//!! FAIL: ctor ambiguous etc.:
//Parser::RULE r = {Parser::RULE("a"), Parser::RULE("b")};
//Parser::RULE r = initializer_list<Parser::RULE>({Parser::RULE("a"), Parser::RULE("b")});

// OK:
//Parser::RULE r = Parser::RULE::PRODUCTION{Parser::RULE("a"), Parser::RULE(" "), Parser::RULE("b")};

// OK:
//Parser::RULE r = Parser::RULE::PRODUCTION{Parser::_MANY, Parser::RULE("x")};

// OK:
//Parser::RULE r = Parser::RULE::PRODUCTION{Parser::_ANY, Parser::RULE("x")};

/* OK:
Parser::RULE r = Parser::RULE::PRODUCTION{
	Parser::RULE("a"),
	Parser::RULE::PRODUCTION{Parser::RULE(Parser::_NIL)},
};*/

///* OK:
Parser::RULE r = Parser::RULE::PRODUCTION{
	Parser::RULE("a"),
	Parser::RULE::PRODUCTION{Parser::_ANY, Parser::RULE(" ")},
	Parser::RULE("b"),
};//*/

/* OK:
Parser::RULE r = Parser::RULE::PRODUCTION{
	Parser::RULE::PRODUCTION{Parser::_OR, Parser::RULE("one"), Parser::RULE("twoe"), Parser::RULE("*") },
};*/

/* OK:
Parser::RULE r = Parser::RULE::PRODUCTION{
	Parser::RULE::PRODUCTION{Parser::_NOT, Parser::RULE(" x ")},
};*/

/* OK:
Parser::RULE r = Parser::RULE::PRODUCTION{
	Parser::RULE::PRODUCTION{Parser::_OR, Parser::RULE("one"), Parser::RULE("two"), Parser::RULE("*") },
	Parser::RULE::PRODUCTION{Parser::_NOT, Parser::RULE("x")},
};*/

//!! Regex not yet...:
//Parser::RULE r = Parser::RULE::PRODUCTION{Parser::RULE("a"), Parser::RULE("_WHITESPACES"), Parser::RULE("b")};

//!! FAIL:
//Parser::RULE r = Parser::RULE::PRODUCTION{"x"};
//Parser::RULE r = Parser::RULE(Parser::RULE::PRODUCTION{"x"});

// OK
//Parser::RULE r = Parser::RULE(Parser::RULE::PRODUCTION{Parser::RULE("x")});

//!!
//!! FAIL: compiles, but "vector too long"!... :-o
//Parser::RULE r = Parser::RULE::PRODUCTION{"a", "b"};
//!!

//!! FAIL:
//Parser::RULE r = Parser::RULE::PRODUCTION{"_WHITESPACE", Parser::RULE::PRODUCTION{"a", "b"}, "_WHITESPACE"};

//!! FAIL: compiles, but "vector too long"!... :-o
/*!!
Parser::RULE r = Parser::RULE::PRODUCTION{
	Parser::RULE("_WHITESPACE"),
	Parser::RULE::PRODUCTION{"a", "b"},
	Parser::RULE("_WHITESPACE"),
};
!!*/

//!! "Ambiguous"...:
//	Parser::RULE g1 = RULE({Parser::RULE("_WHITESPACES"), Parser::RULE("bingo"), Parser::RULE("_WHITESPACES"});
//	Parser::RULE g2 = {Parser::_SEQ, "one", Parser::RULE(Parser::_NIL)};

		Parser p(r);

		auto text = argc < 2 ? "" : argv[1];
		size_t matched_len = 0xffffffff; auto res = p.parse(text, matched_len);

		cerr << format("Result: {} (matched: {})", res, matched_len) << "\n";

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

//cerr << "OK\n";
}