#ifndef _PARSERTOY_HPP_
#define _PARSERTOY_HPP_
/*****************************************************************************
  General-purpose, extensible recursive descent parser for small tasks

  NOTE:

  - In a sense, this is just a naive/uninformed reimplementation of some basic
    regex functionality -- using regexes...

    But, the main feature would be actually building an AST, and supporting
    user hooks etc. for matching constructs, just haven't got 'round to it yet.
    At least this is a "structured regex engine" (or "programmable regexes"
    is also echoing in my mind); i.e. *regular* regular expressions are not
    for structured text, while this can effortlessly handle nested constructs.

  - It copies the source text, so that it can be kept a little more clean
    & robust (e.g. for threading), instead of trying to be copyless (and
    be kinda brittle and ugly with pointers, or offering a false sense of
    security with a string_view). It's still a toy, not for huge texts, so...

  - If you need to #include this in more than one translation units, then
    #define PARSERTOY_DEDUP for all but the first one. (This way the most
    common use case of only including it once can be kept the simplest.)

 -----------------------------------------------------------------------------
  TODO:

  - Change the ATOM type name, which clashes with he Win32 headers!
  - Change the other names, like those of my toy macros, which also
    get stampeded by the Windows headers!

  - WTF is wrong with the move semantics?! RULE's move-ctor is never triggered!
    UPDATE: it kinda does now, but still not fully understand the cases
    where it doesn't.

  - multi-emplace _prod_append(...); -- and a similar ctor?? (possible?)

  - RULE("") and RULE(NIL) should create a *valid* empty rule.
    UPDATE: I think it (kinda?) does now? Not quire sure in which exaxt
            cases, though.
    And then there could also be a default ctor then... The reason I still
    don't have it is I don't want to encourage that. Everything works without
    it (so far), and there's no obvious benefit.

 *****************************************************************************/

//!! Force-disable (the anyway useless) COPYLESS_GRAMMAR.
#ifdef COPYLESS_GRAMMAR
#warning COPYLESS_GRAMMAR is not properly implemented yet; disabled. (See #19!)
#undef COPYLESS_GRAMMAR

//=============================================================================
//---------------------------------------------------------------------------
// My ad-hoc "ground-levelling" base language layer...
//---------------------------------------------------------------------------
#include <cassert>
#include <exception>
#include <stdexcept>
#include <format>
	using std::format;
//#include <print> //!! Sigh... 2023 Sep: still can't use this yet... Wow.
//	using std::print;
#include <string>
	using std::string;
	using namespace std::literals::string_literals;
#ifndef NDEBUG
#include <iostream>
	using std::cerr, std::cout, std::endl;
#endif

//!!
//!! My plain, undecorated toy macros conflict with the Windows headers (included by DocTest), and who knows what else...
//!!
#define CONST constexpr static auto
#define OUT
#define FALLTHROUGH // For switch cases with intentionally no `break;`
/*
#define IGNORE // To silence unused fn. arg warnings. Usage: IGNORE arg1, arg2;
               // This macro can't just be [[maybe_unused]], unfortunately. :)
               // MSVC is fine with just a var list, and so was GCC 12 -Wall, but v13
               // started complaining about "left operand of comma operator has no effect".
*/

//! For variadic macros, e.g. for calling std::format(...):
//!
//! The old MSVC preproc. suppresses the extra ',' when no more args... But, it
//! doesn't understand __VA_OPT__, so the std. c++20 way of
//!
//! __VA_OPT__(,) __VA_ARGS__ approach can't be unified... Welcome to 2023!
//!
//! (GCC is OK with that, as is the new MCVC preproc, activated by /Zc:preprocessor)
#if defined(__GNUC__) \
	|| defined(_MSC_VER) && (!defined(_MSVC_TRADITIONAL) || !_MSVC_TRADITIONAL)
#  define _Sz_CONFORMANT_PREPROCESSOR 1
#elif defined(_MSC_VER) && defined(_MSVC_TRADITIONAL) && _MSVC_TRADITIONAL // Old MS prep.
#  define _Sz_OLD_MSVC_PREPROCESSOR 1
#endif

#ifndef NDEBUG
//# //!! These empty preproc. lines would throw off GCC's error reporting line refs! :-o
#  if defined(_Sz_CONFORMANT_PREPROCESSOR)
#    define DBG(msg, ...) std::cerr << std::format("DBG> {}", std::format(msg __VA_OPT__(,) __VA_ARGS__)) << std::endl
     // Same as DBG(), but with no trailing \n (for continuation lines)
#    define DBG_(msg, ...) std::cerr << std::format("DBG> {}", std::format(msg __VA_OPT__(,) __VA_ARGS__))
     // Continuation lines -- same as DBG(), but without the DBG prefix
#    define _DBG(msg, ...) std::cerr << std::format(msg __VA_OPT__(,) __VA_ARGS__) << std::endl
     // Line fragment -- neither DBG prefix, no trailing \n
#    define _DBG_(msg, ...) std::cerr << std::format(msg __VA_OPT__(,) __VA_ARGS__)
#  elif defined(_Sz_OLD_MSVC_PREPROCESSOR)
#    define DBG(msg, ...) std::cerr << std::format("DBG> {}", std::format(msg, __VA_ARGS__)) << std::endl
#    define DBG_(msg, ...) std::cerr << std::format("DBG> {}", std::format(msg, __VA_ARGS__))
#    define _DBG(msg, ...) std::cerr << std::format(msg, __VA_ARGS__) << std::endl
#    define _DBG_(msg, ...) std::cerr << std::format(msg, __VA_ARGS__)
#  else
#    error Unsupported compiler toolset (not MSVC or GCC/CLANG)!
#  endif

#  define DBG_DEFAULT_TRIM_LEN 30
   // Trim length is ignored as yet, just using the default:
   #define DBG_TRIM(str, ...) (std::string_view(str).length() > DBG_DEFAULT_TRIM_LEN - 3 ? \
		(std::string(std::string_view(str).substr(0, DBG_DEFAULT_TRIM_LEN - 3))) + "..." : \
		str)
//#
#else
#  define DBG(msg, ...)
#  define DBG_(msg, ...)
#  define DBG_(msg, ...)
#  define DBG_TRIM(str, ...)
#endif


// Note: ERROR() below is _not_ a debug feature!
#if defined(_Sz_CONFORMANT_PREPROCESSOR)
#  define ERROR(msg, ...) throw std::runtime_error(std::format("- ERROR: {}", std::format(msg __VA_OPT__(,) __VA_ARGS__)))
#elif defined(_Sz_OLD_MSVC_PREPROCESSOR)
#  define ERROR(msg, ...) throw std::runtime_error(std::format("- ERROR: {}", std::format(msg, __VA_ARGS__)))
#else
#  error Unsupported compiler toolset (not MSVC or GCC/CLANG)!
#endif

// Tame MSVC -Wall just a little
#ifdef _MSC_VER
#  pragma warning(disable:5045) // Compiler will insert Spectre mitigation for memory load if /Qspectre switch specified
#  pragma warning(disable:4514) // unreferenced inline function has been removed
#  pragma warning(disable:4582) // constructor is not implicitly called
#  pragma warning(disable:4583) // destructor is not implicitly called
// Too late here, though (use -wd4464 on the command-line):
#  pragma warning(disable:4464) // relative include path contains '..'
#endif
//---------------------------------------------------------------------------
//=============================================================================


//---------------------------------------------------------------------------
#include <string_view>
	using std::string_view;
#include <regex>
	using namespace std::regex_constants; //!! refine (filter)
#include <functional> // function, reference_wrapper, ...
#include <utility> // move
#include <optional>
	using std::optional;
//!!??#include <variant> // for uniformly AST nodes
#include <vector>
#include <unordered_map>
#include <map>
//#include <initializer_list>
//	using std::initializer_list;

//---------------------------------------------------------------------------
namespace Parsing {

	void init(); // Call this first, unless you start with a Parser() constructor!

	CONST EMPTY_STRING = ""s;

	using ATOM  = string; // direct literal or "terminal regex"
	using REGEX = std::regex; //!! When changing it (e.g. to PCRE2), a light adapter class would be nice!
	using STRING_MAP = std::unordered_map<string, string>;
	using PATTERN_MAP = STRING_MAP;
	//using PATTERN_MAP = std::unordered_map<string, REGEX>;
	extern PATTERN_MAP NAMED_PATTERNS;
		//!! Alas, no constexpr init for dynamic containers... :-/
		//!! See init() and the Parser ctor!
		//!! Must be initialized (sigh: later..., at least to {}) to avoid
		//!! silent crashes, when creating rules before a Parser!

	//-------------------------------------------------------------------
	// Built-in meta-grammar operators (terminal rules)...

	// Opcodes... (regex-inspired)
	//
	// Can be referred to either as Parser::_SOME_OP, or as the 'literal' names.
	// Can be freely extended by users (in sync with the ::$OP map below).
	using OPCODE = int;

	CONST _NIL         = OPCODE('0');  // never matches; ignores any subsequent items in the production
	CONST _T           = OPCODE('1');  // always matches; ignores any subsequent items in the production
	CONST _ATOM        = OPCODE('#');  // Fake opcode for matching atoms (which are not opeators; only defined for a cleaner Parser::match() impl.)
	CONST _SEQ         = OPCODE(',');
	CONST _SEQ_IMPLIED = OPCODE(';');  // SEQ can be omitted; it will be implied for a list of rules that don't start with an opcode
	                                   // (This is to further unify processing: PROD rules all uniformly start with an opcode /internally/.)
	CONST _OR          = OPCODE('|');  // Expects 2 or more arguments
	                                   // - Note: adding _AND, too, would make little sense, I guess.
	                                   // Albeit... ->conjunctive grammars, or e.g.: https://stackoverflow.com/questions/2385762/how-do-i-include-a-boolean-and-within-a-regex
	                                   // (The key is "non-consuming" rules (like regex lookarounds) -- which is not yet supported by RULE directly.)
	CONST _MANY        = OPCODE('+');  // 1 or more (greedy!); expects 1 argument
	CONST _ANY         = OPCODE('*');  // 0 or more (greedy!); shortcut to [_OR [_MANY X] EMPTY]; expects 1 argument
	                                   // - Note: "greedy" above means that [A...]A will never match! Be careful!
	CONST _OPT         = OPCODE('?');  // 0 or 1; expects 1 argument
	CONST _NOT         = OPCODE('!');  // Expects 1 argument (Beware of using it with patterns!... ;) )

	// Meta-operators
	CONST _SAVE        = OPCODE('(');  // Save matched text to unnamed capture results
	CONST _SAVE_AS     = OPCODE('[');  // Save matched text to named capture results
	                                   // - Its 1st arg must be an ATOM (USER_LITERAL) for the name

	// Operator functions...
	struct RULE;
	class Parser;
	using OPERATION = std::function<bool(Parser&, size_t src_pos, const RULE&, OUT size_t& matched_len)>;

	// Operator lookup table...
	using OP_MAP = std::unordered_map<OPCODE, OPERATION>;
	extern OP_MAP OPERATORS; //!! See also NAMED_PATTERNS, why not CONST (or at least static)
		// Will be populated later, as:
		// OPERATORS[RULE::opcode] = [](Parser&, input_pos, rule&) { ... return match-length or 0; }
		// Can be freely extended by users (respecting the opcode list above).


//---------------------------------------------------------------------------
// Grammar rules...
//---------------------------------------------------------------------------
struct RULE
{
	// User Grammar rule expression, as a recursive RULE tree
	//!! Can't define this outside of RULE, sadly. But shipping with a `using RULE::PRODUCTION` can help!
	using PRODUCTION = std::vector<RULE>;

	enum {
		OP,
		PROD,
		CURATED_REGEX,   // built-in "atomic" regex pattern
		CURATED_LITERAL, // built-in "atomic" literal
		USER_REGEX,
		USER_LITERAL,
		
		// Disagnostics support (mostly for the C++ hackery)...
		// See _copy(), _move(), _destruct()!
		_MOVED_FROM_,
		_DESTROYED_,  
	} type;

#ifndef NDEBUG
	const char* _type_to_cstr(auto t) const {
		switch (t) {
		case OP: return "OP";
		case PROD: return "PROD";
		case CURATED_REGEX: return "CURATED_REGEX";
		case CURATED_LITERAL: return "CURATED_LITERAL";
		case USER_REGEX: return "USER_REGEX";
		case USER_LITERAL: return "USER_LITERAL";

		case _MOVED_FROM_: return "_MOVED_FROM_";
		case _DESTROYED_: return "_DESTROYED_";
		default:
			return "!!BUG: MISSING NAME FOR RULE TYPE!!";
		}
	}
	const char* _type_cstr() const { return _type_to_cstr(type); }
#endif

	union { //!! variant<ATOM, OPCODE, PRODUCTION> val;

		//!!Well, making it const didn't help with the mysterious double copy at "COPYLESS" creation!...
		//!! -> TC "PROD: directly from ATOM-RULE"
		const ATOM atom;    //!! Should be extended later to support optional name + regex pairs! (See d_name, currently!)
		                    //!! Or, actually, better to have patterns as non-atomic types instead (finally)?
		                    //!! Also: extend to support precompiled regexes!

		OPCODE     opcode;

		// .prod is treated specially: use prod() for (always const) access!
 		// Since RULE is in a union, we'd have to copy it no matter what,
		// because you can't have references in C++ unions. But using
		// std::reference_wrapper, it could still work...
#ifdef COPYLESS_GRAMMAR
		std::reference_wrapper<const PRODUCTION> _prod;
#else		
		/*const*/ PRODUCTION _prod; //! See the const_cast in _copy() etc. to support constness...
#endif		
	};

	string d_name; // Symbolic name, if any (e.g. for named patterns, opcodes) for diagnostics,
	               // or (a placeholder to) the "uniform string representation" of a rule


	//-----------------------------------------------------------
	// Queries...
	bool is_atom() const { return type == CURATED_REGEX || type == CURATED_LITERAL
	                           || type == USER_REGEX    || type == USER_LITERAL; }
	bool is_prod() const { return type == PROD && !prod().empty(); }
	bool is_opcode() const { return type == OP; }

	const PRODUCTION& prod() const { assert(type == PROD);

#ifdef COPYLESS_GRAMMAR
		return _prod.get();
#else		
		return _prod;
#endif		
	}


	//-----------------------------------------------------------
	// C++ boilerplate...

	//------------------------
	// Construction...
	RULE(const ATOM& atom);
	RULE(ATOM&& atom);
	RULE(const char* atom) : RULE(ATOM(atom)) {} // C++ will do all things evil with autoconversions, but not this, so... added.
	                                               // Also, this should stop the bizarra "vector too long" range
	                                               // misinterpretation errors (with arrays of 2 items), too, as a bonus!

	RULE(OPCODE opcode): type(OP), opcode(opcode), d_name({(char)opcode}) {
DBG("RULE::OPCODE-ctor creating [{}] as: {} ('{}')...", (void*)this, opcode, (char)opcode);
	}

	RULE(const PRODUCTION& expr): type(PROD), _prod(expr) { //! Constructs _prod as ref_wrap(expr) in COPYLESS_GRAMMAR mode.
DBG("RULE::PROD-copy-ctor creating [{}] from [{}] as PROD[0].type: {}...",
	(void*)this, (void*)&expr, expr.empty() ? "<!!EMPTY!!>" : expr[0]._type_cstr());
		assert(prod().size() == expr.size());
		if (prod().size()) assert(expr[0].type == prod()[0].type);

		//! See also the move-PROD ctor!
		if (prod().empty()) {
			_destruct(); // Clean up the empty PROD we've just created...
			_init_as_nil();
		}
//DBG("RULE::PROD-ctor creating [{}] done.", (void*)this);
	}


#if 0 //!!CRASHES, ACTUALLY, SO DISABLED!
#if !defined (COPYLESS_GRAMMAR)
	template<class... ArgsT>
	void append(ArgsT&&... args)
	{
		int dummy[] [[maybe_unused]] = { 0, (_prod.emplace_back(std::forward<ArgsT>(args)), 0)... };
	}
#endif
#endif
	//------------------------
	// Copy(-construction)...
	RULE(const RULE& other): type(_DESTROYED_) {
DBG("RULE::copy-ctor creating [{}] from [{}] as type: {}...",
	(void*)this, (void*)&other, other._type_cstr());
		_copy(other); //! Works with COPYLESS_GRAMMAR transparently, because
		              //! it's a reference_wrapper, which can be rebound!
//DBG("RULE::copy-ctor creating [{}] from type: {} done.", (void*)this, (int)type);
	}
	// Handles COPYLESS_GRAMMAR transparently
	RULE& operator=(const RULE& other) {
		DBG("RULE assigmnet invoked... Could it be spared?");
		assert(type != _DESTROYED_);
		if (&other != this) {
			_destruct();
			_copy(other);
		} 
		assert(type != _DESTROYED_);
		return *this; 
	}

	//------------------------
	// Move...
#ifdef COPYLESS_GRAMMAR
 	// Refuse to take references of temporaries!
	//!! Could be re-enabled, doing copy, with a "smart-destruct" flag
	//!! to know that `*this` is now a copy that can be destructed, plus
	//!! also keeping a whole "spare" value-type _prod ready for this case...
	//!! IOW: GRAND PITA!...
	RULE(RULE&& tmp) = delete;
	RULE(PRODUCTION&& expr) = delete;
	RULE& operator=(RULE&& tmp) = delete;
#else
	RULE(RULE&& tmp) noexcept : type(_DESTROYED_) {
DBG("RULE::move-ctor creating [{}] from: [{}]...", (void*)this, (void*)&tmp);
		_move(std::move(tmp));
	}
	RULE(PRODUCTION&& expr) noexcept : type(PROD), _prod(std::move(expr)) { //!!?? Why is move() still needed here?! :-o
DBG("RULE::PROD-move-ctor created [{}] from PROD[0].type: {}...", (void*)this, prod().empty() ? "<!!EMPTY!!>" : prod()[0]._type_cstr());

		//! See also the copy-PROD ctor!
		if (prod().empty()) {
			_destruct(); // Clean up the empty PROD we've just created...
			_init_as_nil();
		}
	}

	RULE& operator=(RULE&& tmp) noexcept {
		DBG("RULE (move-)assigmnet invoked... Could it be spared?");
		assert(type != _DESTROYED_);
		assert (&tmp != this); //!!?? Why is move() still needed here?! :-o
		_destruct();
		_move(std::move(tmp));
		assert(type != _DESTROYED_);
		return *this; 
	}
#endif
	//------------------------
	// Destruction...
	~RULE() {
DBG("~RULE destructing [{}] (type: {})...", (void*)this, _type_cstr());
		_destruct();
	}

private:
	//-----------------------------------------------------------
	// Construction/destruction/copy/move helpers...

	void _init_as_nil() {
DBG("- Setting up empty rule...");
		assert(type == _DESTROYED_); // Must be called from a ctor(-like context)

#ifdef COPYLESS_GRAMMAR
//! Hehh, GCC has a (bogus) warning even for #error (missing terminating '), so I can't write "can't" there... :-o :)
#  error Sorry, empty COPYLESS_GRAMMAR cannot properly replace the entrails of an empty rule...
#else
		new (const_cast<PRODUCTION*>(&_prod)) PRODUCTION(); //!! Call the ctor manually!... :-o
		_prod.emplace_back(_NIL);
#endif
		type = PROD;
		d_name = "EMPTY";
	}

	void _init_atom(auto&& s);

	void _destruct() {
//DBG("RULE::destruc (type: {})...", _type_cstr()); //DUMP();
		assert (type != _DESTROYED_);
		if      (is_atom()) atom.~string();
#if !defined(COPYLESS_GRAMMAR)
		else if (type == PROD) _prod.~PRODUCTION(); //! Can't use is_prod() here: it's false if empty()!
#endif
		type = _DESTROYED_;
//DBG("RULE::destruct (type: {}) done.", _type_cstr());
	}

	void _copy(const RULE& other) {
//DBG("RULE::_copy...");
		//!! Assumes not being constructed already!
		assert(type == _DESTROYED_);
		assert(other.type != _DESTROYED_);
		assert(other.type != _MOVED_FROM_);
		type = other.type;
		d_name = other.d_name;
		if      (is_atom()) new (const_cast<ATOM*>(&atom)) ATOM(other.atom);
#ifdef COPYLESS_GRAMMAR
		else if (type == PROD) new (&_prod) decltype(_prod)(other._prod); //! Copying ref_wrap will only bind other's!
#else
		else if (type == PROD) new (const_cast<PRODUCTION*>(&_prod)) PRODUCTION(other.prod()); //! Can't use is_prod() here: it's false if empty()!
#endif
		else                opcode = other.opcode; // just a number...
DBG("RULE::_copy (type == {}) done.", _type_cstr());
	}

	void _move(RULE&& tmp) {
#ifdef COPYLESS_GRAMMAR
		ERROR("BUG? RULE::_move called in COPYLESS_GRAMMAR mode!"); // See below...
#endif
//DBG("RULE::_move...");
		//!! Assumes not being constructed already!
		assert(type == _DESTROYED_);
		assert(tmp.type != _DESTROYED_);
		assert(tmp.type != _MOVED_FROM_);
		type = tmp.type;
		d_name = tmp.d_name;
		if      (is_atom()) new (const_cast<ATOM*>(&atom)) ATOM(std::move(tmp.atom));
#ifdef COPYLESS_GRAMMAR
		else if (type == PROD) new (&_prod) decltype(_prod)(std::move(tmp._prod)); //!!?? Will this do what I hope?
		                                                                           //!! I don't think so!...
#else
		else if (type == PROD) new (const_cast<PRODUCTION*>(&_prod)) PRODUCTION(std::move(tmp.prod())); //! Can't use is_prod() here: it's false if empty()!
#endif
		else                opcode = std::move(tmp.opcode); // just a number...
		tmp.type = _MOVED_FROM_;
DBG("RULE::_move (type == {}) done.", _type_cstr());
	}

	//-----------------------------------------------------------
	// Diagnostics...
	void _dump(unsigned level = 0) const {
/*!!
		template <typename ... Types> auto f(Types&& ... args) {
			f(x, std::forward<Types>(args)...);
		}
		template <typename ... Types> auto f(Types&& ... args) {
			return [... args = std::forward<Types>(args)] {
				// use args
			};
		}
		auto s = [&](auto x, auto... args) { string prefix(level * 2, ' ');
			return format("     {}{}", prefix, x); //!! Ignores all the opt. args yet! :-/
			};
		auto p_ = [&](auto x, auto... args) { cerr << s(x, ...args);
			};
		auto p = [&](auto x, auto... args) { p_(x, ...args); };
!!*/
		//!! No multiple args & forwarding yet! :-/
		auto p   [[maybe_unused]] = [&](auto x, auto... args) { string prefix(level * 2, ' ');
			   cerr << "     " << prefix << x << endl; };
		auto p_  [[maybe_unused]] = [&](auto x, auto... args) { string prefix(level * 2, ' ');
			   cerr << "     " << prefix << x; };
		auto _p_ [[maybe_unused]] = [&](auto x, auto... args) {cerr << x; };
		auto _p  [[maybe_unused]] = [&](auto x, auto... args) {cerr << x << endl; };

		if (!level) p("/------------------------------------------------------------------\\");
		if (d_name.empty()) p_(format("[{}] {} (type #{}):",      (void*)this, _type_cstr(), (int)type));
		else                p_(format("[{}] {} (type #{}) '{}':", (void*)this, _type_cstr(), (int)type, d_name));
		if (type == _DESTROYED_)  p(" !!! INVALID (DESTROYED) OBJECT !!!");
		if (type == _MOVED_FROM_) p(" !!! INVALID (MOVED-FROM) OBJECT !!!");
		if (is_atom()) { _p(format(" \"{}\"", atom));
		} else if (type == PROD) { //! Can't use is_prod() here: it's false if empty()!
			_p(""); p("{"); // New line for the {
			for (auto& r : prod()) { r._dump(level + 1); }
			p("}");
		} else if (type == OP) { _p(format(" opcode = {} ('{}')", opcode, char(opcode)));
		} else if (type == _DESTROYED_) { p("!!! _DESTROYED_ !!!");
		} else p("*** UNKNOWN/INVALID RULE TYPE! ***");
		if (!level) p("\\------------------------------------------------------------------/\n");
	}
#ifndef NDEBUG
	public: void DUMP() const { _dump(); }
#else
	public: void DUMP() const {}
#endif
};


//---------------------------------------------------------------------------
class Parser
//---------------------------------------------------------------------------
{
public:
	//-------------------------------------------------------------------
	// Parser state...

	// Input:
#ifdef COPYLESS_GRAMMAR
	const RULE& syntax;
#else
	const RULE syntax;
#endif
	string text;
	size_t text_length;

	// Results of capture ops.; valid only after a successful parse():
	STRING_MAP  named_captures; //!! My initial guess is that SSO makes it pretty much useless to keep string_views here.
	std::map<size_t, string> unnamed_captures; // Ordered map! Now we only have to make sure that its order kinda makes sense! :)

	// Diagnostics:
	int loopguard;
	int depth_reached;
	int rules_tried;
	int terminals_tried;

	CONST DEFAULT_RECURSION_LIMIT = 500;

	void _reset_counters()
	{
		loopguard = DEFAULT_RECURSION_LIMIT;
		depth_reached = DEFAULT_RECURSION_LIMIT;
		rules_tried = 0;
		terminals_tried = 0;
	}

	void _reset_results()
	{
		_reset_counters();
		named_captures = {};
		unnamed_captures = {};
	}

	// Must be called before (i.e. by) each parse()!
	void _reset()
	{
		_reset_results();
	}

	void _set_text(const string& txt)
	{
		_reset(); // First this, to ensure `captures` or any other saved results (like
		          // error messages etc.) can never refer to removed text, esp. if
		          // they store string_views or positions etc. instead of copies!
		text = txt;
		text_length = txt.length();
	}


	//-------------------------------------------------------------------
	Parser(const RULE& syntax, int maxnest = DEFAULT_RECURSION_LIMIT):
		// Sync with _reset*()!
		syntax(syntax),
		loopguard(maxnest),
		depth_reached(maxnest),
		rules_tried(0),
		terminals_tried(0)
	{
		Parsing::init(); //!! Legacy init-once location -- but makes no sense here; now done in RULE()!
	}

	Parser(const Parser& other) = delete;
	Parser& operator=(const Parser& other) = delete;
	Parser(Parser&&) = delete;

	//-------------------------------------------------------------------
	// Convenience front-ends to match(...)
	bool parse(const string& txt)
	{
		_set_text(txt);
		size_t matched_length_ignored;
		return match(0, syntax, matched_length_ignored);
	}
	bool parse(const string& txt, OUT size_t& matched_length)
	{
		_set_text(txt);
		return match(0, syntax, matched_length);
	}

	//!! Move these to a `results` (or directly to `captures` or `saves`) objects
	//!! instead, for more (versatile) queries like unnamed_captures() or saves.count()
	//!! etc. to begin with!
	const string& operator[](const string& name) const;
	const string& operator[](size_t index_of_unnamed) const;

	//-------------------------------------------------------------------
	bool match(size_t pos, const RULE& rule, OUT size_t& len)
	// pos is the source position
	// rule is a syntax rule (tree node)
	// If matches, returns the length of the matched input, otherwise 0.
	//-------------------------------------------------------------------
	{
DBG("Parser::match()");
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
			assert(!OPERATORS.empty());
			f = OPERATORS[_ATOM];
			//!! Should be dispatched further across the various atom types, instead:
			//!!f = atom_handler(rule);
		}
		else if (rule.is_prod())
		{
			f = prod_handler(rule); // First item is the op. of the rule, or else _SEQ is implied:
		}
		else if (rule.is_opcode())
		{
			ERROR("Invalid grammar at rule '{}': OPCODE outside of PRODUCTION", rule.d_name);
		}
		else
		{
			ERROR("Invalid grammar at rule '{}'", rule.d_name);
		}

		auto res = f(*this, pos, rule, len); //! Remember: `len` is OUT!

		++loopguard;

		return res;
	}

private:
	const OPERATION& prod_handler(const RULE& rule) const
	{
DBG("OPERATORS in prod_handler: {}", (void*)&OPERATORS); // Remnant from hunting as accidental shadow copies of it...
//DBG("OPERATORS.size in prod_handler: {}", OPERATORS.size());
		assert(!OPERATORS.empty());
		assert(rule.type == RULE::PROD); //! Shouldn't be asking any other types (not even an opcode-type RULE object directly)
		assert(!rule.prod().empty());

		OPCODE opcode;

 		// First item of a "compound" RULE is the op., or else _SEQ is implied.
 		if (!rule.prod()[0].is_opcode()) { // Not an opcode, imply a sequence
			opcode = _SEQ_IMPLIED; //!! Will expect a headless rule!
				//!!OPTIM: Shouldn't even be a lookup, just return the handler directly!
		} else {
			opcode = rule.prod()[0].opcode;
		}
			
		if (auto it = OPERATORS.find(opcode); it != OPERATORS.end()) {
			decltype(OPERATORS.cbegin()) cit = it; //!!?? better one-liner for iter -> const-iter?
			return cit->second;
		} else {
			ERROR("Unimplemented opcode: {} ('{}')", opcode, (char)opcode);
		}
	}
};


//===========================================================================
inline RULE::RULE(const ATOM& s) {
DBG("RULE::ATOM-copy-ctor creating [{}] from \"{}\"...", (void*)this, s);
	_init_atom(s);
}

inline RULE::RULE(ATOM&& s) {
DBG("RULE::ATOM-move-ctor creating [{}] from \"{}\"...", (void*)this, s);
	_init_atom(s);
}

inline void RULE::_init_atom(auto&& s)
// A `string` arg. can mean:
//   a) symbol: the name of a curated item (either regex or literal)
//   b) direct ("user") string literal
//   c) direct ("user") regex
// For efficiency, the actual type (`type`) and it's "actual value" (e.g. the
// regex of a named pattern) is resolved and recorded (cached) here.
{
DBG("RULE::_init_atom from: \"{}\"...", s);

	// Sneak in the once-only implicit init here...
	Parsing::init();

	type = _DESTROYED_; // Let's not burden the calling *empty* ctors with this...
	// ...But in case I may still move it there, and add some other ctors,
	// but forget to add it to those, here's this assert, too:
	assert(type == _DESTROYED_);

	if (s.empty()) { _init_as_nil(); return; }

	d_name = s; // Save it as name for diagnostics (even though it's the same as it's value for literals)

	auto set_type_and_adjust_regex = [&](string_view pattern, decltype(type) TYPE_if_literal,
	                                                          decltype(type) TYPE_if_regex) {
		// If "/.../" then it's a regex, so unwrap & mark it as such:
		if (pattern.length() >= 2 && pattern[0] == '/' && pattern[pattern.length()-1] == '/')
		{
			type = TYPE_if_regex;
			pattern = pattern.substr(1, pattern.length() - 2);
		}
		else type = TYPE_if_literal;
		return pattern;
	};

	if (auto it = NAMED_PATTERNS.find(s); it != NAMED_PATTERNS.end())
	{
		auto pattern = set_type_and_adjust_regex(it->second, CURATED_LITERAL, CURATED_REGEX);
		new (const_cast<ATOM*>(&atom)) ATOM(pattern); // Replace the atom name with the actual pattern (that's what that lame `second` is)

DBG("RULE initialized as named pattern '{}' ('{}') (type: {})", d_name, atom, _type_cstr());
	} else {
		auto pattern = set_type_and_adjust_regex(s, USER_LITERAL, USER_REGEX);
		new (const_cast<ATOM*>(&atom)) ATOM(pattern);

DBG("RULE initialized as string literal '{}' (type: {}).", atom, _type_cstr());
	}

	if (type == CURATED_REGEX || type == USER_REGEX) {
		//!! atom = ...; // compile it!
	}

	assert(is_atom());
	// Let's just also check if it's still doing what it was paid for... ;)
	assert(type == CURATED_REGEX || type == CURATED_LITERAL
	    || type == USER_REGEX    || type == USER_LITERAL);
}


/*!!
OPERATION RULE::op(OPCODE code) const
{
	assert(type == PROD); //! Never asking the opcode directly! :)
	auto   it  = OPERATORS.find(code);
	return it != OPERATORS.end() ? *it : false; //!!?? OPERATORS[NIL] // -- but that can't be (!op)'ed... :-/
}
!!*/


	// Simple painkillers for grammar-building:
	using PROD = RULE::PRODUCTION;
	using _    = RULE::PRODUCTION; // Even this! ;) For init. lists like RULE r = _{ ... _{...} }
	                               // But this isn't OK for declaring PROD vars, so keeping both.

} // namespace


//
//--------------------------------------------<< C U T  H E R E ! >>--------------------------------------------
//


#ifndef PARSERTOY_DEDUP
//===========================================================================
namespace Parsing {

	PATTERN_MAP NAMED_PATTERNS = {};
	OP_MAP OPERATORS = {};

void init()
{
	static auto initialized = false;
	if (initialized) return;

	//-------------------------------------------------------------------
	// Initialize the predefined "atomic" patterns
	//-------------------------------------------------------------------
	// "Curated atoms" (named terminal pattens) are "metasyntactic sugar" only,
	// as they could as well be just literal patterns. But fancy random regex
	// literals could confuse the parsing, so these "officially" nicely behaving
	// ones are just named & groomed here.
	// (BTW, a user pattern that's not anchored to the left is guaranteed to 
	// fail, as the relevant regex_search() call doesn't anchor it itself!)
	//
#define PATTERN(name, rx) {name, "/^" rx "/"} // ".*" added to allow using std::regex_match for left-anchored partial matching!
//#define PATTERN(name, rx) {name, REGEX(rx, std::regex::extended)}
	assert(NAMED_PATTERNS.empty());
	NAMED_PATTERNS = { //!! Alas, no constexpr init for dynamic containers; have to do it here...
		PATTERN( "_EMPTY"      , "" ),
		PATTERN( "_SPACE"      , " " ), // No \s, and [\s] didn't match ' ' in POSIX2 "extended") for some reason! :-o
		PATTERN( "_TAB"        , "\t" ), // No \t or [\t] (at least in POSIX2 "extended"?)
		PATTERN( "_QUOTE"      , "\"" ), // Not a special char
		PATTERN( "_APOSTROPHE" , "'" ),
		PATTERN( "_SLASH"      , "/" ),
		PATTERN( "_BACKSLASH"  , "\\\\" ),
		PATTERN( "_IDCHAR"     , "[[:alpha:][:alnum:]_]" ),
		PATTERN( "_ID"         , "([[:alpha:]_])([[:alnum:]_])*" ),
		PATTERN( "_DIGIT"      , "[[:digit:]]]" ),
		PATTERN( "_DIGITS"     , "[[:digit:]]+" ),
		PATTERN( "_HEXDIGIT"   , "[[:xdigit:]]" ),
		PATTERN( "_HEXDIGITS"  , "[[:xdigit:]]+" ),
		PATTERN( "_LETTER"     , "[[:alpha:]]" ),
		PATTERN( "_LETTERS"    , "[[:alpha:]]+" ),
		PATTERN( "_ALNUM"      , "[[:alnum:]]" ),
		PATTERN( "_ALNUMS"     , "[[:alnum:]]+" ),
		PATTERN( "_WHITESPACE" , "[[:space:]]" ),
		PATTERN( "_WHITESPACES", "[[:space:]]+" ),
	};
#undef PATTERN

	//-------------------------------------------------------------------
	// Initialize the operation map
	//-------------------------------------------------------------------

	assert(OPERATORS.empty());
	//-------------------------------------------------------------------
	OPERATORS[_NIL] = [](Parser&, size_t, const RULE&, OUT size_t&) -> bool
	{
DBG("NIL: no op. (returning false)");
		return false;
	};

	//-------------------------------------------------------------------
	OPERATORS[_T] = [](Parser&, size_t, const RULE&, OUT size_t&) -> bool
	{
DBG("T: 'true' op. (returning true)");
		return true;
	};

	//-------------------------------------------------------------------
	OPERATORS[_ATOM] = [](Parser& p, size_t pos, const RULE& rule, OUT size_t& len) -> bool
	{
		assert(rule.is_atom());
		static_assert(std::is_same<ATOM, string>::value);
		string atom = rule.atom;

		++p.terminals_tried;

		if (rule.type == RULE::CURATED_REGEX || rule.type == RULE::USER_REGEX)
		{
			try {
//!!?? ^[[:word:]] is not defined?!
//				REGEX regx(atom); //!!PRECOMPILE!...
				REGEX regx(atom, std::regex::extended); //!!PRECOMPILE!...
//!!?? C++			REGEX regx(atom, std::regex::extended | std::regex_constants::multiline ); //!!PRECOMPILE!...
				std::smatch m;
	//!!?? WTF, C++?	if (std::regex_search(string_view(p.text).substr(pos), m, regx)
	//!!?? WTF, C++?	if (std::regex_search(p.text.begin(), p.text.begin() + pos, m, regx)
	//!!?? WTF, C++?	if (std::regex_search(p.text.substr(pos), m, regx)
	//!!?? WTF, C++?	string_view target = string_view(p.text).substr(pos);
	//OK:			string target      = p.text.substr(pos);
	//			if (std::regex_search(target, m, regx))

				if (std::regex_search(p.text.cbegin() + ptrdiff_t(pos), p.text.cend(), m, regx))
//				if ( std::regex_match(p.text.cbegin() + ptrdiff_t(pos), p.text.cend(), m, regx))
					// NOTE: regex_match would enforce a complete (^...$-anchored)
					// match, so can't be used to eat *some* of the text.
					// OTOH, regex_search matches *anywhere*, so then the
					// position of the result must be checked explicitly!
					//
					// But this also means that regex_search won't stop
					// when failing to match at the start, and will go
					// ahead happily playing with all the possible combinations
					// in the entire string! Which is basically unacceptable
					// for larger texts and/or complex grammars...
					//
					// So, still using regex_match with a wildcard suffix
					// of .* would then seem to look viable after all,
					//!![-- But not here! When precompiling!... :) ]
					// but then again, there would be a misunderstanding
					// between it and us about what a "real match" is,
					// and it would always return the full text length for
					// everything we consider a partial match...
					//
					// OK, let me double-check... maybe regex_search does
					// support ^ after all... Phew, OK, it does! :)
				{
//!! MAAAN, C++... Just can't pass DBG_TRIM to format(), as it returns a temporary.
//!! Have to actually create a var for that. :-/
{ auto src = DBG_TRIM(p.text.c_str() + pos);
DBG("REGEX \"{}\": MATCHED \"{}\" with length {}.", atom, src, m.length()); }
/* OK, since regex_search does support ^, this is no longer required:
DBG_("REGEX \"{}\": MATCHED '{}' with length {}...", atom, p.text.substr(pos), m.length());
					if (size_t(m.position()) == 0)
					{
_DBG(" at the start: ACCEPTED!");
						len = size_t(m.length());
						return true;
					}
_DBG(" in the middle -- REJECTED.");
*/
					assert(m.position() == 0); // User patterns are not as well
					                           // prepared as the curated ones,
					                           // and may forget to left-anchor!

					len = size_t(m.length());
					return true;
				}
				else {
DBG("REGEX \"{}\": ---NOT--- MATCHED '{}'!", atom, p.text.substr(pos));
				}
			}
			catch(std::exception& x)
			{
				DBG("OP[_ATOM]: FAILED REGEX \"{}\": ({})", atom, x.what());
			}
			return false;
		}
		else // non-regex literal
		{
			len = atom.length();
			//! Case-insensitivity=true will fail for UNICODE chars!
			//! So we just go case-sensitive. All the $ATOMs are like that anyway, and
			//! the user still has control to change it, but won't over a failing match...
			if (p.text_length - pos >= len //! needed to silence a PHP warning...
				&& string_view(p.text).substr(pos, len) == atom)  {
				return true;
			} else	return false;
		}
	};

	//-------------------------------------------------------------------
	OPERATORS[_SEQ] = [](Parser& p, size_t pos, const RULE& rule, OUT size_t& len) -> bool
	{
		assert(rule.is_prod());
		assert(rule.prod().size() >= 2);

		len = 0;

		for (auto r = rule.prod().cbegin() + 1; r != rule.prod().cend(); ++r)
		{
			size_t len_add;
			if (!p.match(pos + len, *r, len_add)) return false;
			else len += len_add;
		}
		return true;
	};

	//-------------------------------------------------------------------
	OPERATORS[_SEQ_IMPLIED] = [](Parser& p, size_t pos, const RULE& rule, OUT size_t& len) -> bool
	{
		assert(rule.is_prod());
		assert(rule.prod().size() >= 1);

		len = 0;
		for (auto r = rule.prod().cbegin(); !(r == rule.prod().cend()); ++r)
		{
			size_t len_add;
			if (!p.match(pos + len, *r, len_add)) return false;
			else len += len_add;
		}
		return true;
	};

	//-------------------------------------------------------------------
	OPERATORS[_OR]  = [](Parser& p, size_t pos, const RULE& rule, OUT size_t& len) -> bool
	{
		assert(rule.prod().size() >= 3);

		for (auto r = rule.prod().cbegin() + 1; r != rule.prod().cend(); ++r)
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
	OPERATORS[_OPT] = [](Parser& p, size_t pos, const RULE& rule, OUT size_t& len) -> bool
	{
		assert(rule.prod().size() == 2);

		if (!p.match(pos, rule.prod()[1], len)) {
			len = 0;
		}
		return true;
	};

	//-------------------------------------------------------------------
	OPERATORS[_ANY] = [](Parser& p, size_t pos, const RULE& rule, OUT size_t& len) -> bool
	{
		assert(rule.prod().size() == 2);

		len = 0;
		auto const& r = rule.prod()[1];
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
	OPERATORS[_MANY] = [](Parser& p, size_t pos, const RULE& rule, OUT size_t& len) -> bool
	{
		assert(rule.prod().size() == 2);

		len = 0;
		auto const& r = rule.prod()[1];
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
	OPERATORS[_NOT] = [](Parser& p, size_t pos, const RULE& rule, OUT size_t& len) -> bool
	{
		assert(rule.prod().size() == 2);

		if (p.match(pos, rule.prod()[1], len)) {
			return false;
		} else {
			return true;
		}
	};

	//---------------------------------------------------------------------------
	OPERATORS[_SAVE] = [](Parser& p, size_t pos, const RULE& rule, OUT size_t& len) -> bool
	{
		assert(rule.prod().size() >= 2);

		// Shift off the _SAVE prefix...
		//!! ...which, alas, currently means full cloning... :-/
		RULE target_rule(PROD(rule.prod().cbegin() + 1, rule.prod().cend()));

		if (p.match(pos, target_rule, len)) {
			auto snapshot = string_view(p.text).substr(pos, len);
DBG("\n\n    SNAPSHOT: [{}]\n\n", snapshot);
			p.unnamed_captures[(size_t)(void*)&rule] = snapshot;
			return true;
		}
		return false;
	};

	OPERATORS[_SAVE_AS] = [](Parser& p, size_t pos, const RULE& rule, OUT size_t& len) -> bool
	{
		assert(rule.prod().size() >= 3);
		assert(rule.prod()[1].is_atom());

		const string& name = rule.prod()[1].atom;

		// Shift off the _SAVE prefix + the name...
		//!! ...which, alas, currently means full cloning... :-/
		RULE target_rule(PROD(rule.prod().cbegin() + 2, rule.prod().cend()));

		if (p.match(pos, target_rule, len)) {
			auto snapshot = string_view(p.text).substr(pos, len);
DBG("\n\n    SNAPSHOT[{}]: \"{}\"\n\n", name, snapshot);
			p.named_captures[name] = snapshot;
			return true;
		}
		return false;
	};


	assert(!NAMED_PATTERNS.empty());
	assert(!OPERATORS.empty());
	initialized = true;
DBG("+++ Static init done. +++");
} // init()


const string& Parser::operator[](const string& name) const
{
	try { return named_captures.at(name); } // Not [] to avoid messing up .size()
	catch(...) { return EMPTY_STRING; }
}

const string& Parser::operator[](size_t index_of_unnamed) const
{
	try { return unnamed_captures.at(index_of_unnamed); } // Not [] to avoid messing up .size()
	catch(...) { return EMPTY_STRING; }
}

} // namespace Parsing

#endif // PARSERTOY_DEDUP

#endif // _PARSERTOY_HPP_
