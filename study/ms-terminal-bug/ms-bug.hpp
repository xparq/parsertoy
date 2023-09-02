#include <format>
	using std::format;
#include <string>
	using std::string;
	using namespace std::literals::string_literals;
#include <string_view>
	using std::string_view;
#include <vector>
#include <unordered_map>
#include <iostream>
	using std::cerr, std::cout, std::endl;


#define DBG(msg, ...) std::cerr << std::format("DBG> {}", std::format(msg __VA_OPT__(,) __VA_ARGS__)) << std::endl

//---------------------------------------------------------------------------
namespace Parsing
{
	using OPCODE = int;

	auto _NIL         = OPCODE('0');  // NOOP
	auto _ATOM        = OPCODE('#');  // Fake "opcode" for atoms, which are not opeators; only defined for a cleaner Parser::match() impl.

	//---------------------------------------------------------------------------
	struct RULE
	{
		using PRODUCTION = std::vector<RULE>;

		enum {
			NIL,
			CURATED_REGEX,   // built-in "atomic" regex pattern
			CURATED_LITERAL, // built-in "atomic" literal
			USER_REGEX,
			USER_LITERAL,
			OP,
			PROD,
			
			// Disagnostics support (mostly for the C++ hackery)...
			// See _copy(), _move(), _destruct()!
			_MOVED_FROM_,
			_DESTROYED_,  
		} type;

		struct {
			const string atom;
			OPCODE     opcode;
			PRODUCTION prod;
		};

		//-----------------------------------------------------------
		bool is_atom() const { return type == CURATED_REGEX || type == CURATED_LITERAL
						|| type == USER_LITERAL || type == USER_REGEX; }

		//-----------------------------------------------------------
		// Construction...
		RULE(const string& atom);
		RULE(string&& atom);
		RULE(const char* atom) : RULE(string(atom)) {}

		RULE(OPCODE opcode): type(OP), opcode(opcode) {
	DBG("RULE::OPCODE-ctor");
		}

		RULE(const PRODUCTION& expr): type(PROD), prod(expr) {
	DBG("RULE::PROD-ctor");
		}

		// Copy(-construction)...
		RULE(const RULE& other): type(_DESTROYED_) {
	DBG("copy begin");
			type = other.type;
			if (type == PROD) prod = PRODUCTION(other.prod);
	DBG("copy end");
		}

		~RULE() {
	DBG("~RULE");
		}

		//-----------------------------------------------------------
		public: void DUMP(unsigned level = 0) const {
			auto p = [&](auto x, auto... args) { string prefix(level * 2, ' ');
				cerr << "     " << prefix << x << endl;
				};
			if (!level) p("/--\\");
			p(format("---"));
			if (is_atom()) {
			} else if (type == PROD) {
				p("{");
				for (auto& r : prod) { r.DUMP(level + 1); }
				p("}");
			} else p("***");
			if (!level) p("\\--/\n");
		}
	};

	struct Parser
	{
		using PATTERN_MAP = std::unordered_map<string, string>;
		RULE syntax;
		Parser(const RULE& syntax): syntax(syntax)
		{
			NAMED_PATTERN = { { "_EMPTY", "//" } };
		}
	
	static PATTERN_MAP NAMED_PATTERN;
	};
	Parser::PATTERN_MAP Parser::NAMED_PATTERN;

//===========================================================================
	RULE::RULE(string&& s)
	{
	DBG("1");
	DBG("2");
		if (auto it = Parser::NAMED_PATTERN.find(s); it != Parser::NAMED_PATTERN.end()) {
	DBG("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
		} else {
	DBG("???");
		}
	}
} // namespace
