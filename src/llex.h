/*
** $Id: llex.h,v 1.78 2014/10/29 15:38:24 roberto Exp $
** Lexical Analyzer
** See Copyright Notice in lua.h
*/

#ifndef llex_h
#define llex_h

#include "lobject.h"
#include "lzio.h"
#include "lopcodes.h"


#define FIRST_RESERVED	257


#if !defined(LUA_ENV)
#define LUA_ENV		"_ENV"
#endif

#define USE_CHINESE_CODE
#define IS_CHINESE_CODE(charint) (charint > 0x80)

/*
* WARNING: if you change the order of this enumeration,
* grep "ORDER RESERVED"
*/
enum RESERVED {
	/* terminal symbols denoted by reserved words */
	TK_AND = FIRST_RESERVED, TK_BREAK,
	TK_DO, TK_ELSE, TK_ELSEIF, TK_END, TK_FALSE, TK_FOR, TK_FUNCTION,
	TK_GOTO, TK_IF, TK_IN, TK_LOCAL, TK_NIL, TK_NOT, TK_OR, TK_REPEAT,
	TK_RETURN, TK_THEN, TK_TRUE, TK_UNTIL, TK_WHILE,
	/* other terminal symbols */
	TK_IDIV, TK_CONCAT, TK_DOTS, TK_EQ, TK_GE, TK_LE, TK_NE,
	TK_SHL, TK_SHR,
	TK_DBCOLON, TK_EOS,
	TK_FLT, TK_INT, TK_NAME, TK_STRING
};

/* number of reserved words */
#define NUM_RESERVED	(cast(int, TK_WHILE-FIRST_RESERVED+1))


typedef union {
	lua_Number r;
	lua_Integer i;
	TString *ts;
} SemInfo;  /* semantics information */

class Token {
//typedef struct Token {
public:
	int token;
	SemInfo seminfo;
} ;//Token;

class expdesc;
class Labeldesc;
class Labellist;
class FuncState;
class Dyndata;
class BlockCnt;

class LHS_assign;
/* state of the lexer plus state of the parser when shared by all
	 functions */
class LexState {
//typedef struct LexState {
public:
	int current;  /* current character (charint) */
	int linenumber;  /* input line counter */
	int lastline;  /* line of last token 'consumed' */
	Token t;  /* current token */
	Token lookahead;  /* look ahead token */
	/*struct*/ FuncState *fs;  /* current function (parser) */
	/*struct*/ lua_State *L;
	ZIO *z;  /* input stream */
	Mbuffer *buff;  /* buffer for tokens */
	Table *h;  /* to avoid collection/reuse strings */
	/*struct*/ Dyndata *dyd;  /* dynamic structures used by the parser */
	TString *source;  /* current source name */
	TString *envn;  /* environment variable name */
	char decpoint;  /* locale decimal point */


/*LUAI_FUNC*/ TString *luaX_newstring (/*LexState *ls,*/ const char *str, size_t l);
/*LUAI_FUNC*/ void luaX_next (/*LexState *ls*/);
/*LUAI_FUNC*/ int luaX_lookahead (/*LexState *ls*/);
/*LUAI_FUNC*/ l_noret luaX_syntaxerror (/*LexState *ls,*/ const char *s);
/*LUAI_FUNC*/ const char *luaX_token2str (/*LexState *ls,*/ int token);


 void statement (/*LexState *ls*/);
 void expr (/*LexState *ls,*/ expdesc *v);
 l_noret semerror (/*LexState *ls,*/ const char *msg);
 l_noret error_expected (/*LexState *ls,*/ int token);
 int testnext (/*LexState *ls,*/ int c);
 void check (/*LexState *ls,*/ int c);
 void checknext (/*LexState *ls,*/ int c);

l_noret check_condition(bool c , const char * msg);

 void check_match (/*LexState *ls,*/ int what, int who, int where);
 TString *str_checkname (/*LexState *ls*/);
 void codestring (/*LexState *ls,*/ expdesc *e, TString *s);
 void checkname (/*LexState *ls,*/ expdesc *e);
 int registerlocalvar (/*LexState *ls,*/ TString *varname);
 void new_localvar (/*LexState *ls,*/ TString *name);
 void new_localvarliteral_ (/*LexState *ls,*/ const char *name, size_t sz);



 void adjustlocalvars (/*LexState *ls,*/ int nvars);
 void singlevar (/*LexState *ls,*/ expdesc *var);
 void adjust_assign (/*LexState *ls,*/ int nvars, int nexps, expdesc *e);
 void enterlevel (/*LexState *ls*/);

void leavelevel();

 void closegoto (/*LexState *ls,*/ int g, Labeldesc *label);
 int findlabel (/*LexState *ls,*/ int g);
 int newlabelentry (/*LexState *ls,*/ Labellist *l, TString *name, int line, int pc);
 void findgotos (/*LexState *ls,*/ Labeldesc *lb);
 void breaklabel (/*LexState *ls*/);
 l_noret undefgoto (/*LexState *ls,*/ Labeldesc *gt);
 Proto *addprototype (/*LexState *ls*/);
 void codeclosure (/*LexState *ls,*/ expdesc *v);
 void open_func (/*LexState *ls,*/ FuncState *fs, BlockCnt *bl);
 void close_func (/*LexState *ls*/);
 int block_follow (/*LexState *ls,*/ int withuntil);
 void statlist (/*LexState *ls*/);
 void fieldsel (/*LexState *ls,*/ expdesc *v);
 void yindex (/*LexState *ls,*/ expdesc *v);
 void recfield (/*LexState *ls,*/ struct ConsControl *cc);
 void listfield (/*LexState *ls,*/ struct ConsControl *cc);
 void field (/*LexState *ls,*/ struct ConsControl *cc);
 void constructor (/*LexState *ls,*/ expdesc *t);
 void parlist (/*LexState *ls*/);
 void body (/*LexState *ls,*/ expdesc *e, int ismethod, int line);
 int explist (/*LexState *ls,*/ expdesc *v);
 void funcargs (/*LexState *ls,*/ expdesc *f, int line);
 void primaryexp (/*LexState *ls,*/ expdesc *v);
 void suffixedexp (/*LexState *ls,*/ expdesc *v);
 void simpleexp (/*LexState *ls,*/ expdesc *v);
 BinOpr subexpr (/*LexState *ls,*/ expdesc *v, int limit);
 //void expr (LexState *ls, expdesc *v);
 void block (/*LexState *ls*/);

 void check_conflict (/*LexState *ls,*/ /*struct*/ LHS_assign *lh, expdesc *v);
 void assignment (/*LexState *ls,*/ /*struct*/ LHS_assign *lh, int nvars);
 int cond (/*LexState *ls*/);
 void gotostat (/*LexState *ls,*/ int pc);
 void skipnoopstat (/*LexState *ls*/);
 void labelstat (/*LexState *ls,*/ TString *label, int line);
 void whilestat (/*LexState *ls,*/ int line);
 void repeatstat (/*LexState *ls,*/ int line);
 int exp1 (/*LexState *ls*/);
 void forbody (/*LexState *ls,*/ int base, int line, int nvars, int isnum);
 void fornum (/*LexState *ls,*/ TString *varname, int line);
 void forlist (/*LexState *ls,*/ TString *indexname);
 void forstat (/*LexState *ls,*/ int line);
 void test_then_block (/*LexState *ls,*/ int *escapelist);
 void ifstat (/*LexState *ls,*/ int line);
 void localfunc (/*LexState *ls*/);
 void localstat (/*LexState *ls*/);
 int funcname (/*LexState *ls,*/ expdesc *v);
 void funcstat (/*LexState *ls,*/ int line);
 void exprstat (/*LexState *ls*/);
 void retstat (/*LexState *ls*/);
 //void statement (LexState *ls);
 void mainfunc (/*LexState *ls,*/ FuncState *fs);



private:
	void next();
	bool currIsNewline();
	void save_and_next();

	l_noret lexerror (/*LexState *ls,*/ const char *msg, int token);
	void save (/*LexState *ls,*/ int c);
	const char *txtToken (/*LexState *ls,*/ int token);

	void inclinenumber (/*LexState *ls*/);
	int check_next1 (/*LexState *ls,*/ int c);
	int check_next2 (/*LexState *ls,*/ const char *set);
	void buffreplace (/*LexState *ls,*/ char from, char to);

	void trydecpoint (/*LexState *ls,*/ TValue *o);
	int read_numeral (/*LexState *ls,*/ SemInfo *seminfo);
	int skip_sep (/*LexState *ls*/);
	void read_long_string (/*LexState *ls,*/ SemInfo *seminfo, int sep);
	void esccheck (/*LexState *ls,*/ int c, const char *msg);
	int gethexa (/*LexState *ls*/);
	int readhexaesc (/*LexState *ls*/);
	unsigned long readutf8esc (/*LexState *ls*/);
	void utf8esc (/*LexState *ls*/);
	int readdecesc (/*LexState *ls*/);
	void read_string (/*LexState *ls,*/ int del, SemInfo *seminfo);
	int llex (/*LexState *ls,*/ SemInfo *seminfo);
	int llex_default_func(/*LexState *ls,*/ SemInfo *seminfo);
} ;//LexState;


LUAI_FUNC void luaX_init (lua_State *L);
LUAI_FUNC void luaX_setinput (lua_State *L, LexState *ls, ZIO *z, TString *source, int firstchar);
//LUAI_FUNC TString *luaX_newstring (LexState *ls, const char *str, size_t l);
//LUAI_FUNC void luaX_next (LexState *ls);
//LUAI_FUNC int luaX_lookahead (LexState *ls);
//LUAI_FUNC l_noret luaX_syntaxerror (LexState *ls, const char *s);
//LUAI_FUNC const char *luaX_token2str (LexState *ls, int token);


#endif
