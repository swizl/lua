/*
** $Id: llex.h,v 1.78 2014/10/29 15:38:24 roberto Exp $
** Lexical Analyzer
** See Copyright Notice in lua.h
*/

#ifndef llex_h
#define llex_h

#include "lobject.h"
#include "lzio.h"


#define FIRST_RESERVED	257


#if !defined(LUA_ENV)
#define LUA_ENV		"_ENV"
#endif


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

class FuncState;
class Dyndata;
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
} ;//LexState;


LUAI_FUNC void luaX_init (lua_State *L);
LUAI_FUNC void luaX_setinput (lua_State *L, LexState *ls, ZIO *z, TString *source, int firstchar);
//LUAI_FUNC TString *luaX_newstring (LexState *ls, const char *str, size_t l);
//LUAI_FUNC void luaX_next (LexState *ls);
//LUAI_FUNC int luaX_lookahead (LexState *ls);
//LUAI_FUNC l_noret luaX_syntaxerror (LexState *ls, const char *s);
//LUAI_FUNC const char *luaX_token2str (LexState *ls, int token);


#endif
