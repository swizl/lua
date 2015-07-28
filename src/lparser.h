/*
** $Id: lparser.h,v 1.74 2014/10/25 11:50:46 roberto Exp $
** Lua Parser
** See Copyright Notice in lua.h
*/

#ifndef lparser_h
#define lparser_h

#include "llimits.h"
#include "lobject.h"
#include "lzio.h"
#include "lopcodes.h"

/*
** Expression descriptor
*/

typedef enum {
	VVOID,	/* no value */
	VNIL,
	VTRUE,
	VFALSE,
	VK,		/* info = index of constant in 'k' */
	VKFLT,	/* nval = numerical float value */
	VKINT,	/* nval = numerical integer value */
	VNONRELOC,	/* info = result register */
	VLOCAL,	/* info = local register */
	VUPVAL,       /* info = index of upvalue in 'upvalues' */
	VINDEXED,	/* t = table register/upvalue; idx = index R/K */
	VJMP,		/* info = instruction pc */
	VRELOCABLE,	/* info = instruction pc */
	VCALL,	/* info = instruction pc */
	VVARARG	/* info = instruction pc */
} expkind;


#define vkisvar(k)	(VLOCAL <= (k) && (k) <= VINDEXED)
#define vkisinreg(k)	((k) == VNONRELOC || (k) == VLOCAL)
class expdesc {
//typedef struct expdesc {
public:
	expkind k;
	union {
		struct {  /* for indexed variables (VINDEXED) */
			short idx;  /* index (R/K) */
			lu_byte t;  /* table (register or upvalue) */
			lu_byte vt;  /* whether 't' is register (VLOCAL) or upvalue (VUPVAL) */
		} ind;
		int info;  /* for generic use */
		lua_Number nval;  /* for VKFLT */
		lua_Integer ival;    /* for VKINT */
	} u;
	int t;  /* patch list of 'exit when true' */
	int f;  /* patch list of 'exit when false' */
} ;//expdesc;


/* description of active local variable */
class Vardesc {
//typedef struct Vardesc {
public:
	short idx;  /* variable index in stack */
} ;//Vardesc;


/* description of pending goto statements and label statements */
class Labeldesc {
//typedef struct Labeldesc {
public:
	TString *name;  /* label identifier */
	int pc;  /* position in code */
	int line;  /* line where it appeared */
	lu_byte nactvar;  /* local level where it appears in current block */
} ;//Labeldesc;


/* list of labels or gotos */
class Labellist {
//typedef struct Labellist {
public:
	Labeldesc *arr;  /* array */
	int n;  /* number of entries in use */
	int size;  /* array size */
} ;//Labellist;


/* dynamic structures used by the parser */
class Dyndata {
//typedef struct Dyndata {
public:
	struct {  /* list of active local variables */
		Vardesc *arr;
		int n;
		int size;
	} actvar;
	Labellist gt;  /* list of pending gotos */
	Labellist label;   /* list of active labels */
} ;//Dyndata;


/* control of blocks */
//struct BlockCnt;  /* defined in lparser.c */
class BlockCnt;

class LexState;
/* state needed to generate code for a given function */
class FuncState {
//typedef struct FuncState {
public:
	Proto *f;  /* current function header */
	/*struct*/ FuncState *prev;  /* enclosing function */
	/*struct*/ LexState *ls;  /* lexical state */
	/*struct*/ BlockCnt *bl;  /* chain of current blocks */
	int pc;  /* next position to code (equivalent to 'ncode') */
	int lasttarget;   /* 'label' of last 'jump label' */
	int jpc;  /* list of pending jumps to 'pc' */
	int nk;  /* number of elements in 'k' */
	int np;  /* number of elements in 'p' */
	int firstlocal;  /* index of first local var (in Dyndata array) */
	short nlocvars;  /* number of elements in 'f->locvars' */
	lu_byte nactvar;  /* number of active local variables */
	lu_byte nups;  /* number of upvalues */
	lu_byte free_reg;  /* first free register */


//#define getcode(fs,e)	((fs)->f->code[(e)->u.info])
//Instruction getcode(expdesc *e);

//#define luaK_codeAsBx(fs,o,A,sBx)	luaK_codeABx(fs,o,A,(sBx)+MAXARG_sBx)
int luaK_codeAsBx(OpCode o, int A, unsigned int sBx);

//#define luaK_setmultret(fs,e)	luaK_setreturns(fs, e, LUA_MULTRET)
void luaK_setmultret(expdesc *e);

//#define luaK_jumpto(fs,t)	luaK_patchlist(fs, luaK_jump(fs), t)
void luaK_jumpto(int t);


/*LUAI_FUNC*/ int luaK_codeABx (/*FuncState *fs,*/ OpCode o, int A, unsigned int Bx);
/*LUAI_FUNC*/ int luaK_codeABC (/*FuncState *fs,*/ OpCode o, int A, int B, int C);
/*LUAI_FUNC*/ int luaK_codek (/*FuncState *fs,*/ int reg, int k);
/*LUAI_FUNC*/ void luaK_fixline (/*FuncState *fs,*/ int line);
/*LUAI_FUNC*/ void luaK_nil (/*FuncState *fs,*/ int from, int n);
/*LUAI_FUNC*/ void luaK_reserveregs (/*FuncState *fs,*/ int n);
/*LUAI_FUNC*/ void luaK_checkstack (/*FuncState *fs,*/ int n);
/*LUAI_FUNC*/ int luaK_stringK (/*FuncState *fs,*/ TString *s);
/*LUAI_FUNC*/ int luaK_intK (/*FuncState *fs,*/ lua_Integer n);
/*LUAI_FUNC*/ void luaK_dischargevars (/*FuncState *fs,*/ expdesc *e);
/*LUAI_FUNC*/ int luaK_exp2anyreg (/*FuncState *fs,*/ expdesc *e);
/*LUAI_FUNC*/ void luaK_exp2anyregup (/*FuncState *fs,*/ expdesc *e);
/*LUAI_FUNC*/ void luaK_exp2nextreg (/*FuncState *fs,*/ expdesc *e);
/*LUAI_FUNC*/ void luaK_exp2val (/*FuncState *fs,*/ expdesc *e);
/*LUAI_FUNC*/ int luaK_exp2RK (/*FuncState *fs,*/ expdesc *e);
/*LUAI_FUNC*/ void luaK_self (/*FuncState *fs,*/ expdesc *e, expdesc *key);
/*LUAI_FUNC*/ void luaK_indexed (/*FuncState *fs,*/ expdesc *t, expdesc *k);
/*LUAI_FUNC*/ void luaK_goiftrue (/*FuncState *fs,*/ expdesc *e);
/*LUAI_FUNC*/ void luaK_goiffalse (/*FuncState *fs,*/ expdesc *e);
/*LUAI_FUNC*/ void luaK_storevar (/*FuncState *fs,*/ expdesc *var, expdesc *e);
/*LUAI_FUNC*/ void luaK_setreturns (/*FuncState *fs,*/ expdesc *e, int nresults);
/*LUAI_FUNC*/ void luaK_setoneret (/*FuncState *fs,*/ expdesc *e);
/*LUAI_FUNC*/ int luaK_jump (/*FuncState *fs*/);
/*LUAI_FUNC*/ void luaK_ret (/*FuncState *fs,*/ int first, int nret);
/*LUAI_FUNC*/ void luaK_patchlist (/*FuncState *fs,*/ int list, int target);
/*LUAI_FUNC*/ void luaK_patchtohere (/*FuncState *fs,*/ int list);
/*LUAI_FUNC*/ void luaK_patchclose (/*FuncState *fs,*/ int list, int level);
/*LUAI_FUNC*/ void luaK_concat (/*FuncState *fs,*/ int *l1, int l2);
/*LUAI_FUNC*/ int luaK_getlabel (/*FuncState *fs*/);
/*LUAI_FUNC*/ void luaK_prefix (/*FuncState *fs,*/ UnOpr op, expdesc *v, int line);
/*LUAI_FUNC*/ void luaK_infix (/*FuncState *fs,*/ BinOpr op, expdesc *v);
/*LUAI_FUNC*/ void luaK_posfix (/*FuncState *fs,*/ BinOpr op, expdesc *v1, expdesc *v2, int line);
/*LUAI_FUNC*/ void luaK_setlist (/*FuncState *fs,*/ int base, int nelems, int tostore);

int condjump (/*FuncState *fs,*/ OpCode op, int A, int B, int C);
void fixjump (/*FuncState *fs,*/ int pc, int dest);
int getjump (/*FuncState *fs,*/ int pc);
Instruction *getjumpcontrol (/*FuncState *fs,*/ int pc);
int need_value (/*FuncState *fs,*/ int list);
int patchtestreg (/*FuncState *fs,*/ int node, int reg);
void removevalues (/*FuncState *fs,*/ int list);
void patchlistaux (/*FuncState *fs,*/ int list, int vtarget, int reg, int dtarget);
void dischargejpc (/*FuncState *fs*/);

int luaK_code (/*FuncState *fs,*/ Instruction i);
int codeextraarg (/*FuncState *fs,*/ int a);
void freereg (/*FuncState *fs,*/ int reg);/*FuncState *fs,*/
void freeexp (/*FuncState *fs,*/ expdesc *e);
int addk (/*FuncState *fs,*/ TValue *key, TValue *v);
int luaK_numberK (/*FuncState *fs,*/ lua_Number r);
int boolK (/*FuncState *fs,*/ int b);
int nilK (/*FuncState *fs*/);
int code_label (/*FuncState *fs,*/ int A, int b, int jump);
void discharge2reg (/*FuncState *fs,*/ expdesc *e, int reg);
void discharge2anyreg (/*FuncState *fs,*/ expdesc *e);
void exp2reg (/*FuncState *fs,*/ expdesc *e, int reg);
void invertjump (/*FuncState *fs,*/ expdesc *e);
int jumponcond (/*FuncState *fs,*/ expdesc *e, int cond);
void codenot (/*FuncState *fs,*/ expdesc *e);
//int validop (int op, TValue *v1, TValue *v2);
int constfolding (/*FuncState *fs,*/ int op, expdesc *e1, expdesc *e2);
void codeexpval (/*FuncState *fs,*/ OpCode op, expdesc *e1, expdesc *e2, int line);
void codecomp (/*FuncState *fs,*/ OpCode op, int cond, expdesc *e1, expdesc *e2);



l_noret errorlimit (/*FuncState *fs,*/ int limit, const char *what);
void checklimit (/*FuncState *fs,*/ int v, int l, const char *what);
LocVar *getlocvar (/*FuncState *fs,*/ int i);
void removevars (/*FuncState *fs,*/ int tolevel);
int searchupvalue (/*FuncState *fs,*/ TString *name);
int newupvalue (/*FuncState *fs,*/ TString *name, expdesc *v);
int searchvar (/*FuncState *fs,*/ TString *n);
void markupval (/*FuncState *fs,*/ int level);
int singlevaraux (/*FuncState *fs,*/ TString *n, expdesc *var, int base);
void movegotosout (/*FuncState *fs,*/ BlockCnt *bl);
void enterblock (/*FuncState *fs,*/ BlockCnt *bl, lu_byte isloop);
void leaveblock (/*FuncState *fs*/);
void closelistfield (/*FuncState *fs,*/ struct ConsControl *cc);
void lastlistfield (/*FuncState *fs,*/ struct ConsControl *cc);
void checkrepeated (/*FuncState *fs,*/ Labellist *ll, TString *label);

} ;//FuncState;


LUAI_FUNC LClosure *luaY_parser (lua_State *L, ZIO *z, Mbuffer *buff, Dyndata *dyd, const char *name, int firstchar);


#endif
