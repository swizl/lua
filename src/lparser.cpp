/*
** $Id: lparser.c,v 2.147 2014/12/27 20:31:43 roberto Exp $
** Lua Parser
** See Copyright Notice in lua.h
*/

#define lparser_c
#define LUA_CORE

#include "lprefix.h"


#include <string.h>

#include "lua.h"

#include "lcode.h"
#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "llex.h"
#include "lmem.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lparser.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"



/* maximum number of local variables per function (must be smaller
	 than 250, due to the bytecode format) */
#define MAXVARS		200


#define hasmultret(k)		((k) == VCALL || (k) == VVARARG)


/* because all strings are unified by the scanner, the parser
	 can use pointer equality for string equality */
#define eqstr(a,b)	((a) == (b))


/*
** nodes for block list (list of active blocks)
*/
class BlockCnt {
//typedef struct BlockCnt {
public:
	/*struct*/ BlockCnt *previous;  /* chain */
	int firstlabel;  /* index of first label in this block */
	int firstgoto;  /* index of first pending goto in this block */
	lu_byte nactvar;  /* # active locals outside the block */
	lu_byte upval;  /* true if some variable in the block is an upvalue */
	lu_byte isloop;  /* true if 'block' is a loop */
} ;//BlockCnt;



/*
** prototypes for recursive non-terminal functions
*/
//static void statement (LexState *ls);
//static void expr (LexState *ls, expdesc *v);


/* semantic error */
/*static*/ l_noret LexState::semerror (/*LexState *ls,*/ const char *msg) {
	t.token = 0;  /* remove "near <token>" from final message */
	luaX_syntaxerror(msg);
}


/*static*/ l_noret LexState::error_expected (/*LexState *ls,*/ int token) {
	luaX_syntaxerror(
			luaO_pushfstring(L, "%s expected", luaX_token2str(token)));
}


/*static*/ l_noret FuncState::errorlimit (/*FuncState *fs,*/ int limit, const char *what) {
	lua_State *L = ls->L;
	const char *msg;
	int line = f->linedefined;
	const char *where = (line == 0)
											? "main function"
											: luaO_pushfstring(L, "function at line %d", line);
	msg = luaO_pushfstring(L, "too many %s (limit is %d) in %s",
														 what, limit, where);
	ls->luaX_syntaxerror(msg);
}


/*static*/ void FuncState::checklimit (/*FuncState *fs,*/ int v, int l, const char *what) {
	if (v > l) errorlimit(l, what);
}


/*static*/ int LexState::testnext (/*LexState *ls,*/ int c) {
	if (t.token == c) {
		luaX_next();
		return 1;
	}
	else return 0;
}


/*static*/ void LexState::check (/*LexState *ls,*/ int c) {
	if (t.token != c)
		error_expected(c);
}


/*static*/ void LexState::checknext (/*LexState *ls,*/ int c) {
	check(c);
	luaX_next();
}


//#define check_condition(ls,c,msg)	{ if (!(c)) ls->luaX_syntaxerror(msg); }
l_noret LexState::check_condition(bool c , const char * msg)
{
		if (!c) luaX_syntaxerror(msg);
}


/*static*/ void LexState::check_match (/*LexState *ls,*/ int what, int who, int where) {
	if (!testnext(what)) {
		if (where == linenumber)
			error_expected(what);
		else {
			luaX_syntaxerror( luaO_pushfstring(L,
						 "%s expected (to close %s at line %d)",
							luaX_token2str(what), luaX_token2str(who), where));
		}
	}
}


/*static*/ TString *LexState::str_checkname (/*LexState *ls*/) {
	TString *ts;
	check(TK_NAME);
	ts = t.seminfo.ts;
	luaX_next();
	return ts;
}


static void init_exp (expdesc *e, expkind k, int i) {
	e->f = e->t = NO_JUMP;
	e->k = k;
	e->u.info = i;
}


/*static*/ void LexState::codestring (/*LexState *ls,*/ expdesc *e, TString *s) {
	init_exp(e, VK, fs->luaK_stringK(s));
}


/*static*/ void LexState::checkname (/*LexState *ls,*/ expdesc *e) {
	codestring(e, str_checkname());
}


/*static*/ int LexState::registerlocalvar (/*LexState *ls,*/ TString *varname) {
	//FuncState *fs = fs;
	Proto *f = fs->f;
	int oldsize = f->sizelocvars;
	luaM_growvector(L, f->locvars, fs->nlocvars, f->sizelocvars,
									LocVar, SHRT_MAX, "local variables");
	while (oldsize < f->sizelocvars) f->locvars[oldsize++].varname = NULL;
	f->locvars[fs->nlocvars].varname = varname;
	luaC_objbarrier(L, f, varname);
	return fs->nlocvars++;
}


/*static*/ void LexState::new_localvar (/*LexState *ls,*/ TString *name) {
	//FuncState *fs = fs;
	//Dyndata *dyd = dyd;
	int reg = registerlocalvar(name);
	fs->checklimit(dyd->actvar.n + 1 - fs->firstlocal,
									MAXVARS, "local variables");
	luaM_growvector(L, dyd->actvar.arr, dyd->actvar.n + 1,
									dyd->actvar.size, Vardesc, MAX_INT, "local variables");
	dyd->actvar.arr[dyd->actvar.n++].idx = cast(short, reg);
}


/*static*/ void LexState::new_localvarliteral_ (/*LexState *ls,*/ const char *name, size_t sz) {
	new_localvar(luaX_newstring(name, sz));
}

//#define new_localvarliteral(ls,v) \
	new_localvarliteral_(ls, "" v, (sizeof(v)/sizeof(char))-1)

#define new_localvarliteral(v) \
	new_localvarliteral_("" v, (sizeof(v)/sizeof(char))-1)
//void LexState::new_localvarliteral(char v[])
//{
//	new_localvarliteral_("" v, (sizeof(v)/sizeof(char))-1);
//}


/*static*/ LocVar *FuncState::getlocvar (/*FuncState *fs,*/ int i) {
	int idx = ls->dyd->actvar.arr[firstlocal + i].idx;
	lua_assert(idx < nlocvars);
	return &f->locvars[idx];
}


/*static*/ void LexState::adjustlocalvars (/*LexState *ls,*/ int nvars) {
	//FuncState *fs = fs;
	fs->nactvar = cast_byte(fs->nactvar + nvars);
	for (; nvars; nvars--) {
		fs->getlocvar(fs->nactvar - nvars)->startpc = fs->pc;
	}
}


/*static*/ void FuncState::removevars (/*FuncState *fs,*/ int tolevel) {
	ls->dyd->actvar.n -= (nactvar - tolevel);
	while (nactvar > tolevel)
		getlocvar(--nactvar)->endpc = pc;
}


/*static*/ int FuncState::searchupvalue (/*FuncState *fs,*/ TString *name) {
	int i;
	Upvaldesc *up = f->upvalues;
	for (i = 0; i < nups; i++) {
		if (eqstr(up[i].name, name)) return i;
	}
	return -1;  /* not found */
}


/*static*/ int FuncState::newupvalue (/*FuncState *fs,*/ TString *name, expdesc *v) {
	//Proto *f = fs->f;
	int oldsize = f->sizeupvalues;
	checklimit(nups + 1, MAXUPVAL, "upvalues");
	luaM_growvector(ls->L, f->upvalues, nups, f->sizeupvalues,
									Upvaldesc, MAXUPVAL, "upvalues");
	while (oldsize < f->sizeupvalues) f->upvalues[oldsize++].name = NULL;
	f->upvalues[nups].instack = (v->k == VLOCAL);
	f->upvalues[nups].idx = cast_byte(v->u.info);
	f->upvalues[nups].name = name;
	luaC_objbarrier(ls->L, f, name);
	return nups++;
}


/*static*/ int FuncState::searchvar (/*FuncState *fs,*/ TString *n) {
	int i;
	for (i = cast_int(nactvar) - 1; i >= 0; i--) {
		if (eqstr(n, getlocvar(i)->varname))
			return i;
	}
	return -1;  /* not found */
}


/*
	Mark block where variable at given level was defined
	(to emit close instructions later).
*/
/*static*/ void FuncState::markupval (/*FuncState *fs,*/ int level) {
	//BlockCnt *bl = fs->bl;
	while (bl->nactvar > level) bl = bl->previous;
	bl->upval = 1;
}


/*
	Find variable with given name 'n'. If it is an upvalue, add this
	upvalue into all intermediate functions.
*/
/*static*/ int FuncState::singlevaraux (/*FuncState *fs,*/ TString *n, expdesc *var, int base) {
	//if (fs == NULL)  /* no more levels? */
		//return VVOID;  /* default is global */
	//else {
		int v = searchvar(n);  /* look up locals at current level */
		if (v >= 0) {  /* found? */
			init_exp(var, VLOCAL, v);  /* variable is local */
			if (!base)
				markupval(v);  /* local will be used as an upval */
			return VLOCAL;
		}
		else {  /* not found as local at current level; try upvalues */
			int idx = searchupvalue(n);  /* try existing upvalues */
			if (idx < 0) {  /* not found? */
				if (prev->singlevaraux(n, var, 0) == VVOID) /* try upper levels */
					return VVOID;  /* not found; is a global */
				/* else was LOCAL or UPVAL */
				idx  = newupvalue(n, var);  /* will be a new upvalue */
			}
			init_exp(var, VUPVAL, idx);
			return VUPVAL;
		}
	//}
}


/*static*/ void LexState::singlevar (/*LexState *ls,*/ expdesc *var) {
	TString *varname = str_checkname();
	//FuncState *fs = fs;
	if (fs->singlevaraux(varname, var, 1) == VVOID) {  /* global name? */
		expdesc key;
		fs->singlevaraux(envn, var, 1);  /* get environment variable */
		lua_assert(var->k == VLOCAL || var->k == VUPVAL);
		codestring(&key, varname);  /* key is variable name */
		fs->luaK_indexed(var, &key);  /* env[varname] */
	}
}


/*static*/ void LexState::adjust_assign (/*LexState *ls,*/ int nvars, int nexps, expdesc *e) {
	//FuncState *fs = fs;
	int extra = nvars - nexps;
	if (hasmultret(e->k)) {
		extra++;  /* includes call itself */
		if (extra < 0) extra = 0;
		fs->luaK_setreturns(e, extra);  /* last exp. provides the difference */
		if (extra > 1) fs->luaK_reserveregs(extra-1);
	}
	else {
		if (e->k != VVOID) fs->luaK_exp2nextreg(e);  /* close last expression */
		if (extra > 0) {
			int reg = fs->free_reg;
			fs->luaK_reserveregs(extra);
			fs->luaK_nil(reg, extra);
		}
	}
}


/*static*/ void LexState::enterlevel (/*LexState *ls*/) {
	//lua_State *L = L;
	++L->nCcalls;
	fs->checklimit(L->nCcalls, LUAI_MAXCCALLS, "C levels");
}


//#define leavelevel(ls)	((ls)->L->nCcalls--)
void LexState::leavelevel()
{
		L->nCcalls--;
}


/*static*/ void LexState::closegoto (/*LexState *ls,*/ int g, Labeldesc *label) {
	int i;
	//FuncState *fs = fs;
	Labellist *gl = &dyd->gt;
	Labeldesc *gt = &gl->arr[g];
	lua_assert(eqstr(gt->name, label->name));
	if (gt->nactvar < label->nactvar) {
		TString *vname = fs->getlocvar(gt->nactvar)->varname;
		const char *msg = luaO_pushfstring(L,
			"<goto %s> at line %d jumps into the scope of local '%s'",
			getstr(gt->name), gt->line, getstr(vname));
		semerror(msg);
	}
	fs->luaK_patchlist(gt->pc, label->pc);
	/* remove goto from pending list */
	for (i = g; i < gl->n - 1; i++)
		gl->arr[i] = gl->arr[i + 1];
	gl->n--;
}


/*
** try to close a goto with existing labels; this solves backward jumps
*/
/*static*/ int LexState::findlabel (/*LexState *ls,*/ int g) {
	int i;
	BlockCnt *bl = fs->bl;
	Dyndata *dyd = dyd;
	Labeldesc *gt = &dyd->gt.arr[g];
	/* check labels in current block for a match */
	for (i = bl->firstlabel; i < dyd->label.n; i++) {
		Labeldesc *lb = &dyd->label.arr[i];
		if (eqstr(lb->name, gt->name)) {  /* correct label? */
			if (gt->nactvar > lb->nactvar &&
					(bl->upval || dyd->label.n > bl->firstlabel))
				fs->luaK_patchclose(gt->pc, lb->nactvar);
			closegoto(g, lb);  /* close it */
			return 1;
		}
	}
	return 0;  /* label not found; cannot close goto */
}


/*static*/ int LexState::newlabelentry (/*LexState *ls,*/ Labellist *l, TString *name, int line, int pc) {
	int n = l->n;
	luaM_growvector(L, l->arr, n, l->size, Labeldesc, SHRT_MAX, "labels/gotos");
	l->arr[n].name = name;
	l->arr[n].line = line;
	l->arr[n].nactvar = fs->nactvar;
	l->arr[n].pc = pc;
	l->n = n + 1;
	return n;
}


/*
** check whether new label 'lb' matches any pending gotos in current
** block; solves forward jumps
*/
/*static*/ void LexState::findgotos (/*LexState *ls,*/ Labeldesc *lb) {
	Labellist *gl = &dyd->gt;
	int i = fs->bl->firstgoto;
	while (i < gl->n) {
		if (eqstr(gl->arr[i].name, lb->name))
			closegoto(i, lb);
		else
			i++;
	}
}


/*
** export pending gotos to outer level, to check them against
** outer labels; if the block being exited has upvalues, and
** the goto exits the scope of any variable (which can be the
** upvalue), close those variables being exited.
*/
/*static*/ void FuncState::movegotosout (/*FuncState *fs,*/ BlockCnt *bl) {
	int i = bl->firstgoto;
	Labellist *gl = &ls->dyd->gt;
	/* correct pending gotos to current block and try to close it
		 with visible labels */
	while (i < gl->n) {
		Labeldesc *gt = &gl->arr[i];
		if (gt->nactvar > bl->nactvar) {
			if (bl->upval)
				luaK_patchclose(gt->pc, bl->nactvar);
			gt->nactvar = bl->nactvar;
		}
		if (!ls->findlabel(i))
			i++;  /* move to next one */
	}
}


/*static*/ void FuncState::enterblock (/*FuncState *fs,*/ BlockCnt *bl, lu_byte isloop) {
	bl->isloop = isloop;
	bl->nactvar = nactvar;
	bl->firstlabel = ls->dyd->label.n;
	bl->firstgoto = ls->dyd->gt.n;
	bl->upval = 0;
	bl->previous = bl;
	this->bl = bl;
	lua_assert(free_reg == nactvar);
}


/*
** create a label named 'break' to resolve break statements
*/
/*static*/ void LexState::breaklabel (/*LexState *ls*/) {
	TString *n = luaS_new(L, "break");
	int l = newlabelentry(&dyd->label, n, 0, fs->pc);
	findgotos(&dyd->label.arr[l]);
}

/*
** generates an error for an undefined 'goto'; choose appropriate
** message when label name is a reserved word (which can only be 'break')
*/
/*static*/ l_noret LexState::undefgoto (/*LexState *ls,*/ Labeldesc *gt) {
	const char *msg = isreserved(gt->name)
										? "<%s> at line %d not inside a loop"
										: "no visible label '%s' for <goto> at line %d";
	msg = luaO_pushfstring(L, msg, getstr(gt->name), gt->line);
	semerror(msg);
}


/*static*/ void FuncState::leaveblock (/*FuncState *fs*/) {
	//BlockCnt *bl = fs->bl;
	//LexState *ls = fs->ls;
	if (bl->previous && bl->upval) {
		/* create a 'jump to here' to close upvalues */
		int j = luaK_jump();
		luaK_patchclose(j, bl->nactvar);
		luaK_patchtohere(j);
	}
	if (bl->isloop)
		ls->breaklabel();  /* close pending breaks */
	bl = bl->previous;
	removevars(bl->nactvar);
	lua_assert(bl->nactvar == nactvar);
	free_reg = nactvar;  /* free registers */
	ls->dyd->label.n = bl->firstlabel;  /* remove local labels */
	if (bl->previous)  /* inner block? */
		movegotosout(bl);  /* update pending gotos to outer block */
	else if (bl->firstgoto < ls->dyd->gt.n)  /* pending gotos in outer block? */
		ls->undefgoto(&ls->dyd->gt.arr[bl->firstgoto]);  /* error */
}


/*
** adds a new prototype into list of prototypes
*/
/*static*/ Proto *LexState::addprototype (/*LexState *ls*/) {
	Proto *clp;
	//lua_State *L = ls->L;
	//FuncState *fs = ls->fs;
	Proto *f = fs->f;  /* prototype of current function */
	if (fs->np >= f->sizep) {
		int oldsize = f->sizep;
		luaM_growvector(L, f->p, fs->np, f->sizep, Proto *, MAXARG_Bx, "functions");
		while (oldsize < f->sizep) f->p[oldsize++] = NULL;
	}
	f->p[fs->np++] = clp = luaF_newproto(L);
	luaC_objbarrier(L, f, clp);
	return clp;
}


/*
** codes instruction to create new closure in parent function.
** The OP_CLOSURE instruction must use the last available register,
** so that, if it invokes the GC, the GC knows which registers
** are in use at that time.
*/
/*static*/ void LexState::codeclosure (/*LexState *ls,*/ expdesc *v) {
	FuncState *fs = this->fs->prev;
	init_exp(v, VRELOCABLE, fs->luaK_codeABx(OP_CLOSURE, 0, fs->np - 1));
	fs->luaK_exp2nextreg(v);  /* fix it at the last register */
}


/*static*/ void LexState::open_func (/*LexState *ls,*/ FuncState *fs, BlockCnt *bl) {
	Proto *f;
	fs->prev = this->fs;  /* linked list of funcstates */
	fs->ls = this;
	this->fs = fs;
	fs->pc = 0;
	fs->lasttarget = 0;
	fs->jpc = NO_JUMP;
	fs->free_reg = 0;
	fs->nk = 0;
	fs->np = 0;
	fs->nups = 0;
	fs->nlocvars = 0;
	fs->nactvar = 0;
	fs->firstlocal = dyd->actvar.n;
	fs->bl = NULL;
	f = fs->f;
	f->source = source;
	f->maxstacksize = 2;  /* registers 0/1 are always valid */
	fs->enterblock(bl, 0);
}


/*static*/ void LexState::close_func (/*LexState *ls*/) {
	//lua_State *L = ls->L;
	//FuncState *fs = ls->fs;
	Proto *f = fs->f;
	fs->luaK_ret(0, 0);  /* final return */
	fs->leaveblock();
	luaM_reallocvector(L, f->code, f->sizecode, fs->pc, Instruction);
	f->sizecode = fs->pc;
	luaM_reallocvector(L, f->lineinfo, f->sizelineinfo, fs->pc, int);
	f->sizelineinfo = fs->pc;
	luaM_reallocvector(L, f->k, f->sizek, fs->nk, TValue);
	f->sizek = fs->nk;
	luaM_reallocvector(L, f->p, f->sizep, fs->np, Proto *);
	f->sizep = fs->np;
	luaM_reallocvector(L, f->locvars, f->sizelocvars, fs->nlocvars, LocVar);
	f->sizelocvars = fs->nlocvars;
	luaM_reallocvector(L, f->upvalues, f->sizeupvalues, fs->nups, Upvaldesc);
	f->sizeupvalues = fs->nups;
	lua_assert(fs->bl == NULL);
	fs = fs->prev;
	luaC_checkGC(L);
}



/*============================================================*/
/* GRAMMAR RULES */
/*============================================================*/


/*
** check whether current token is in the follow set of a block.
** 'until' closes syntactical blocks, but do not close scope,
** so it is handled in separate.
*/
/*static*/ int LexState::block_follow (/*LexState *ls,*/ int withuntil) {
	switch (t.token) {
		case TK_ELSE: case TK_ELSEIF:
		case TK_END: case TK_EOS:
			return 1;
		case TK_UNTIL: return withuntil;
		default: return 0;
	}
}


/*static*/ void LexState::statlist (/*LexState *ls*/) {
	/* statlist -> { stat [';'] } */
	while (!block_follow(1)) {
		if (t.token == TK_RETURN) {
			statement();
			return;  /* 'return' must be last statement */
		}
		statement();
	}
}


/*static*/ void LexState::fieldsel (/*LexState *ls,*/ expdesc *v) {
	/* fieldsel -> ['.' | ':'] NAME */
	//FuncState *fs = ls->fs;
	expdesc key;
	fs->luaK_exp2anyregup(v);
	luaX_next();  /* skip the dot or colon */
	checkname(&key);
	fs->luaK_indexed(v, &key);
}


/*static*/ void LexState::yindex (/*LexState *ls,*/ expdesc *v) {
	/* index -> '[' expr ']' */
	luaX_next();  /* skip the '[' */
	expr(v);
	fs->luaK_exp2val(v);
	checknext(']');
}


/*
** {======================================================================
** Rules for Constructors
** =======================================================================
*/

class ConsControl {
//struct ConsControl {
public:
	expdesc v;  /* last list item read */
	expdesc *t;  /* table descriptor */
	int nh;  /* total number of 'record' elements */
	int na;  /* total number of array elements */
	int tostore;  /* number of array elements pending to be stored */
};


/*static*/ void LexState::recfield (/*LexState *ls,*/ struct ConsControl *cc) {
	/* recfield -> (NAME | '['exp1']') = exp1 */
	//FuncState *fs = ls->fs;
	int reg = fs->free_reg;
	expdesc key, val;
	int rkkey;
	if (t.token == TK_NAME) {
		fs->checklimit(cc->nh, MAX_INT, "items in a constructor");
		checkname(&key);
	}
	else  /* ls->t.token == '[' */
		yindex(&key);
	cc->nh++;
	checknext('=');
	rkkey = fs->luaK_exp2RK(&key);
	expr(&val);
	fs->luaK_codeABC(OP_SETTABLE, cc->t->u.info, rkkey, fs->luaK_exp2RK(&val));
	fs->free_reg = reg;  /* free registers */
}


/*static*/ void FuncState::closelistfield (/*FuncState *fs,*/ struct ConsControl *cc) {
	if (cc->v.k == VVOID) return;  /* there is no list item */
	luaK_exp2nextreg(&cc->v);
	cc->v.k = VVOID;
	if (cc->tostore == LFIELDS_PER_FLUSH) {
		luaK_setlist(cc->t->u.info, cc->na, cc->tostore);  /* flush */
		cc->tostore = 0;  /* no more items pending */
	}
}


/*static*/ void FuncState::lastlistfield (/*FuncState *fs,*/ struct ConsControl *cc) {
	if (cc->tostore == 0) return;
	if (hasmultret(cc->v.k)) {
		luaK_setmultret(&cc->v);
		luaK_setlist(cc->t->u.info, cc->na, LUA_MULTRET);
		cc->na--;  /* do not count last expression (unknown number of elements) */
	}
	else {
		if (cc->v.k != VVOID)
			luaK_exp2nextreg(&cc->v);
		luaK_setlist(cc->t->u.info, cc->na, cc->tostore);
	}
}


/*static*/ void LexState::listfield (/*LexState *ls,*/ struct ConsControl *cc) {
	/* listfield -> exp */
	expr(&cc->v);
	fs->checklimit(cc->na, MAX_INT, "items in a constructor");
	cc->na++;
	cc->tostore++;
}


/*static*/ void LexState::field (/*LexState *ls,*/ struct ConsControl *cc) {
	/* field -> listfield | recfield */
	switch(t.token) {
		case TK_NAME: {  /* may be 'listfield' or 'recfield' */
			if (luaX_lookahead() != '=')  /* expression? */
				listfield(cc);
			else
				recfield(cc);
			break;
		}
		case '[': {
			recfield(cc);
			break;
		}
		default: {
			listfield(cc);
			break;
		}
	}
}


/*static*/ void LexState::constructor (/*LexState *ls,*/ expdesc *t) {
	/* constructor -> '{' [ field { sep field } [sep] ] '}'
		 sep -> ',' | ';' */
	//FuncState *fs = ls->fs;
	int line = linenumber;
	int pc = fs->luaK_codeABC(OP_NEWTABLE, 0, 0, 0);
	struct ConsControl cc;
	cc.na = cc.nh = cc.tostore = 0;
	cc.t = t;
	init_exp(t, VRELOCABLE, pc);
	init_exp(&cc.v, VVOID, 0);  /* no value (yet) */
	fs->luaK_exp2nextreg(t);  /* fix it at stack top */
	checknext('{');
	do {
		lua_assert(cc.v.k == VVOID || cc.tostore > 0);
		if (this->t.token == '}') break;
		fs->closelistfield(&cc);
		field(&cc);
	} while (testnext(',') || testnext(';'));
	check_match('}', '{', line);
	fs->lastlistfield(&cc);
	SETARG_B(fs->f->code[pc], luaO_int2fb(cc.na)); /* set initial array size */
	SETARG_C(fs->f->code[pc], luaO_int2fb(cc.nh));  /* set initial table size */
}

/* }====================================================================== */



/*static*/ void LexState::parlist (/*LexState *ls*/) {
	/* parlist -> [ param { ',' param } ] */
	//FuncState *fs = ls->fs;
	Proto *f = fs->f;
	int nparams = 0;
	f->is_vararg = 0;
	if (t.token != ')') {  /* is 'parlist' not empty? */
		do {
			switch (t.token) {
				case TK_NAME: {  /* param -> NAME */
					new_localvar(str_checkname());
					nparams++;
					break;
				}
				case TK_DOTS: {  /* param -> '...' */
					luaX_next();
					f->is_vararg = 1;
					break;
				}
				default: luaX_syntaxerror( "<name> or '...' expected");
			}
		} while (!f->is_vararg && testnext(','));
	}
	adjustlocalvars(nparams);
	f->numparams = cast_byte(fs->nactvar);
	fs->luaK_reserveregs(fs->nactvar);  /* reserve register for parameters */
}


/*static*/ void LexState::body (/*LexState *ls,*/ expdesc *e, int ismethod, int line) {
	/* body ->  '(' parlist ')' block END */
	FuncState new_fs;
	BlockCnt bl;
	new_fs.f = addprototype();
	new_fs.f->linedefined = line;
	open_func(&new_fs, &bl);
	checknext('(');
	if (ismethod) {
		new_localvarliteral("self");  /* create 'self' parameter */
		adjustlocalvars(1);
	}
	parlist();
	checknext(')');
	statlist();
	new_fs.f->lastlinedefined = linenumber;
	check_match(TK_END, TK_FUNCTION, line);
	codeclosure(e);
	close_func();
}


/*static*/ int LexState::explist (/*LexState *ls,*/ expdesc *v) {
	/* explist -> expr { ',' expr } */
	int n = 1;  /* at least one expression */
	expr(v);
	while (testnext(',')) {
		fs->luaK_exp2nextreg(v);
		expr(v);
		n++;
	}
	return n;
}


/*static*/ void LexState::funcargs (/*LexState *ls,*/ expdesc *f, int line) {
	//FuncState *fs = ls->fs;
	expdesc args;
	int base, nparams;
	switch (t.token) {
		case '(': {  /* funcargs -> '(' [ explist ] ')' */
			luaX_next();
			if (t.token == ')')  /* arg list is empty? */
				args.k = VVOID;
			else {
				explist(&args);
				fs->luaK_setmultret(&args);
			}
			check_match(')', '(', line);
			break;
		}
		case '{': {  /* funcargs -> constructor */
			constructor(&args);
			break;
		}
		case TK_STRING: {  /* funcargs -> STRING */
			codestring(&args, t.seminfo.ts);
			luaX_next();  /* must use 'seminfo' before 'next' */
			break;
		}
		default: {
			luaX_syntaxerror( "function arguments expected");
		}
	}
	lua_assert(f->k == VNONRELOC);
	base = f->u.info;  /* base register for call */
	if (hasmultret(args.k))
		nparams = LUA_MULTRET;  /* open call */
	else {
		if (args.k != VVOID)
			fs->luaK_exp2nextreg(&args);  /* close last argument */
		nparams = fs->free_reg - (base+1);
	}
	init_exp(f, VCALL, fs->luaK_codeABC(OP_CALL, base, nparams+1, 2));
	fs->luaK_fixline(line);
	fs->free_reg = base+1;  /* call remove function and arguments and leaves
														(unless changed) one result */
}




/*
** {======================================================================
** Expression parsing
** =======================================================================
*/


/*static*/ void LexState::primaryexp (/*LexState *ls,*/ expdesc *v) {
	/* primaryexp -> NAME | '(' expr ')' */
	switch (t.token) {
		case '(': {
			int line = linenumber;
			luaX_next();
			expr(v);
			check_match(')', '(', line);
			fs->luaK_dischargevars(v);
			return;
		}
		case TK_NAME: {
			singlevar(v);
			return;
		}
		default: {
			luaX_syntaxerror( "unexpected symbol");
		}
	}
}


/*static*/ void LexState::suffixedexp (/*LexState *ls,*/ expdesc *v) {
	/* suffixedexp ->
			 primaryexp { '.' NAME | '[' exp ']' | ':' NAME funcargs | funcargs } */
	//FuncState *fs = ls->fs;
	int line = linenumber;
	primaryexp(v);
	for (;;) {
		switch (t.token) {
			case '.': {  /* fieldsel */
				fieldsel(v);
				break;
			}
			case '[': {  /* '[' exp1 ']' */
				expdesc key;
				fs->luaK_exp2anyregup(v);
				yindex(&key);
				fs->luaK_indexed(v, &key);
				break;
			}
			case ':': {  /* ':' NAME funcargs */
				expdesc key;
				luaX_next();
				checkname(&key);
				fs->luaK_self(v, &key);
				funcargs(v, line);
				break;
			}
			case '(': case TK_STRING: case '{': {  /* funcargs */
				fs->luaK_exp2nextreg(v);
				funcargs(v, line);
				break;
			}
			default: return;
		}
	}
}


/*static*/ void LexState::simpleexp (/*LexState *ls,*/ expdesc *v) {
	/* simpleexp -> FLT | INT | STRING | NIL | TRUE | FALSE | ... |
									constructor | FUNCTION body | suffixedexp */
	switch (t.token) {
		case TK_FLT: {
			init_exp(v, VKFLT, 0);
			v->u.nval = t.seminfo.r;
			break;
		}
		case TK_INT: {
			init_exp(v, VKINT, 0);
			v->u.ival = t.seminfo.i;
			break;
		}
		case TK_STRING: {
			codestring(v, t.seminfo.ts);
			break;
		}
		case TK_NIL: {
			init_exp(v, VNIL, 0);
			break;
		}
		case TK_TRUE: {
			init_exp(v, VTRUE, 0);
			break;
		}
		case TK_FALSE: {
			init_exp(v, VFALSE, 0);
			break;
		}
		case TK_DOTS: {  /* vararg */
			//FuncState *fs = ls->fs;
			check_condition(fs->f->is_vararg,
											"cannot use '...' outside a vararg function");
			init_exp(v, VVARARG, fs->luaK_codeABC(OP_VARARG, 0, 1, 0));
			break;
		}
		case '{': {  /* constructor */
			constructor(v);
			return;
		}
		case TK_FUNCTION: {
			luaX_next();
			body(v, 0, linenumber);
			return;
		}
		default: {
			suffixedexp(v);
			return;
		}
	}
	luaX_next();
}


static UnOpr getunopr (int op) {
	switch (op) {
		case TK_NOT: return OPR_NOT;
		case '-': return OPR_MINUS;
		case '~': return OPR_BNOT;
		case '#': return OPR_LEN;
		default: return OPR_NOUNOPR;
	}
}


static BinOpr getbinopr (int op) {
	switch (op) {
		case '+': return OPR_ADD;
		case '-': return OPR_SUB;
		case '*': return OPR_MUL;
		case '%': return OPR_MOD;
		case '^': return OPR_POW;
		case '/': return OPR_DIV;
		case TK_IDIV: return OPR_IDIV;
		case '&': return OPR_BAND;
		case '|': return OPR_BOR;
		case '~': return OPR_BXOR;
		case TK_SHL: return OPR_SHL;
		case TK_SHR: return OPR_SHR;
		case TK_CONCAT: return OPR_CONCAT;
		case TK_NE: return OPR_NE;
		case TK_EQ: return OPR_EQ;
		case '<': return OPR_LT;
		case TK_LE: return OPR_LE;
		case '>': return OPR_GT;
		case TK_GE: return OPR_GE;
		case TK_AND: return OPR_AND;
		case TK_OR: return OPR_OR;
		default: return OPR_NOBINOPR;
	}
}


static const struct {
	lu_byte left;  /* left priority for each binary operator */
	lu_byte right; /* right priority */
} priority[] = {  /* ORDER OPR */
	 {10, 10}, {10, 10},           /* '+' '-' */
	 {11, 11}, {11, 11},           /* '*' '%' */
	 {14, 13},                  /* '^' (right associative) */
	 {11, 11}, {11, 11},           /* '/' '//' */
	 {6, 6}, {4, 4}, {5, 5},   /* '&' '|' '~' */
	 {7, 7}, {7, 7},           /* '<<' '>>' */
	 {9, 8},                   /* '..' (right associative) */
	 {3, 3}, {3, 3}, {3, 3},   /* ==, <, <= */
	 {3, 3}, {3, 3}, {3, 3},   /* ~=, >, >= */
	 {2, 2}, {1, 1}            /* and, or */
};

#define UNARY_PRIORITY	12  /* priority for unary operators */


/*
** subexpr -> (simpleexp | unop subexpr) { binop subexpr }
** where 'binop' is any binary operator with a priority higher than 'limit'
*/
/*static*/ BinOpr LexState::subexpr (/*LexState *ls,*/ expdesc *v, int limit) {
	BinOpr op;
	UnOpr uop;
	enterlevel();
	uop = getunopr(t.token);
	if (uop != OPR_NOUNOPR) {
		int line = linenumber;
		luaX_next();
		subexpr(v, UNARY_PRIORITY);
		fs->luaK_prefix(uop, v, line);
	}
	else simpleexp(v);
	/* expand while operators have priorities higher than 'limit' */
	op = getbinopr(t.token);
	while (op != OPR_NOBINOPR && priority[op].left > limit) {
		expdesc v2;
		BinOpr nextop;
		int line = linenumber;
		luaX_next();
		fs->luaK_infix(op, v);
		/* read sub-expression with higher priority */
		nextop = subexpr(&v2, priority[op].right);
		fs->luaK_posfix(op, v, &v2, line);
		op = nextop;
	}
	leavelevel();
	return op;  /* return first untreated operator */
}


/*static*/ void LexState::expr (/*LexState *ls,*/ expdesc *v) {
	subexpr(v, 0);
}

/* }==================================================================== */



/*
** {======================================================================
** Rules for Statements
** =======================================================================
*/


/*static*/ void LexState::block (/*LexState *ls*/) {
	/* block -> statlist */
	//FuncState *fs = ls->fs;
	BlockCnt bl;
	fs->enterblock(&bl, 0);
	statlist();
	fs->leaveblock();
}


/*
** structure to chain all variables in the left-hand side of an
** assignment
*/
class LHS_assign {
//struct LHS_assign {
public:
	/*struct*/ LHS_assign *prev;
	expdesc v;  /* variable (global, local, upvalue, or indexed) */
};


/*
** check whether, in an assignment to an upvalue/local variable, the
** upvalue/local variable is begin used in a previous assignment to a
** table. If so, save original upvalue/local value in a safe place and
** use this safe copy in the previous assignment.
*/
/*static*/ void LexState::check_conflict (/*LexState *ls,*/ /*struct*/ LHS_assign *lh, expdesc *v) {
	//FuncState *fs = ls->fs;
	int extra = fs->free_reg;  /* eventual position to save local variable */
	int conflict = 0;
	for (; lh; lh = lh->prev) {  /* check all previous assignments */
		if (lh->v.k == VINDEXED) {  /* assigning to a table? */
			/* table is the upvalue/local being assigned now? */
			if (lh->v.u.ind.vt == v->k && lh->v.u.ind.t == v->u.info) {
				conflict = 1;
				lh->v.u.ind.vt = VLOCAL;
				lh->v.u.ind.t = extra;  /* previous assignment will use safe copy */
			}
			/* index is the local being assigned? (index cannot be upvalue) */
			if (v->k == VLOCAL && lh->v.u.ind.idx == v->u.info) {
				conflict = 1;
				lh->v.u.ind.idx = extra;  /* previous assignment will use safe copy */
			}
		}
	}
	if (conflict) {
		/* copy upvalue/local value to a temporary (in position 'extra') */
		OpCode op = (v->k == VLOCAL) ? OP_MOVE : OP_GETUPVAL;
		fs->luaK_codeABC(op, extra, v->u.info, 0);
		fs->luaK_reserveregs(1);
	}
}


/*static*/ void LexState::assignment (/*LexState *ls,*/ /*struct*/ LHS_assign *lh, int nvars) {
	expdesc e;
	check_condition(vkisvar(lh->v.k), "syntax error");
	if (testnext(',')) {  /* assignment -> ',' suffixedexp assignment */
		/*struct*/ LHS_assign nv;
		nv.prev = lh;
		suffixedexp(&nv.v);
		if (nv.v.k != VINDEXED)
			check_conflict(lh, &nv.v);
		fs->checklimit(nvars + L->nCcalls, LUAI_MAXCCALLS,
										"C levels");
		assignment(&nv, nvars+1);
	}
	else {  /* assignment -> '=' explist */
		int nexps;
		checknext('=');
		nexps = explist(&e);
		if (nexps != nvars) {
			adjust_assign(nvars, nexps, &e);
			if (nexps > nvars)
				fs->free_reg -= nexps - nvars;  /* remove extra values */
		}
		else {
			fs->luaK_setoneret(&e);  /* close last expression */
			fs->luaK_storevar(&lh->v, &e);
			return;  /* avoid default */
		}
	}
	init_exp(&e, VNONRELOC, fs->free_reg-1);  /* default assignment */
	fs->luaK_storevar(&lh->v, &e);
}


/*static*/ int LexState::cond (/*LexState *ls*/) {
	/* cond -> exp */
	expdesc v;
	expr(&v);  /* read condition */
	if (v.k == VNIL) v.k = VFALSE;  /* 'falses' are all equal here */
	fs->luaK_goiftrue(&v);
	return v.f;
}


/*static*/ void LexState::gotostat (/*LexState *ls,*/ int pc) {
	int line = linenumber;
	TString *label;
	int g;
	if (testnext(TK_GOTO))
		label = str_checkname();
	else {
		luaX_next();  /* skip break */
		label = luaS_new(L, "break");
	}
	g = newlabelentry(&dyd->gt, label, line, pc);
	findlabel(g);  /* close it if label already defined */
}


/* check for repeated labels on the same block */
static void checkrepeated (FuncState *fs, Labellist *ll, TString *label) {
	int i;
	for (i = fs->bl->firstlabel; i < ll->n; i++) {
		if (eqstr(label, ll->arr[i].name)) {
			const char *msg = luaO_pushfstring(fs->ls->L,
													"label '%s' already defined on line %d",
													getstr(label), ll->arr[i].line);
			fs->ls->semerror(msg);
		}
	}
}


/* skip no-op statements */
/*static*/ void LexState::skipnoopstat (/*LexState *ls*/) {
	while (t.token == ';' || t.token == TK_DBCOLON)
		statement();
}


/*static*/ void LexState::labelstat (/*LexState *ls,*/ TString *label, int line) {
	/* label -> '::' NAME '::' */
	//FuncState *fs = ls->fs;
	Labellist *ll = &dyd->label;
	int l;  /* index of new label being created */
	checkrepeated(fs, ll, label);  /* check for repeated labels */
	checknext(TK_DBCOLON);  /* skip double colon */
	/* create new entry for this label */
	l = newlabelentry(ll, label, line, fs->pc);
	skipnoopstat();  /* skip other no-op statements */
	if (block_follow(0)) {  /* label is last no-op statement in the block? */
		/* assume that locals are already out of scope */
		ll->arr[l].nactvar = fs->bl->nactvar;
	}
	findgotos(&ll->arr[l]);
}


/*static*/ void LexState::whilestat (/*LexState *ls,*/ int line) {
	/* whilestat -> WHILE cond DO block END */
	//FuncState *fs = ls->fs;
	int whileinit;
	int condexit;
	BlockCnt bl;
	luaX_next();  /* skip WHILE */
	whileinit = fs->luaK_getlabel();
	condexit = cond();
	fs->enterblock(&bl, 1);
	checknext(TK_DO);
	block();
	fs->luaK_jumpto(whileinit);
	check_match(TK_END, TK_WHILE, line);
	fs->leaveblock();
	fs->luaK_patchtohere(condexit);  /* false conditions finish the loop */
}


/*static*/ void LexState::repeatstat (/*LexState *ls,*/ int line) {
	/* repeatstat -> REPEAT block UNTIL cond */
	int condexit;
	//FuncState *fs = fs;
	int repeat_init = fs->luaK_getlabel();
	BlockCnt bl1, bl2;
	fs->enterblock(&bl1, 1);  /* loop block */
	fs->enterblock(&bl2, 0);  /* scope block */
	luaX_next();  /* skip REPEAT */
	statlist();
	check_match(TK_UNTIL, TK_REPEAT, line);
	condexit = cond();  /* read condition (inside scope block) */
	if (bl2.upval)  /* upvalues? */
		fs->luaK_patchclose(condexit, bl2.nactvar);
	fs->leaveblock();  /* finish scope */
	fs->luaK_patchlist(condexit, repeat_init);  /* close the loop */
	fs->leaveblock();  /* finish loop */
}


/*static*/ int LexState::exp1 (/*LexState *ls*/) {
	expdesc e;
	int reg;
	expr(&e);
	fs->luaK_exp2nextreg(&e);
	lua_assert(e.k == VNONRELOC);
	reg = e.u.info;
	return reg;
}


/*static*/ void LexState::forbody (/*LexState *ls,*/ int base, int line, int nvars, int isnum) {
	/* forbody -> DO block */
	BlockCnt bl;
	//FuncState *fs = fs;
	int prep, endfor;
	adjustlocalvars(3);  /* control variables */
	checknext(TK_DO);
	prep = isnum ? fs->luaK_codeAsBx(OP_FORPREP, base, NO_JUMP) : fs->luaK_jump();
	fs->enterblock(&bl, 0);  /* scope for declared variables */
	adjustlocalvars(nvars);
	fs->luaK_reserveregs(nvars);
	block();
	fs->leaveblock();  /* end of scope for declared variables */
	fs->luaK_patchtohere(prep);
	if (isnum)  /* numeric for? */
		endfor = fs->luaK_codeAsBx(OP_FORLOOP, base, NO_JUMP);
	else {  /* generic for */
		fs->luaK_codeABC(OP_TFORCALL, base, 0, nvars);
		fs->luaK_fixline(line);
		endfor = fs->luaK_codeAsBx(OP_TFORLOOP, base + 2, NO_JUMP);
	}
	fs->luaK_patchlist(endfor, prep + 1);
	fs->luaK_fixline(line);
}


/*static*/ void LexState::fornum (/*LexState *ls,*/ TString *varname, int line) {
	/* fornum -> NAME = exp1,exp1[,exp1] forbody */
	//FuncState *fs = fs;
	int base = fs->free_reg;
	new_localvarliteral("(for index)");
	new_localvarliteral("(for limit)");
	new_localvarliteral("(for step)");
	new_localvar(varname);
	checknext('=');
	exp1();  /* initial value */
	checknext(',');
	exp1();  /* limit */
	if (testnext(','))
		exp1();  /* optional step */
	else {  /* default step = 1 */
		fs->luaK_codek(fs->free_reg, fs->luaK_intK(1));
		fs->luaK_reserveregs(1);
	}
	forbody(base, line, 1, 1);
}


/*static*/ void LexState::forlist (/*LexState *ls,*/ TString *indexname) {
	/* forlist -> NAME {,NAME} IN explist forbody */
	//FuncState *fs = fs;
	expdesc e;
	int nvars = 4;  /* gen, state, control, plus at least one declared var */
	int line;
	int base = fs->free_reg;
	/* create control variables */
	new_localvarliteral("(for generator)");
	new_localvarliteral("(for state)");
	new_localvarliteral("(for control)");
	/* create declared variables */
	new_localvar(indexname);
	while (testnext(',')) {
		new_localvar(str_checkname());
		nvars++;
	}
	checknext(TK_IN);
	line = linenumber;
	adjust_assign(3, explist(&e), &e);
	fs->luaK_checkstack(3);  /* extra space to call generator */
	forbody(base, line, nvars - 3, 0);
}


/*static*/ void LexState::forstat (/*LexState *ls,*/ int line) {
	/* forstat -> FOR (fornum | forlist) END */
	//FuncState *fs = fs;
	TString *varname;
	BlockCnt bl;
	fs->enterblock(&bl, 1);  /* scope for loop and control variables */
	luaX_next();  /* skip 'for' */
	varname = str_checkname();  /* first variable name */
	switch (t.token) {
		case '=': fornum(varname, line); break;
		case ',': case TK_IN: forlist(varname); break;
		default: luaX_syntaxerror( "'=' or 'in' expected");
	}
	check_match(TK_END, TK_FOR, line);
	fs->leaveblock();  /* loop scope ('break' jumps to this point) */
}


/*static*/ void LexState::test_then_block (/*LexState *ls,*/ int *escapelist) {
	/* test_then_block -> [IF | ELSEIF] cond THEN block */
	BlockCnt bl;
	//FuncState *fs = fs;
	expdesc v;
	int jf;  /* instruction to skip 'then' code (if condition is false) */
	luaX_next();  /* skip IF or ELSEIF */
	expr(&v);  /* read condition */
	checknext(TK_THEN);
	if (t.token == TK_GOTO || t.token == TK_BREAK) {
		fs->luaK_goiffalse(&v);  /* will jump to label if condition is true */
		fs->enterblock(&bl, 0);  /* must enter block before 'goto' */
		gotostat(v.t);  /* handle goto/break */
		skipnoopstat();  /* skip other no-op statements */
		if (block_follow(0)) {  /* 'goto' is the entire block? */
			fs->leaveblock();
			return;  /* and that is it */
		}
		else  /* must skip over 'then' part if condition is false */
			jf = fs->luaK_jump();
	}
	else {  /* regular case (not goto/break) */
		fs->luaK_goiftrue(&v);  /* skip over block if condition is false */
		fs->enterblock(&bl, 0);
		jf = v.f;
	}
	statlist();  /* 'then' part */
	fs->leaveblock();
	if (t.token == TK_ELSE ||
			t.token == TK_ELSEIF)  /* followed by 'else'/'elseif'? */
		fs->luaK_concat(escapelist, fs->luaK_jump());  /* must jump over it */
	fs->luaK_patchtohere(jf);
}


/*static*/ void LexState::ifstat (/*LexState *ls,*/ int line) {
	/* ifstat -> IF cond THEN block {ELSEIF cond THEN block} [ELSE block] END */
	//FuncState *fs = fs;
	int escapelist = NO_JUMP;  /* exit list for finished parts */
	test_then_block(&escapelist);  /* IF cond THEN block */
	while (t.token == TK_ELSEIF)
		test_then_block(&escapelist);  /* ELSEIF cond THEN block */
	if (testnext(TK_ELSE))
		block();  /* 'else' part */
	check_match(TK_END, TK_IF, line);
	fs->luaK_patchtohere(escapelist);  /* patch escape list to 'if' end */
}


/*static*/ void LexState::localfunc (/*LexState *ls*/) {
	expdesc b;
	//FuncState *fs = fs;
	new_localvar(str_checkname());  /* new local variable */
	adjustlocalvars(1);  /* enter its scope */
	body(&b, 0, linenumber);  /* function created in next register */
	/* debug information will only see the variable after this point! */
	fs->getlocvar(b.u.info)->startpc = fs->pc;
}


/*static*/ void LexState::localstat (/*LexState *ls*/) {
	/* stat -> LOCAL NAME {',' NAME} ['=' explist] */
	int nvars = 0;
	int nexps;
	expdesc e;
	do {
		new_localvar(str_checkname());
		nvars++;
	} while (testnext(','));
	if (testnext('='))
		nexps = explist(&e);
	else {
		e.k = VVOID;
		nexps = 0;
	}
	adjust_assign(nvars, nexps, &e);
	adjustlocalvars(nvars);
}


/*static*/ int LexState::funcname (/*LexState *ls,*/ expdesc *v) {
	/* funcname -> NAME {fieldsel} [':' NAME] */
	int ismethod = 0;
	singlevar(v);
	while (t.token == '.')
		fieldsel(v);
	if (t.token == ':') {
		ismethod = 1;
		fieldsel(v);
	}
	return ismethod;
}


/*static*/ void LexState::funcstat (/*LexState *ls,*/ int line) {
	/* funcstat -> FUNCTION funcname body */
	int ismethod;
	expdesc v, b;
	luaX_next();  /* skip FUNCTION */
	ismethod = funcname(&v);
	body(&b, ismethod, line);
	fs->luaK_storevar(&v, &b);
	fs->luaK_fixline(line);  /* definition "happens" in the first line */
}


/*static*/ void LexState::exprstat (/*LexState *ls*/) {
	/* stat -> func | assignment */
	//FuncState *fs = fs;
	/*struct*/ LHS_assign v;
	suffixedexp(&v.v);
	if (t.token == '=' || t.token == ',') { /* stat -> assignment ? */
		v.prev = NULL;
		assignment(&v, 1);
	}
	else {  /* stat -> func */
		check_condition(v.v.k == VCALL, "syntax error");
		SETARG_C(getcode(fs, &v.v), 1);  /* call statement uses no results */
	}
}


/*static*/ void LexState::retstat (/*LexState *ls*/) {
	/* stat -> RETURN [explist] [';'] */
	//FuncState *fs = fs;
	expdesc e;
	int first, nret;  /* registers with returned values */
	if (block_follow(1) || t.token == ';')
		first = nret = 0;  /* return no values */
	else {
		nret = explist(&e);  /* optional return values */
		if (hasmultret(e.k)) {
			fs->luaK_setmultret(&e);
			if (e.k == VCALL && nret == 1) {  /* tail call? */
				SET_OPCODE(getcode(fs, &e), OP_TAILCALL);
				lua_assert(GETARG_A(getcode(fs, &e)) == fs->nactvar);
			}
			first = fs->nactvar;
			nret = LUA_MULTRET;  /* return all values */
		}
		else {
			if (nret == 1)  /* only one single value? */
				first = fs->luaK_exp2anyreg(&e);
			else {
				fs->luaK_exp2nextreg(&e);  /* values must go to the stack */
				first = fs->nactvar;  /* return all active values */
				lua_assert(nret == fs->free_reg - first);
			}
		}
	}
	fs->luaK_ret(first, nret);
	testnext(';');  /* skip optional semicolon */
}


/*static*/ void LexState::statement (/*LexState *ls*/) {
	int line = linenumber;  /* may be needed for error messages */
	enterlevel();
	switch (t.token) {
		case ';': {  /* stat -> ';' (empty statement) */
			luaX_next();  /* skip ';' */
			break;
		}
		case TK_IF: {  /* stat -> ifstat */
			ifstat(line);
			break;
		}
		case TK_WHILE: {  /* stat -> whilestat */
			whilestat(line);
			break;
		}
		case TK_DO: {  /* stat -> DO block END */
			luaX_next();  /* skip DO */
			block();
			check_match(TK_END, TK_DO, line);
			break;
		}
		case TK_FOR: {  /* stat -> forstat */
			forstat(line);
			break;
		}
		case TK_REPEAT: {  /* stat -> repeatstat */
			repeatstat(line);
			break;
		}
		case TK_FUNCTION: {  /* stat -> funcstat */
			funcstat(line);
			break;
		}
		case TK_LOCAL: {  /* stat -> localstat */
			luaX_next();  /* skip LOCAL */
			if (testnext(TK_FUNCTION))  /* local function? */
				localfunc();
			else
				localstat();
			break;
		}
		case TK_DBCOLON: {  /* stat -> label */
			luaX_next();  /* skip double colon */
			labelstat(str_checkname(), line);
			break;
		}
		case TK_RETURN: {  /* stat -> retstat */
			luaX_next();  /* skip RETURN */
			retstat();
			break;
		}
		case TK_BREAK:   /* stat -> breakstat */
		case TK_GOTO: {  /* stat -> 'goto' NAME */
			gotostat(fs->luaK_jump());
			break;
		}
		default: {  /* stat -> func | assignment */
			exprstat();
			break;
		}
	}
	lua_assert(fs->f->maxstacksize >= fs->free_reg && fs->free_reg >= fs->nactvar);
	fs->free_reg = fs->nactvar;  /* free registers */
	leavelevel();
}

/* }====================================================================== */


/*
** compiles the main function, which is a regular vararg function with an
** upvalue named LUA_ENV
*/
/*static*/ void LexState::mainfunc (/*LexState *ls,*/ FuncState *fs) {
	BlockCnt bl;
	expdesc v;
	open_func(fs, &bl);
	fs->f->is_vararg = 1;  /* main function is always vararg */
	init_exp(&v, VLOCAL, 0);  /* create and... */
	fs->newupvalue(envn, &v);  /* ...set environment upvalue */
	luaX_next();  /* read first token */
	statlist();  /* parse main body */
	check(TK_EOS);
	close_func();
}


LClosure *luaY_parser (lua_State *L, ZIO *z, Mbuffer *buff, Dyndata *dyd, const char *name, int firstchar) {
	LexState lexstate;
	FuncState funcstate;
	LClosure *cl = luaF_newLclosure(L, 1);  /* create main closure */
	setclLvalue(L, L->top, cl);  /* anchor it (to avoid being collected) */
	incr_top(L);
	lexstate.h = luaH_new(L);  /* create table for scanner */
	sethvalue(L, L->top, lexstate.h);  /* anchor it */
	incr_top(L);
	funcstate.f = cl->p = luaF_newproto(L);
	funcstate.f->source = luaS_new(L, name);  /* create and anchor TString */
	lua_assert(iswhite(funcstate.f));  /* do not need barrier here */
	lexstate.buff = buff;
	lexstate.dyd = dyd;
	dyd->actvar.n = dyd->gt.n = dyd->label.n = 0;
	luaX_setinput(L, &lexstate, z, funcstate.f->source, firstchar);
	lexstate.mainfunc(&funcstate);
	lua_assert(!funcstate.prev && funcstate.nups == 1 && !lexstate.fs);
	/* all scopes should be correctly finished */
	lua_assert(dyd->actvar.n == 0 && dyd->gt.n == 0 && dyd->label.n == 0);
	L->top--;  /* remove scanner's table */
	return cl;  /* closure is on the stack, too */
}

