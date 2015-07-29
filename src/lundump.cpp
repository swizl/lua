/*
** $Id: lundump.c,v 2.41 2014/11/02 19:19:04 roberto Exp $
** load precompiled Lua chunks
** See Copyright Notice in lua.h
*/

#define lundump_c
#define LUA_CORE

#include "lprefix.h"


#include <string.h>

#include "lua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstring.h"
#include "lundump.h"
#include "lzio.h"


#if !defined(luai_verifycode)
#define luai_verifycode(L,b,f)  /* empty */
#endif

class LoadState {
//typedef struct {
public:
	lua_State *L;
	ZIO *Z;
	Mbuffer *b;
	const char *name;

	l_noret error(/*LoadState *S,*/ const char *why);


	void LoadBlock (/*LoadState *S,*/ void *b, size_t size);

	lu_byte LoadByte (/*LoadState *S*/);
	int LoadInt (/*LoadState *S*/);
	lua_Number LoadNumber (/*LoadState *S*/);
	lua_Integer LoadInteger (/*LoadState *S*/);
	TString *LoadString (/*LoadState *S*/);
	void LoadCode (/*LoadState *S,*/ Proto *f);
	void LoadConstants (/*LoadState *S,*/ Proto *f);
	void LoadProtos (/*LoadState *S,*/ Proto *f);
	void LoadUpvalues (/*LoadState *S,*/ Proto *f);
	void LoadDebug (/*LoadState *S,*/ Proto *f);
	void LoadFunction (/*LoadState *S,*/ Proto *f, TString *psource);
	void checkliteral (/*LoadState *S,*/ const char *s, const char *msg);
	void fchecksize (/*LoadState *S,*/ size_t size, const char *tname);
	void checkHeader (/*LoadState *S*/);


} ;//LoadState;


/*static*/ l_noret LoadState::error(/*LoadState *S,*/ const char *why) {
	luaO_pushfstring(L, "%s: %s precompiled chunk", name, why);
	luaD_throw(L, LUA_ERRSYNTAX);
}


/*
** All high-level loads go through LoadVector; you can change it to
** adapt to the endianness of the input
*/
//#define LoadVector(S,b,n)	LoadBlock(S,b,(n)*sizeof((b)[0]))
#define LoadVector(b,n)	LoadBlock(b,(n)*sizeof((b)[0]))

/*static*/ void LoadState::LoadBlock (/*LoadState *S,*/ void *b, size_t size) {
	if (Z->luaZ_read(b, size) != 0)
		error("truncated");
}


//#define LoadVar(S,x)		LoadVector(S,&x,1)
#define LoadVar(x)		LoadVector(&x,1)


/*static*/ lu_byte LoadState::LoadByte (/*LoadState *S*/) {
	lu_byte x;
	LoadVar(x);
	return x;
}


/*static*/ int LoadState::LoadInt (/*LoadState *S*/) {
	int x;
	LoadVar(x);
	return x;
}


/*static*/ lua_Number LoadState::LoadNumber (/*LoadState *S*/) {
	lua_Number x;
	LoadVar(x);
	return x;
}


/*static*/ lua_Integer LoadState::LoadInteger (/*LoadState *S*/) {
	lua_Integer x;
	LoadVar(x);
	return x;
}


/*static*/ TString *LoadState::LoadString (/*LoadState *S*/) {
	size_t size = LoadByte();
	if (size == 0xFF)
		LoadVar(size);
	if (size == 0)
		return NULL;
	else {
		char *s = L->luaZ_openspace(b, --size);
		LoadVector(s, size);
		return luaS_newlstr(L, s, size);
	}
}


/*static*/ void LoadState::LoadCode (/*LoadState *S,*/ Proto *f) {
	int n = LoadInt();
	f->code = luaM_newvector(L, n, Instruction);
	f->sizecode = n;
	LoadVector(f->code, n);
}


//static void LoadFunction(LoadState *S, Proto *f, TString *psource);


/*static*/ void LoadState::LoadConstants (/*LoadState *S,*/ Proto *f) {
	int i;
	int n = LoadInt();
	f->k = luaM_newvector(L, n, TValue);
	f->sizek = n;
	for (i = 0; i < n; i++)
		setnilvalue(&f->k[i]);
	for (i = 0; i < n; i++) {
		TValue *o = &f->k[i];
		int t = LoadByte();
		switch (t) {
		case LUA_TNIL:
			setnilvalue(o);
			break;
		case LUA_TBOOLEAN:
			setbvalue(o, LoadByte());
			break;
		case LUA_TNUMFLT:
			setfltvalue(o, LoadNumber());
			break;
		case LUA_TNUMINT:
			setivalue(o, LoadInteger());
			break;
		case LUA_TSHRSTR:
		case LUA_TLNGSTR:
			setsvalue2n(L, o, LoadString());
			break;
		default:
			lua_assert(0);
		}
	}
}


/*static*/ void LoadState::LoadProtos (/*LoadState *S,*/ Proto *f) {
	int i;
	int n = LoadInt();
	f->p = luaM_newvector(L, n, Proto *);
	f->sizep = n;
	for (i = 0; i < n; i++)
		f->p[i] = NULL;
	for (i = 0; i < n; i++) {
		f->p[i] = luaF_newproto(L);
		LoadFunction(f->p[i], f->source);
	}
}


/*static*/ void LoadState::LoadUpvalues (/*LoadState *S,*/ Proto *f) {
	int i, n;
	n = LoadInt();
	f->upvalues = luaM_newvector(L, n, Upvaldesc);
	f->sizeupvalues = n;
	for (i = 0; i < n; i++)
		f->upvalues[i].name = NULL;
	for (i = 0; i < n; i++) {
		f->upvalues[i].instack = LoadByte();
		f->upvalues[i].idx = LoadByte();
	}
}


/*static*/ void LoadState::LoadDebug (/*LoadState *S,*/ Proto *f) {
	int i, n;
	n = LoadInt();
	f->lineinfo = luaM_newvector(L, n, int);
	f->sizelineinfo = n;
	LoadVector(f->lineinfo, n);
	n = LoadInt();
	f->locvars = luaM_newvector(L, n, LocVar);
	f->sizelocvars = n;
	for (i = 0; i < n; i++)
		f->locvars[i].varname = NULL;
	for (i = 0; i < n; i++) {
		f->locvars[i].varname = LoadString();
		f->locvars[i].startpc = LoadInt();
		f->locvars[i].endpc = LoadInt();
	}
	n = LoadInt();
	for (i = 0; i < n; i++)
		f->upvalues[i].name = LoadString();
}


/*static*/ void LoadState::LoadFunction (/*LoadState *S,*/ Proto *f, TString *psource) {
	f->source = LoadString();
	if (f->source == NULL)  /* no source in dump? */
		f->source = psource;  /* reuse parent's source */
	f->linedefined = LoadInt();
	f->lastlinedefined = LoadInt();
	f->numparams = LoadByte();
	f->is_vararg = LoadByte();
	f->maxstacksize = LoadByte();
	LoadCode(f);
	LoadConstants(f);
	LoadUpvalues(f);
	LoadProtos(f);
	LoadDebug(f);
}


/*static*/ void LoadState::checkliteral (/*LoadState *S,*/ const char *s, const char *msg) {
	char buff[sizeof(LUA_SIGNATURE) + sizeof(LUAC_DATA)]; /* larger than both */
	size_t len = strlen(s);
	LoadVector(buff, len);
	if (memcmp(s, buff, len) != 0)
		error(msg);
}


/*static*/ void LoadState::fchecksize (/*LoadState *S,*/ size_t size, const char *tname) {
	if (LoadByte() != size)
		error(luaO_pushfstring(L, "%s size mismatch in", tname));
}


#define checksize(t)	fchecksize(sizeof(t),#t)

/*static*/ void LoadState::checkHeader (/*LoadState *S*/) {
	checkliteral(LUA_SIGNATURE + 1, "not a");  /* 1st char already checked */
	if (LoadByte() != LUAC_VERSION)
		error("version mismatch in");
	if (LoadByte() != LUAC_FORMAT)
		error("format mismatch in");
	checkliteral(LUAC_DATA, "corrupted");
	checksize(int);
	checksize(size_t);
	checksize(Instruction);
	checksize(lua_Integer);
	checksize(lua_Number);
	if (LoadInteger() != LUAC_INT)
		error("endianness mismatch in");
	if (LoadNumber() != LUAC_NUM)
		error("float format mismatch in");
}


/*
** load precompiled chunk
*/
LClosure *luaU_undump(lua_State *L, ZIO *Z, Mbuffer *buff, const char *name) {
	LoadState S;
	LClosure *cl;
	if (*name == '@' || *name == '=')
		S.name = name + 1;
	else if (*name == LUA_SIGNATURE[0])
		S.name = "binary string";
	else
		S.name = name;
	S.L = L;
	S.Z = Z;
	S.b = buff;
	S.checkHeader();
	cl = luaF_newLclosure(L, S.LoadByte());
	setclLvalue(L, L->top, cl);
	incr_top(L);
	cl->p = luaF_newproto(L);
	S.LoadFunction(cl->p, NULL);
	lua_assert(cl->nupvalues == cl->p->sizeupvalues);
	luai_verifycode(L, buff, cl->p);
	return cl;
}

