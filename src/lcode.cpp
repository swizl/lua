/*
** $Id: lcode.c,v 2.101 2015/04/29 18:24:11 roberto Exp $
** Code generator for Lua
** See Copyright Notice in lua.h
*/

#define lcode_c
#define LUA_CORE

#include "lprefix.h"


#include <math.h>
#include <stdlib.h>

#include "lua.h"

#include "lcode.h"
#include "ldebug.h"
#include "ldo.h"
#include "lgc.h"
#include "llex.h"
#include "lmem.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lparser.h"
#include "lstring.h"
#include "ltable.h"
#include "lvm.h"


/* Maximum number of registers in a Lua function (must fit in 8 bits) */
#define MAXREGS		255


#define hasjumps(e)	((e)->t != (e)->f)


static int tonumeral(expdesc *e, TValue *v) {
  if (e->t != NO_JUMP || e->f != NO_JUMP)
    return 0;  /* not a numeral */
  switch (e->k) {
    case VKINT:
      if (v) setivalue(v, e->u.ival);
      return 1;
    case VKFLT:
      if (v) setfltvalue(v, e->u.nval);
      return 1;
    default: return 0;
  }
}


//#define getcode(fs,e)	((fs)->f->code[(e)->u.info])
//Instruction FuncState::getcode(expdesc *e)
//{
//    return f->code[e->u.info];
//}

int FuncState::luaK_codeAsBx(OpCode o, int A, unsigned int sBx)
{
    return luaK_codeABx(o,A,(sBx)+MAXARG_sBx);
}

void FuncState::luaK_setmultret(expdesc *e)
{
    luaK_setreturns(e, LUA_MULTRET);
}

void FuncState::luaK_jumpto(int t)
{
    luaK_patchlist(luaK_jump(), t);
}

void FuncState::luaK_nil (/*FuncState *fs,*/ int from, int n) {
  Instruction *previous;
  int l = from + n - 1;  /* last register to set nil */
  if (pc > lasttarget) {  /* no jumps to current position? */
    previous = &f->code[pc-1];
    if (GET_OPCODE(*previous) == OP_LOADNIL) {
      int pfrom = GETARG_A(*previous);
      int pl = pfrom + GETARG_B(*previous);
      if ((pfrom <= from && from <= pl + 1) ||
          (from <= pfrom && pfrom <= l + 1)) {  /* can connect both? */
        if (pfrom < from) from = pfrom;  /* from = min(from, pfrom) */
        if (pl > l) l = pl;  /* l = max(l, pl) */
        SETARG_A(*previous, from);
        SETARG_B(*previous, l - from);
        return;
      }
    }  /* else go through */
  }
  luaK_codeABC(OP_LOADNIL, from, n - 1, 0);  /* else no optimization */
}


int FuncState::luaK_jump (/*FuncState *fs*/) {
  int jpc = this->jpc;  /* save list of jumps to here */
  int j;
  this->jpc = NO_JUMP;
  j = luaK_codeAsBx(OP_JMP, 0, NO_JUMP);
  luaK_concat(&j, jpc);  /* keep them on hold */
  return j;
}


void FuncState::luaK_ret (/*FuncState *fs,*/ int first, int nret) {
  luaK_codeABC(OP_RETURN, first, nret+1, 0);
}


/*static*/ int FuncState::condjump (/*FuncState *fs,*/ OpCode op, int A, int B, int C) {
  luaK_codeABC(op, A, B, C);
  return luaK_jump();
}


/*static*/ void FuncState::fixjump (/*FuncState *fs,*/ int pc, int dest) {
  Instruction *jmp = &f->code[pc];
  int offset = dest-(pc+1);
  lua_assert(dest != NO_JUMP);
  if (abs(offset) > MAXARG_sBx)
    ls->luaX_syntaxerror("control structure too long");
  SETARG_sBx(*jmp, offset);
}


/*
** returns current 'pc' and marks it as a jump target (to avoid wrong
** optimizations with consecutive instructions not in the same basic block).
*/
int FuncState::luaK_getlabel (/*FuncState *fs*/) {
  lasttarget = pc;
  return pc;
}


/*static*/ int FuncState::getjump (/*FuncState *fs,*/ int pc) {
  int offset = GETARG_sBx(f->code[pc]);
  if (offset == NO_JUMP)  /* point to itself represents end of list */
    return NO_JUMP;  /* end of list */
  else
    return (pc+1)+offset;  /* turn offset into absolute position */
}


/*static*/ Instruction *FuncState::getjumpcontrol (/*FuncState *fs,*/ int pc) {
  Instruction *pi = &f->code[pc];
  if (pc >= 1 && testTMode(GET_OPCODE(*(pi-1))))
    return pi-1;
  else
    return pi;
}


/*
** check whether list has any jump that do not produce a value
** (or produce an inverted value)
*/
/*static*/ int FuncState::need_value (/*FuncState *fs,*/ int list) {
  for (; list != NO_JUMP; list = getjump(list)) {
    Instruction i = *getjumpcontrol(list);
    if (GET_OPCODE(i) != OP_TESTSET) return 1;
  }
  return 0;  /* not found */
}


/*static*/ int FuncState::patchtestreg (/*FuncState *fs,*/ int node, int reg) {
  Instruction *i = getjumpcontrol(node);
  if (GET_OPCODE(*i) != OP_TESTSET)
    return 0;  /* cannot patch other instructions */
  if (reg != NO_REG && reg != GETARG_B(*i))
    SETARG_A(*i, reg);
  else  /* no register to put value or register already has the value */
    *i = CREATE_ABC(OP_TEST, GETARG_B(*i), 0, GETARG_C(*i));

  return 1;
}


/*static*/ void FuncState::removevalues (/*FuncState *fs,*/ int list) {
  for (; list != NO_JUMP; list = getjump(list))
      patchtestreg(list, NO_REG);
}


void FuncState::patchlistaux (/*FuncState *fs,*/ int list, int vtarget, int reg, int dtarget) {
  while (list != NO_JUMP) {
    int next = getjump(list);
    if (patchtestreg(list, reg))
      fixjump(list, vtarget);
    else
      fixjump(list, dtarget);  /* jump to default target */
    list = next;
  }
}


/*static*/ void FuncState::dischargejpc (/*FuncState *fs*/) {
  patchlistaux(jpc, pc, NO_REG, pc);
  jpc = NO_JUMP;
}


void FuncState::luaK_patchlist (/*FuncState *fs,*/ int list, int target) {
  if (target == pc)
    luaK_patchtohere(list);
  else {
    lua_assert(target < pc);
    patchlistaux(list, target, NO_REG, target);
  }
}


void FuncState::luaK_patchclose (/*FuncState *fs,*/ int list, int level) {
  level++;  /* argument is +1 to reserve 0 as non-op */
  while (list != NO_JUMP) {
    int next = getjump(list);
    lua_assert(GET_OPCODE(f->code[list]) == OP_JMP &&
                (GETARG_A(f->code[list]) == 0 ||
                 GETARG_A(f->code[list]) >= level));
    SETARG_A(f->code[list], level);
    list = next;
  }
}


void FuncState::luaK_patchtohere (/*FuncState *fs,*/ int list) {
  luaK_getlabel();
  luaK_concat(&jpc, list);
}


void FuncState::luaK_concat (/*FuncState *fs,*/ int *l1, int l2) {
  if (l2 == NO_JUMP) return;
  else if (*l1 == NO_JUMP)
    *l1 = l2;
  else {
    int list = *l1;
    int next;
    while ((next = getjump(list)) != NO_JUMP)  /* find last element */
      list = next;
    fixjump(list, l2);
  }
}


/*static*/ int FuncState::luaK_code (/*FuncState *fs,*/ Instruction i) {
  //Proto *f = fs->f;
  dischargejpc();  /* 'pc' will change */
  /* put new instruction in code array */
  luaM_growvector(ls->L, f->code, pc, f->sizecode, Instruction, MAX_INT, "opcodes");
  f->code[pc] = i;
  /* save corresponding line information */
  luaM_growvector(ls->L, f->lineinfo, pc, f->sizelineinfo, int, MAX_INT, "opcodes");
  f->lineinfo[pc] = ls->lastline;
  return pc++;
}


int FuncState::luaK_codeABC (/*FuncState *fs,*/ OpCode o, int a, int b, int c) {
  lua_assert(getOpMode(o) == iABC);
  lua_assert(getBMode(o) != OpArgN || b == 0);
  lua_assert(getCMode(o) != OpArgN || c == 0);
  lua_assert(a <= MAXARG_A && b <= MAXARG_B && c <= MAXARG_C);
  return luaK_code(CREATE_ABC(o, a, b, c));
}


int FuncState::luaK_codeABx (/*FuncState *fs,*/ OpCode o, int a, unsigned int bc) {
  lua_assert(getOpMode(o) == iABx || getOpMode(o) == iAsBx);
  lua_assert(getCMode(o) == OpArgN);
  lua_assert(a <= MAXARG_A && bc <= MAXARG_Bx);
  return luaK_code(CREATE_ABx(o, a, bc));
}


/*static*/ int FuncState::codeextraarg (/*FuncState *fs,*/ int a) {
  lua_assert(a <= MAXARG_Ax);
  return luaK_code(CREATE_Ax(OP_EXTRAARG, a));
}


int FuncState::luaK_codek (/*FuncState *fs,*/ int reg, int k) {
  if (k <= MAXARG_Bx)
    return luaK_codeABx(OP_LOADK, reg, k);
  else {
    int p = luaK_codeABx(OP_LOADKX, reg, 0);
    codeextraarg(k);
    return p;
  }
}


void FuncState::luaK_checkstack (/*FuncState *fs,*/ int n) {
  int newstack = free_reg + n;
  if (newstack > f->maxstacksize) {
    if (newstack >= MAXREGS)
      ls->luaX_syntaxerror(
        "function or expression needs too many registers");
    f->maxstacksize = cast_byte(newstack);
  }
}


void FuncState::luaK_reserveregs (/*FuncState *fs,*/ int n) {
  luaK_checkstack(n);
  free_reg += n;
}


/*static*/ void FuncState::freereg (/*FuncState *fs,*/ int reg) {
  if (!ISK(reg) && reg >= nactvar) {
    free_reg--;
    lua_assert(reg == free_reg);
  }
}


/*static*/ void FuncState::freeexp (/*FuncState *fs,*/ expdesc *e) {
  if (e->k == VNONRELOC)
    freereg(e->u.info);
}


/*
** Use scanner's table to cache position of constants in constant list
** and try to reuse constants
*/
/*static*/ int FuncState::addk (/*FuncState *fs,*/ TValue *key, TValue *v) {
  lua_State *L = ls->L;
  //Proto *f = fs->f;
  TValue *idx = luaH_set(L, ls->h, key);  /* index scanner table */
  int k, oldsize;
  if (ttisinteger(idx)) {  /* is there an index there? */
    k = cast_int(ivalue(idx));
    /* correct value? (warning: must distinguish floats from integers!) */
    if (k < nk && ttype(&f->k[k]) == ttype(v) &&
                      luaV_rawequalobj(&f->k[k], v))
      return k;  /* reuse index */
  }
  /* constant not found; create a new entry */
  oldsize = f->sizek;
  k = nk;
  /* numerical value does not need GC barrier;
     table has no metatable, so it does not need to invalidate cache */
  setivalue(idx, k);
  luaM_growvector(L, f->k, k, f->sizek, TValue, MAXARG_Ax, "constants");
  while (oldsize < f->sizek) setnilvalue(&f->k[oldsize++]);
  setobj(L, &f->k[k], v);
  nk++;
  luaC_barrier(L, f, v);
  return k;
}


int FuncState::luaK_stringK (/*FuncState *fs,*/ TString *s) {
  TValue o;
  setsvalue(ls->L, &o, s);
  return addk(&o, &o);
}


/*
** Integers use userdata as keys to avoid collision with floats with same
** value; conversion to 'void*' used only for hashing, no "precision"
** problems
*/
int FuncState::luaK_intK (/*FuncState *fs,*/ lua_Integer n) {
  TValue k, o;
  setpvalue(&k, cast(void*, cast(size_t, n)));
  setivalue(&o, n);
  return addk(&k, &o);
}


/*static*/ int FuncState::luaK_numberK (/*FuncState *fs,*/ lua_Number r) {
  TValue o;
  setfltvalue(&o, r);
  return addk(&o, &o);
}


/*static*/ int FuncState::boolK (/*FuncState *fs,*/ int b) {
  TValue o;
  setbvalue(&o, b);
  return addk(&o, &o);
}


/*static*/ int FuncState::nilK (/*FuncState *fs*/) {
  TValue k, v;
  setnilvalue(&v);
  /* cannot use nil as key; instead use table itself to represent nil */
  sethvalue(ls->L, &k, ls->h);
  return addk(&k, &v);
}


void FuncState::luaK_setreturns (/*FuncState *fs,*/ expdesc *e, int nresults) {
  if (e->k == VCALL) {  /* expression is an open function call? */
    SETARG_C(getcode(this, e), nresults+1);
  }
  else if (e->k == VVARARG) {
    SETARG_B(getcode(this, e), nresults+1);
    SETARG_A(getcode(this, e), free_reg);
    luaK_reserveregs(1);
  }
}


void FuncState::luaK_setoneret (/*FuncState *fs,*/ expdesc *e) {
  if (e->k == VCALL) {  /* expression is an open function call? */
    e->k = VNONRELOC;
    e->u.info = GETARG_A(getcode(this, e));
  }
  else if (e->k == VVARARG) {
    SETARG_B(getcode(this, e), 2);
    e->k = VRELOCABLE;  /* can relocate its simple result */
  }
}


void FuncState::luaK_dischargevars (/*FuncState *fs,*/ expdesc *e) {
  switch (e->k) {
    case VLOCAL: {
      e->k = VNONRELOC;
      break;
    }
    case VUPVAL: {
      e->u.info = luaK_codeABC(OP_GETUPVAL, 0, e->u.info, 0);
      e->k = VRELOCABLE;
      break;
    }
    case VINDEXED: {
      OpCode op = OP_GETTABUP;  /* assume 't' is in an upvalue */
      freereg(e->u.ind.idx);
      if (e->u.ind.vt == VLOCAL) {  /* 't' is in a register? */
        freereg(e->u.ind.t);
        op = OP_GETTABLE;
      }
      e->u.info = luaK_codeABC(op, 0, e->u.ind.t, e->u.ind.idx);
      e->k = VRELOCABLE;
      break;
    }
    case VVARARG:
    case VCALL: {
      luaK_setoneret(e);
      break;
    }
    default: break;  /* there is one value available (somewhere) */
  }
}


/*static*/ int FuncState::code_label (/*FuncState *fs,*/ int A, int b, int jump) {
  luaK_getlabel();  /* those instructions may be jump targets */
  return luaK_codeABC(OP_LOADBOOL, A, b, jump);
}


/*static*/ void FuncState::discharge2reg (/*FuncState *fs,*/ expdesc *e, int reg) {
  luaK_dischargevars(e);
  switch (e->k) {
    case VNIL: {
      luaK_nil(reg, 1);
      break;
    }
    case VFALSE: case VTRUE: {
      luaK_codeABC(OP_LOADBOOL, reg, e->k == VTRUE, 0);
      break;
    }
    case VK: {
      luaK_codek(reg, e->u.info);
      break;
    }
    case VKFLT: {
      luaK_codek(reg, luaK_numberK(e->u.nval));
      break;
    }
    case VKINT: {
      luaK_codek(reg, luaK_intK(e->u.ival));
      break;
    }
    case VRELOCABLE: {
      Instruction *pc = &getcode(this, e);
      SETARG_A(*pc, reg);
      break;
    }
    case VNONRELOC: {
      if (reg != e->u.info)
        luaK_codeABC(OP_MOVE, reg, e->u.info, 0);
      break;
    }
    default: {
      lua_assert(e->k == VVOID || e->k == VJMP);
      return;  /* nothing to do... */
    }
  }
  e->u.info = reg;
  e->k = VNONRELOC;
}


/*static*/ void FuncState::discharge2anyreg (/*FuncState *fs,*/ expdesc *e) {
  if (e->k != VNONRELOC) {
    luaK_reserveregs(1);
    discharge2reg(e, free_reg-1);
  }
}


/*static*/ void FuncState::exp2reg (/*FuncState *fs,*/ expdesc *e, int reg) {
  discharge2reg(e, reg);
  if (e->k == VJMP)
    luaK_concat(&e->t, e->u.info);  /* put this jump in 't' list */
  if (hasjumps(e)) {
    int final;  /* position after whole expression */
    int p_f = NO_JUMP;  /* position of an eventual LOAD false */
    int p_t = NO_JUMP;  /* position of an eventual LOAD true */
    if (need_value(e->t) || need_value(e->f)) {
      int fj = (e->k == VJMP) ? NO_JUMP : luaK_jump();
      p_f = code_label(reg, 0, 1);
      p_t = code_label(reg, 1, 0);
      luaK_patchtohere(fj);
    }
    final = luaK_getlabel();
    patchlistaux(e->f, final, reg, p_f);
    patchlistaux(e->t, final, reg, p_t);
  }
  e->f = e->t = NO_JUMP;
  e->u.info = reg;
  e->k = VNONRELOC;
}


void FuncState::luaK_exp2nextreg (/*FuncState *fs,*/ expdesc *e) {
  luaK_dischargevars(e);
  freeexp(e);
  luaK_reserveregs(1);
  exp2reg(e, free_reg - 1);
}


int FuncState::luaK_exp2anyreg (/*FuncState *fs,*/ expdesc *e) {
  luaK_dischargevars(e);
  if (e->k == VNONRELOC) {
    if (!hasjumps(e)) return e->u.info;  /* exp is already in a register */
    if (e->u.info >= nactvar) {  /* reg. is not a local? */
      exp2reg(e, e->u.info);  /* put value on it */
      return e->u.info;
    }
  }
  luaK_exp2nextreg(e);  /* default */
  return e->u.info;
}


void FuncState::luaK_exp2anyregup (/*FuncState *fs,*/ expdesc *e) {
  if (e->k != VUPVAL || hasjumps(e))
    luaK_exp2anyreg(e);
}


void FuncState::luaK_exp2val (/*FuncState *fs,*/ expdesc *e) {
  if (hasjumps(e))
    luaK_exp2anyreg(e);
  else
    luaK_dischargevars(e);
}


int FuncState::luaK_exp2RK (/*FuncState *fs,*/ expdesc *e) {
  luaK_exp2val(e);
  switch (e->k) {
    case VTRUE:
    case VFALSE:
    case VNIL: {
      if (nk <= MAXINDEXRK) {  /* constant fits in RK operand? */
        e->u.info = (e->k == VNIL) ? nilK() : boolK((e->k == VTRUE));
        e->k = VK;
        return RKASK(e->u.info);
      }
      else break;
    }
    case VKINT: {
      e->u.info = luaK_intK(e->u.ival);
      e->k = VK;
      goto vk;
    }
    case VKFLT: {
      e->u.info = luaK_numberK(e->u.nval);
      e->k = VK;
    }
    /* FALLTHROUGH */
    case VK: {
     vk:
      if (e->u.info <= MAXINDEXRK)  /* constant fits in 'argC'? */
        return RKASK(e->u.info);
      else break;
    }
    default: break;
  }
  /* not a constant in the right range: put it in a register */
  return luaK_exp2anyreg(e);
}


void FuncState::luaK_storevar (/*FuncState *fs,*/ expdesc *var, expdesc *ex) {
  switch (var->k) {
    case VLOCAL: {
      freeexp(ex);
      exp2reg(ex, var->u.info);
      return;
    }
    case VUPVAL: {
      int e = luaK_exp2anyreg(ex);
      luaK_codeABC(OP_SETUPVAL, e, var->u.info, 0);
      break;
    }
    case VINDEXED: {
      OpCode op = (var->u.ind.vt == VLOCAL) ? OP_SETTABLE : OP_SETTABUP;
      int e = luaK_exp2RK(ex);
      luaK_codeABC(op, var->u.ind.t, var->u.ind.idx, e);
      break;
    }
    default: {
      lua_assert(0);  /* invalid var kind to store */
      break;
    }
  }
  freeexp(ex);
}


void FuncState::luaK_self (/*FuncState *fs,*/ expdesc *e, expdesc *key) {
  int ereg;
  luaK_exp2anyreg(e);
  ereg = e->u.info;  /* register where 'e' was placed */
  freeexp(e);
  e->u.info = free_reg;  /* base register for op_self */
  e->k = VNONRELOC;
  luaK_reserveregs(2);  /* function and 'self' produced by op_self */
  luaK_codeABC(OP_SELF, e->u.info, ereg, luaK_exp2RK(key));
  freeexp(key);
}


/*static*/ void FuncState::invertjump (/*FuncState *fs,*/ expdesc *e) {
  Instruction *pc = getjumpcontrol(e->u.info);
  lua_assert(testTMode(GET_OPCODE(*pc)) && GET_OPCODE(*pc) != OP_TESTSET && GET_OPCODE(*pc) != OP_TEST);
  SETARG_A(*pc, !(GETARG_A(*pc)));
}


/*static*/ int FuncState::jumponcond (/*FuncState *fs,*/ expdesc *e, int cond) {
  if (e->k == VRELOCABLE) {
    Instruction ie = getcode(this, e);
    if (GET_OPCODE(ie) == OP_NOT) {
      pc--;  /* remove previous OP_NOT */
      return condjump(OP_TEST, GETARG_B(ie), 0, !cond);
    }
    /* else go through */
  }
  discharge2anyreg(e);
  freeexp(e);
  return condjump(OP_TESTSET, NO_REG, e->u.info, cond);
}


void FuncState::luaK_goiftrue (/*FuncState *fs,*/ expdesc *e) {
  int pc;  /* pc of last jump */
  luaK_dischargevars(e);
  switch (e->k) {
    case VJMP: {
      invertjump(e);
      pc = e->u.info;
      break;
    }
    case VK: case VKFLT: case VKINT: case VTRUE: {
      pc = NO_JUMP;  /* always true; do nothing */
      break;
    }
    default: {
      pc = jumponcond(e, 0);
      break;
    }
  }
  luaK_concat(&e->f, pc);  /* insert last jump in 'f' list */
  luaK_patchtohere(e->t);
  e->t = NO_JUMP;
}


void FuncState::luaK_goiffalse (/*FuncState *fs,*/ expdesc *e) {
  int pc;  /* pc of last jump */
  luaK_dischargevars(e);
  switch (e->k) {
    case VJMP: {
      pc = e->u.info;
      break;
    }
    case VNIL: case VFALSE: {
      pc = NO_JUMP;  /* always false; do nothing */
      break;
    }
    default: {
      pc = jumponcond(e, 1);
      break;
    }
  }
  luaK_concat(&e->t, pc);  /* insert last jump in 't' list */
  luaK_patchtohere(e->f);
  e->f = NO_JUMP;
}


/*static*/ void FuncState::codenot (/*FuncState *fs,*/ expdesc *e) {
  luaK_dischargevars(e);
  switch (e->k) {
    case VNIL: case VFALSE: {
      e->k = VTRUE;
      break;
    }
    case VK: case VKFLT: case VKINT: case VTRUE: {
      e->k = VFALSE;
      break;
    }
    case VJMP: {
      invertjump(e);
      break;
    }
    case VRELOCABLE:
    case VNONRELOC: {
      discharge2anyreg(e);
      freeexp(e);
      e->u.info = luaK_codeABC(OP_NOT, 0, e->u.info, 0);
      e->k = VRELOCABLE;
      break;
    }
    default: {
      lua_assert(0);  /* cannot happen */
      break;
    }
  }
  /* interchange true and false lists */
  { int temp = e->f; e->f = e->t; e->t = temp; }
  removevalues(e->f);
  removevalues(e->t);
}


void FuncState::luaK_indexed (/*FuncState *fs,*/ expdesc *t, expdesc *k) {
  lua_assert(!hasjumps(t));
  t->u.ind.t = t->u.info;
  t->u.ind.idx = luaK_exp2RK(k);
  t->u.ind.vt = (t->k == VUPVAL) ? VUPVAL : check_exp(vkisinreg(t->k), VLOCAL);
  t->k = VINDEXED;
}


/*
** return false if folding can raise an error
*/
static int validop (int op, TValue *v1, TValue *v2) {
  switch (op) {
    case LUA_OPBAND: case LUA_OPBOR: case LUA_OPBXOR:
    case LUA_OPSHL: case LUA_OPSHR: case LUA_OPBNOT: {  /* conversion errors */
      lua_Integer i;
      return (tointeger(v1, &i) && tointeger(v2, &i));
    }
    case LUA_OPDIV: case LUA_OPIDIV: case LUA_OPMOD:  /* division by 0 */
      return (nvalue(v2) != 0);
    default: return 1;  /* everything else is valid */
  }
}


/*
** Try to "constant-fold" an operation; return 1 iff successful
*/
/*static*/ int FuncState::constfolding (/*FuncState *fs,*/ int op, expdesc *e1, expdesc *e2) {
  TValue v1, v2, res;
  if (!tonumeral(e1, &v1) || !tonumeral(e2, &v2) || !validop(op, &v1, &v2))
    return 0;  /* non-numeric operands or not safe to fold */
  luaO_arith(ls->L, op, &v1, &v2, &res);  /* does operation */
  if (ttisinteger(&res)) {
    e1->k = VKINT;
    e1->u.ival = ivalue(&res);
  }
  else {  /* folds neither NaN nor 0.0 (to avoid collapsing with -0.0) */
    lua_Number n = fltvalue(&res);
    if (luai_numisnan(n) || n == 0)
      return 0;
    e1->k = VKFLT;
    e1->u.nval = n;
  }
  return 1;
}


/*
** Code for binary and unary expressions that "produce values"
** (arithmetic operations, bitwise operations, concat, length). First
** try to do constant folding (only for numeric [arithmetic and
** bitwise] operations, which is what 'lua_arith' accepts).
** Expression to produce final result will be encoded in 'e1'.
*/
/*static*/ void FuncState::codeexpval (/*FuncState *fs,*/ OpCode op, expdesc *e1, expdesc *e2, int line) {
  lua_assert(op >= OP_ADD);
  if (op <= OP_BNOT && constfolding((op - OP_ADD) + LUA_OPADD, e1, e2))
    return;  /* result has been folded */
  else {
    int o1, o2;
    /* move operands to registers (if needed) */
    if (op == OP_UNM || op == OP_BNOT || op == OP_LEN) {  /* unary op? */
      o2 = 0;  /* no second expression */
      o1 = luaK_exp2anyreg(e1);  /* cannot operate on constants */
    }
    else {  /* regular case (binary operators) */
      o2 = luaK_exp2RK(e2);  /* both operands are "RK" */
      o1 = luaK_exp2RK(e1);
    }
    if (o1 > o2) {  /* free registers in proper order */
      freeexp(e1);
      freeexp(e2);
    }
    else {
      freeexp(e2);
      freeexp(e1);
    }
    e1->u.info = luaK_codeABC(op, 0, o1, o2);  /* generate opcode */
    e1->k = VRELOCABLE;  /* all those operations are relocable */
    luaK_fixline(line);
  }
}


/*static*/ void FuncState::codecomp (/*FuncState *fs,*/ OpCode op, int cond, expdesc *e1, expdesc *e2) {
  int o1 = luaK_exp2RK(e1);
  int o2 = luaK_exp2RK(e2);
  freeexp(e2);
  freeexp(e1);
  if (cond == 0 && op != OP_EQ) {
    int temp;  /* exchange args to replace by '<' or '<=' */
    temp = o1; o1 = o2; o2 = temp;  /* o1 <==> o2 */
    cond = 1;
  }
  e1->u.info = condjump(op, cond, o1, o2);
  e1->k = VJMP;
}


void FuncState::luaK_prefix (/*FuncState *fs,*/ UnOpr op, expdesc *e, int line) {
  expdesc e2;
  e2.t = e2.f = NO_JUMP; e2.k = VKINT; e2.u.ival = 0;
  switch (op) {
    case OPR_MINUS: case OPR_BNOT: case OPR_LEN: {
      codeexpval(cast(OpCode, (op - OPR_MINUS) + OP_UNM), e, &e2, line);
      break;
    }
    case OPR_NOT: codenot(e); break;
    default: lua_assert(0);
  }
}


void FuncState::luaK_infix (/*FuncState *fs,*/ BinOpr op, expdesc *v) {
  switch (op) {
    case OPR_AND: {
      luaK_goiftrue(v);
      break;
    }
    case OPR_OR: {
      luaK_goiffalse(v);
      break;
    }
    case OPR_CONCAT: {
      luaK_exp2nextreg(v);  /* operand must be on the 'stack' */
      break;
    }
    case OPR_ADD: case OPR_SUB:
    case OPR_MUL: case OPR_DIV: case OPR_IDIV:
    case OPR_MOD: case OPR_POW:
    case OPR_BAND: case OPR_BOR: case OPR_BXOR:
    case OPR_SHL: case OPR_SHR: {
      if (!tonumeral(v, NULL)) luaK_exp2RK(v);
      break;
    }
    default: {
      luaK_exp2RK(v);
      break;
    }
  }
}


void FuncState::luaK_posfix (/*FuncState *fs,*/ BinOpr op, expdesc *e1, expdesc *e2, int line) {
  switch (op) {
    case OPR_AND: {
      lua_assert(e1->t == NO_JUMP);  /* list must be closed */
      luaK_dischargevars(e2);
      luaK_concat(&e2->f, e1->f);
      *e1 = *e2;
      break;
    }
    case OPR_OR: {
      lua_assert(e1->f == NO_JUMP);  /* list must be closed */
      luaK_dischargevars(e2);
      luaK_concat(&e2->t, e1->t);
      *e1 = *e2;
      break;
    }
    case OPR_CONCAT: {
      luaK_exp2val(e2);
      if (e2->k == VRELOCABLE && GET_OPCODE(getcode(this, e2)) == OP_CONCAT) {
        lua_assert(e1->u.info == GETARG_B(getcode(this, e2))-1);
        freeexp(e1);
        SETARG_B(getcode(this, e2), e1->u.info);
        e1->k = VRELOCABLE; e1->u.info = e2->u.info;
      }
      else {
        luaK_exp2nextreg(e2);  /* operand must be on the 'stack' */
        codeexpval(OP_CONCAT, e1, e2, line);
      }
      break;
    }
    case OPR_ADD: case OPR_SUB: case OPR_MUL: case OPR_DIV:
    case OPR_IDIV: case OPR_MOD: case OPR_POW:
    case OPR_BAND: case OPR_BOR: case OPR_BXOR:
    case OPR_SHL: case OPR_SHR: {
      codeexpval(cast(OpCode, (op - OPR_ADD) + OP_ADD), e1, e2, line);
      break;
    }
    case OPR_EQ: case OPR_LT: case OPR_LE: {
      codecomp(cast(OpCode, (op - OPR_EQ) + OP_EQ), 1, e1, e2);
      break;
    }
    case OPR_NE: case OPR_GT: case OPR_GE: {
      codecomp(cast(OpCode, (op - OPR_NE) + OP_EQ), 0, e1, e2);
      break;
    }
    default: lua_assert(0);
  }
}


void FuncState::luaK_fixline (/*FuncState *fs,*/ int line) {
  f->lineinfo[pc - 1] = line;
}


void FuncState::luaK_setlist (/*FuncState *fs,*/ int base, int nelems, int tostore) {
  int c =  (nelems - 1)/LFIELDS_PER_FLUSH + 1;
  int b = (tostore == LUA_MULTRET) ? 0 : tostore;
  lua_assert(tostore != 0);
  if (c <= MAXARG_C)
    luaK_codeABC(OP_SETLIST, base, b, c);
  else if (c <= MAXARG_Ax) {
    luaK_codeABC(OP_SETLIST, base, b, 0);
    codeextraarg(c);
  }
  else
    ls->luaX_syntaxerror( "constructor too long");
  free_reg = base + 1;  /* free registers with list values */
}

