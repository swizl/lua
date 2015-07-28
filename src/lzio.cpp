/*
** $Id: lzio.c,v 1.36 2014/11/02 19:19:04 roberto Exp $
** Buffered streams
** See Copyright Notice in lua.h
*/

#define lzio_c
#define LUA_CORE

#include "lprefix.h"


#include <string.h>

#include "lua.h"

#include "llimits.h"
#include "lmem.h"
#include "lstate.h"
#include "lzio.h"


int Zio::luaZ_fill (/*ZIO *z*/) {
	size_t size;
	//lua_State *L = L;
	const char *buff;
	lua_unlock(L);
	buff = reader(L, data, &size);
	lua_lock(L);
	if (buff == NULL || size == 0)
		return EOZ;
	n = size - 1;  /* discount char being returned */
	p = buff;
	return cast_uchar(*(p++));
}


void luaZ_init (lua_State *L, ZIO *z, lua_Reader reader, void *data) {
	z->L = L;
	z->reader = reader;
	z->data = data;
	z->n = 0;
	z->p = NULL;
}


/* --------------------------------------------------------------- read --- */
size_t Zio::luaZ_read (/*ZIO *z,*/ void *b, size_t n) {
	while (n) {
		size_t m;
		if (n == 0) {  /* no bytes in buffer? */
			if (luaZ_fill() == EOZ)  /* try to read more */
				return n;  /* no more input; return number of missing bytes */
			else {
				n++;  /* luaZ_fill consumed first byte; put it back */
				p--;
			}
		}
		m = (n <= n) ? n : n;  /* min. between n and z->n */
		memcpy(b, p, m);
		n -= m;
		p += m;
		b = (char *)b + m;
		n -= m;
	}
	return 0;
}

/* ------------------------------------------------------------------------ */
char *luaZ_openspace (lua_State *L, Mbuffer *buff, size_t n) {
	if (n > buff->buffsize) {
		if (n < LUA_MINBUFFER) n = LUA_MINBUFFER;
		luaZ_resizebuffer(L, buff, n);
	}
	return buff->buffer;
}


