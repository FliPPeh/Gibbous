/*
 * Tiny readline module for Lua.
 *
 * To compile on systems where libreadline is available, use...
 *
 *   cc readline.c -fpic -shared -o readline.so
 *
 * ...and place the resulting readline.so somewhere where Lua can find it.
 */
#include <stdlib.h>

#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

#include <readline/readline.h>
#include <readline/history.h>

static int l_readline(lua_State *l)
{
	char const *p = luaL_checkstring(l, 1);

	char *line = readline(p);
	lua_pushstring(l, line);

	free(line);
	return 1;
}

static int l_add_history(lua_State *l)
{
	char const *line = luaL_checkstring(l, 1);

	add_history(line);
	return 0;
}

static struct luaL_Reg const l_readline_functions[] = {
	{ "readline", l_readline },
	{ "add_history", l_add_history },
	{ NULL,       NULL }
};

int luaopen_readline(lua_State *l)
{
	lua_newtable(l);
	luaL_setfuncs(l, l_readline_functions, 0);

	return 1;
}
