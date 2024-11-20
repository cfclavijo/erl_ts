#ifndef ERL_TS_NIF_H_
#define ERL_TS_NIF_H_

#include <erl_nif.h>
#include <tree_sitter/api.h>

#define ERL_TS_FUNCTION_DECL(f) \
  static ERL_NIF_TERM f(ErlNifEnv* , int, const ERL_NIF_TERM []);

#define ERL_TS_FUNCTION(f) \
  static ERL_NIF_TERM f(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])

#define ERL_TS_FUNCTION_ARRAY(f , a) { #f, a, f ## _nif }

#define RETURN_BADARG_IF(p) if (p) return enif_make_badarg(env)

#endif /* ERL_TS_NIF_H_ */
