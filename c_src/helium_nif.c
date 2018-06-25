#include "erl_nif.h"
#include <string.h>
#include <hydrogen.h>

#define ATOM_OK "ok"
#define ATOM_ERROR "error"
#define ATOM_TRUE "true"
#define ATOM_FALSE "false"

static int
helium_load(ErlNifEnv * env, void **priv_data, ERL_NIF_TERM load_info)
{
	return hydro_init();
}

static int
helium_upgrade(ErlNifEnv * env, void **priv, void **old_priv, ERL_NIF_TERM info)
{
	return 0;
}

static void helium_unload(ErlNifEnv * env, void *priv)
{
	return;
}

static ERL_NIF_TERM helium_error(ErlNifEnv * env, char *error_atom)
{
	return enif_make_tuple2(env, enif_make_atom(env, "error"),
				enif_make_atom(env, error_atom));
}


static ERL_NIF_TERM
enif_hydro_random_buf(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
	unsigned req_size;
	ErlNifBinary result;

	if ((argc != 1) || (!enif_get_uint(env, argv[0], &req_size))) {
		return enif_make_badarg(env);
	}

	if (!enif_alloc_binary(req_size, &result)) {
		return helium_error(env, "alloc_failed");
	}

	hydro_random_buf(result.data, result.size);

	return enif_make_binary(env, &result);
}


static ErlNifFunc nif_funcs[] = {
  {"hydro_random_buf", 1,
	 enif_hydro_random_buf, ERL_NIF_DIRTY_JOB_CPU_BOUND},
};

ERL_NIF_INIT(helium_api, nif_funcs, &helium_load, NULL, &helium_upgrade, &helium_unload);
