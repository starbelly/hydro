#include "erl_nif.h"
#include <string.h>
#include <hydrogen.h>

#define ATOM_OK "ok"
#define ATOM_ERROR "error"
#define ATOM_TRUE "true"
#define ATOM_FALSE "false"

static int
hydro_load(ErlNifEnv * env, void **priv_data, ERL_NIF_TERM load_info)
{
	return hydro_init();
}

static int
hydro_upgrade(ErlNifEnv * env, void **priv, void **old_priv, ERL_NIF_TERM info)
{
	return 0;
}

static void hydro_unload(ErlNifEnv * env, void *priv)
{
	return;
}

static ERL_NIF_TERM hydro_error(ErlNifEnv * env, char *error_atom)
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
		return hydro_error(env, "alloc_failed");
	}

	hydro_random_buf(result.data, result.size);

	return enif_make_binary(env, &result);
}

static ERL_NIF_TERM
enif_hydro_random_u32(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{

	if (argc != 0) {
		return enif_make_badarg(env);
	}

  uint32_t r_uint32 = hydro_random_u32();

	return enif_make_uint(env, r_uint32);
}

static ERL_NIF_TERM
enif_hydro_random_uniform(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
	unsigned upper_bound;

	if ((argc != 1) || (!enif_get_uint(env, argv[0], &upper_bound))) {
		return enif_make_badarg(env);
	}

	uint32_t r_uint32 = hydro_random_uniform(upper_bound);

  return enif_make_uint(env, r_uint32);
}

static ERL_NIF_TERM
enif_hydro_hash_keygen(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
  ErlNifBinary hash;

  if (argc != 0) {
    return enif_make_badarg(env);
  }

 //  uint8_t key[hydro_hash_KEYBYTES];

	if (!enif_alloc_binary(hydro_hash_KEYBYTES, &hash)) {
		return hydro_error(env, "alloc_failed");
	}

  hydro_hash_keygen(hash.data);

	return enif_make_binary(env, &hash);
}

static ErlNifFunc nif_funcs[] = {
  {"hydro_random_buf", 1,
	 enif_hydro_random_buf, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  {"hydro_random_u32", 0,
	 enif_hydro_random_u32, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  {"hydro_random_uniform", 1,
	 enif_hydro_random_uniform, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  {"hydro_hash_keygen", 0,
	 enif_hydro_hash_keygen, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(hydro_api, nif_funcs, &hydro_load, NULL, &hydro_upgrade, &hydro_unload);
