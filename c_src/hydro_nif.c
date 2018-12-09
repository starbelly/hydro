#include "erl_nif.h"
#include <string.h>
#include <hydrogen.h>

#define ATOM_OK "ok"
#define ATOM_UNKNOWN "unknown"
#define ATOM_ERROR "error"
#define ATOM_TRUE "true"
#define ATOM_FALSE "false"
#define ATOM_OOM "out_of_memory"
#define ATOM_ENCRYPT_FAIL "encrypt_failed"
#define ATOM_DECRYPT_FAIL "decrypt_failed"
#define ATOM_BAD_SIZE "bad_size"
#define ATOM_BAD_SALT_SIZE "bad_salt_size"
#define ATOM_BAD_HASH_SIZE "bad_hash_size"
#define ATOM_BAD_CTX_SIZE "bad_context_size"
#define ATOM_BAD_KEY_SIZE "bad_key_size"
#define ATOM_BAD_NONCE_SIZE "bad_nonce_size"

#define MK_ATOM(env, str) enif_make_atom(env, str)
#define MK_BIN(env, bin) enif_make_binary(env, bin)
#define MK_TUPLE(env, ret1, ret2) enif_make_tuple2(env, ret1, ret2)
#define MK_RESOURCE(env, res) enif_make_resource(env, res)
#define BADARG(env) enif_make_badarg(env)
#define GET_BIN(env, term, bin) enif_inspect_binary(env, term, bin)
#define GET_RESOURCE(env, term, type, res) enif_get_resource(env, term, type, res)
#define MK_UINT(env, val, size) enif_get_uint(env, val, size)
#define ALLOC_BIN(size, pk) enif_alloc_binary(size, pk)
#define ALLOC_RESOURCE(env, type) enif_alloc_resource(env, type)
#define ERROR(env, atom_arg) enif_make_tuple2(env, MK_ATOM(env, ATOM_ERROR), MK_ATOM(env, atom_arg))
#define OOM_ERROR(env) ERROR(env, ATOM_OOM)
#define ENCRYPT_FAILED_ERROR(env) ERROR(env, ATOM_ENCRYPT_FAIL)
#define DECRYPT_FAILED_ERROR(env) ERROR(env, ATOM_DECRYPT_FAIL)
#define BAD_SALT_SIZE_ERROR(env) ERROR(env, ATOM_BAD_SALT_SIZE)
#define BAD_KEY_SIZE_ERROR(env) ERROR(env, ATOM_BAD_KEY_SIZE)
#define BAD_NONCE_SIZE_ERROR(env) ERROR(env, ATOM_BAD_NONCE_SIZE)
#define OK_TUPLE(env, ret) enif_make_tuple2(env, MK_ATOM(env, ATOM_OK), ret)
#define OK_TUPLE3(env, ret1, ret2) enif_make_tuple3(env, MK_ATOM(env, ATOM_OK), ret1, ret2)
#define RAISE(env, atom_arg) enif_raise_exception(env, MK_ATOM(env, atom_arg))
#define FREE(r) enif_free(r)
#define FREE_BIN(bin) enif_release_binary(bin)
#define FREE_RESOURCE(res) enif_release_resource(res)
#define IS_NUM(env, arg) enif_is_number(env, arg)
#define GT(arg1, arg2) arg1 > arg2
#define LT(arg1, arg2) arg1 < arg2
#define LT_OR_EQ(arg1, arg2) arg1 <= arg2
#define GT_OR_EQ(arg1, arg2) arg1 >= arg2
#define IN_RANGE(arg1, arg2, arg3) (LT_OR_EQ(arg2, arg1) && GT_OR_EQ(arg2, arg1))
#define NOT_IN_RANGE(arg1, arg2, arg3) (LT_OR_EQ(arg1, arg2) && GT_OR_EQ(arg1, arg2))


#define HASH_STATE_NAME "hydro_hash_state"

static ErlNifResourceType *hydro_hash_state_t = NULL;

static ErlNifResourceType *init_resource_type(ErlNifEnv * env)
{
  return enif_open_resource_type(env, NULL, HASH_STATE_NAME, NULL,
               ERL_NIF_RT_CREATE, NULL);
}

static int
hydro_load(ErlNifEnv * env, void **priv_data, ERL_NIF_TERM load_info)
{
  hydro_hash_state_t = init_resource_type(env);
	return !hydro_hash_state_t || hydro_init() == -1 ? 1 : 0;
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
	ErlNifBinary buf;

	if ((argc != 1) || (!enif_get_uint(env, argv[0], &req_size))) {
		return enif_make_badarg(env);
	}

	if (!enif_alloc_binary(req_size, &buf)) {
		return hydro_error(env, "alloc_failed");
	}

	hydro_random_buf(buf.data, buf.size);

	return enif_make_binary(env, &buf);
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
enif_hydro_random_ratchet(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])  { 
	if ((argc != 0)) {
		return enif_make_badarg(env);
	}
  
  hydro_random_ratchet();

  return MK_ATOM(env, ATOM_OK);
}

static ERL_NIF_TERM
enif_hydro_hash_keygen(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
  ErlNifBinary hash;

  if (argc != 0) {
    return enif_make_badarg(env);
  }

	if (!enif_alloc_binary(hydro_hash_KEYBYTES, &hash)) {
		return hydro_error(env, "alloc_failed");
	}

  hydro_hash_keygen(hash.data);

	return enif_make_binary(env, &hash);
}

static ERL_NIF_TERM
enif_hydro_hash_hash(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
  unsigned int s;
  ErlNifBinary h, m, c, k;

  if ((4 != argc)
      || !IS_NUM(env, argv[0])
      || (!MK_UINT(env, argv[0], &s))
      || (!GET_BIN(env, argv[1], &m))
      || (!GET_BIN(env, argv[2], &c))
      || (!GET_BIN(env, argv[3], &k))) {
    return BADARG(env);
  }

  if (NOT_IN_RANGE(s, hydro_hash_BYTES_MIN, hydro_hash_BYTES_MAX)) {
    return ERROR(env, ATOM_BAD_HASH_SIZE);
  }

  unsigned char *key = k.data;

  if (0 == k.size) { 
    key = NULL;
  }

  if (key && LT(k.size, hydro_hash_KEYBYTES)) {
    return ERROR(env, ATOM_BAD_KEY_SIZE);
  }

  if (c.size != hydro_hash_CONTEXTBYTES) { 
    return ERROR(env, ATOM_BAD_CTX_SIZE);
  }

  if (!ALLOC_BIN(s, &h)) {
    return OOM_ERROR(env);
  }

  if (0 !=
      hydro_hash_hash(h.data, h.size, m.data, m.size, (const char *)c.data, k.data)) {
    FREE_BIN(&h);
    return ENCRYPT_FAILED_ERROR(env);
  }

  return OK_TUPLE(env, MK_BIN(env, &h));

}

static ERL_NIF_TERM
enif_hydro_hash_init(ErlNifEnv * env, int argc,
           ERL_NIF_TERM const argv[])
{
  unsigned int s;
  ErlNifBinary c, k;

  if ((3 != argc)
      || !IS_NUM(env, argv[0])
      || (!MK_UINT(env, argv[0], &s))
      || (!GET_BIN(env, argv[1], &c))
      || (!GET_BIN(env, argv[2], &k))) {
    return BADARG(env);
  }

  if (NOT_IN_RANGE(s, hydro_hash_BYTES, hydro_hash_BYTES_MAX)) {
    return ERROR(env, ATOM_BAD_HASH_SIZE);
  }

  unsigned char *key = (0 == k.size) ? NULL : k.data;

  if (key && LT(k.size, hydro_hash_KEYBYTES)) {
    return ERROR(env, ATOM_BAD_KEY_SIZE);
  }

  if (c.size != hydro_hash_CONTEXTBYTES) { 
    return ERROR(env, ATOM_BAD_CTX_SIZE);
  }

  hydro_hash_state *state =
      (hydro_hash_state *) ALLOC_RESOURCE(hydro_hash_state_t, s);

  if (!state) {
    return OOM_ERROR(env);
  }

  if (0 != hydro_hash_init(state, (const char *)c.data, key)) {
    FREE_RESOURCE(state);
    return ENCRYPT_FAILED_ERROR(env);
  }

  ERL_NIF_TERM r = MK_RESOURCE(env, state);
  FREE_RESOURCE(state);

  return OK_TUPLE(env, r);
}

static ERL_NIF_TERM
enif_hydro_hash_update(ErlNifEnv * env, int argc,
             ERL_NIF_TERM const argv[])
{
  ErlNifBinary m;
  hydro_hash_state *state;

  if ((2 != argc)
      || (!GET_RESOURCE(env, argv[0], hydro_hash_state_t, (void **)&state))
      || (!GET_BIN(env, argv[1], &m))) {
    return BADARG(env);
  }

  if (0 != hydro_hash_update(state, m.data, m.size)) {
    return ENCRYPT_FAILED_ERROR(env);
  }

  return OK_TUPLE(env, MK_ATOM(env, ATOM_TRUE));
}

static ERL_NIF_TERM
enif_hydro_hash_final(ErlNifEnv * env, int argc,
            ERL_NIF_TERM const argv[])
{
  unsigned int s;
  ErlNifBinary h;
  hydro_hash_state *state;

  if ((2 != argc)
      || !IS_NUM(env, argv[0])
      || (!MK_UINT(env, argv[0], &s))
      || (!GET_RESOURCE(env, argv[1], hydro_hash_state_t, (void **)&state))) {
    return BADARG(env);
  }

  if (NOT_IN_RANGE(s, hydro_hash_BYTES, hydro_hash_BYTES_MAX)) {
    return ERROR(env, ATOM_BAD_HASH_SIZE);
  }

  if (!ALLOC_BIN(enif_sizeof_resource(state), &h)) {
    return OOM_ERROR(env);
  }

  if (0 != hydro_hash_final(state, h.data, h.size)) {
    FREE_BIN(&h);
    return ENCRYPT_FAILED_ERROR(env);
  }

  ERL_NIF_TERM ret = enif_make_binary(env, &h);
  return OK_TUPLE(env, ret);
}

static ErlNifFunc nif_funcs[] = {
  {"hydro_random_buf", 1,
	 enif_hydro_random_buf, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  {"hydro_random_u32", 0,
	 enif_hydro_random_u32, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  {"hydro_random_uniform", 1,
	 enif_hydro_random_uniform, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  {"hydro_random_ratchet", 0,
	 enif_hydro_random_ratchet, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  {"hydro_hash_keygen", 0,
	 enif_hydro_hash_keygen, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  {"hydro_hash_hash", 4,
	 enif_hydro_hash_hash, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  {"hydro_hash_init", 3,
	 enif_hydro_hash_init, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  {"hydro_hash_update", 2,
	 enif_hydro_hash_update, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  {"hydro_hash_final", 2,
	 enif_hydro_hash_final, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(hydro_api, nif_funcs, &hydro_load, NULL, &hydro_upgrade, &hydro_unload);
