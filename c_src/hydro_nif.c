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
#define NOT_IN_RANGE(arg1, arg2, arg3) (LT_OR_EQ(arg1, arg2) || GT_OR_EQ(arg1, arg3))

#define HASH_STATE_NAME "hydro_hash_state"
#define SIGN_STATE_NAME "hydro_sign_state"

static ErlNifResourceType *hydro_hash_state_t = NULL;
static ErlNifResourceType *hydro_sign_state_t = NULL;

static ErlNifResourceType *init_resource_type(ErlNifEnv * env, const char *type)
{
	return enif_open_resource_type(env, NULL, type, NULL,
				       ERL_NIF_RT_CREATE, NULL);
}

static int hydro_load(ErlNifEnv * env, void **priv_data, ERL_NIF_TERM load_info)
{
	hydro_hash_state_t =
	    init_resource_type(env, (const char *)HASH_STATE_NAME);
	hydro_sign_state_t =
	    init_resource_type(env, (const char *)SIGN_STATE_NAME);

	return !hydro_hash_state_t || !hydro_sign_state_t
	    || hydro_init() == -1 ? 1 : 0;
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
enif_hydro_bin2hex(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
	ErlNifBinary bin, hex;

	if ((1 != argc)
	    || (!GET_BIN(env, argv[0], &bin))) {
		return BADARG(env);
	}

	if (!enif_alloc_binary((bin.size * 2) + 1, &hex)) {
		return OOM_ERROR(env);
	}

	if (NULL ==
	    hydro_bin2hex((char *)hex.data, (bin.size * 2) + 1, bin.data,
			  bin.size)) {
		return ENCRYPT_FAILED_ERROR(env);
	}

	return enif_make_binary(env, &hex);
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
enif_hydro_random_buf_deterministic(ErlNifEnv * env, int argc,
				    ERL_NIF_TERM const argv[])
{
	unsigned size;
	ErlNifBinary buf, seed;

	if ((argc != 2)
	    || (!enif_get_uint(env, argv[0], &size))
	    || (!GET_BIN(env, argv[1], &seed))) {
		return enif_make_badarg(env);
	}

	if (!enif_alloc_binary(size, &buf)) {
		return hydro_error(env, "alloc_failed");
	}

	hydro_random_buf_deterministic(buf.data, buf.size, seed.data);

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
enif_hydro_random_ratchet(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
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
	ErlNifBinary h, m, c, k;

	if ((3 != argc)
	    || (!GET_BIN(env, argv[0], &m))
	    || (!GET_BIN(env, argv[1], &c))
	    || (!GET_BIN(env, argv[2], &k))) {
		return BADARG(env);
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

	if (!ALLOC_BIN(hydro_hash_BYTES, &h)) {
		return OOM_ERROR(env);
	}

	if (0 !=
	    hydro_hash_hash(h.data, h.size, (const char *)m.data,
			    (unsigned long)m.size, (const char *)c.data, key)) {
		FREE_BIN(&h);
		return ENCRYPT_FAILED_ERROR(env);
	}

	return OK_TUPLE(env, MK_BIN(env, &h));

}

static ERL_NIF_TERM
enif_hydro_hash_init(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
	ErlNifBinary c, k;

	if ((2 != argc)
	    || (!GET_BIN(env, argv[0], &c))
	    || (!GET_BIN(env, argv[1], &k))) {
		return BADARG(env);
	}

	unsigned char *key = (0 == k.size) ? NULL : k.data;

	if (key && LT(k.size, hydro_hash_KEYBYTES)) {
		return ERROR(env, ATOM_BAD_KEY_SIZE);
	}

	if (c.size != hydro_hash_CONTEXTBYTES) {
		return ERROR(env, ATOM_BAD_CTX_SIZE);
	}

	hydro_hash_state *state =
	    (hydro_hash_state *) ALLOC_RESOURCE(hydro_hash_state_t,
						sizeof(struct
						       hydro_hash_state));

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
enif_hydro_hash_update(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
	ErlNifBinary m;
	hydro_hash_state *state;

	if ((2 != argc)
	    ||
	    (!GET_RESOURCE(env, argv[0], hydro_hash_state_t, (void **)&state))
	    || (!GET_BIN(env, argv[1], &m))) {
		return BADARG(env);
	}

	hydro_hash_state *new_state =
	    (hydro_hash_state *) ALLOC_RESOURCE(hydro_hash_state_t,
						sizeof(struct
						       hydro_hash_state));

	memcpy(new_state->state, state->state, sizeof(*new_state));

	if (0 !=
	    hydro_hash_update(new_state, (const char *)m.data,
			      (unsigned long)m.size)) {
		return ENCRYPT_FAILED_ERROR(env);
	}

	ERL_NIF_TERM r = MK_RESOURCE(env, new_state);
	FREE_RESOURCE(new_state);

	return OK_TUPLE(env, r);
}

static ERL_NIF_TERM
enif_hydro_hash_final(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
	ErlNifBinary h;
	hydro_hash_state *state;

	if ((1 != argc)
	    ||
	    (!GET_RESOURCE(env, argv[0], hydro_hash_state_t, (void **)&state)))
	{
		return BADARG(env);
	}

	if (!ALLOC_BIN(hydro_hash_BYTES, &h)) {
		return OOM_ERROR(env);
	}

	if (0 != hydro_hash_final(state, h.data, h.size)) {
		FREE_BIN(&h);
		return ENCRYPT_FAILED_ERROR(env);
	}

	ERL_NIF_TERM ret = enif_make_binary(env, &h);
	return OK_TUPLE(env, ret);
}

static ERL_NIF_TERM
enif_hydro_kdf_keygen(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{

	if (argc != 0) {
		return enif_make_badarg(env);
	}

	ErlNifBinary m;

	if (!enif_alloc_binary(hydro_kdf_KEYBYTES, &m)) {
		return hydro_error(env, "alloc_failed");
	}

	hydro_kdf_keygen(m.data);

	return MK_BIN(env, &m);
}

static ERL_NIF_TERM
enif_hydro_kdf_derive_from_key(ErlNifEnv * env, int argc,
			       ERL_NIF_TERM const argv[])
{
	ErlNifBinary c, m, k;

	unsigned size;
	unsigned sub_id;

	if ((4 != argc)
	    || (!GET_BIN(env, argv[0], &c))
	    || (!GET_BIN(env, argv[1], &m))
	    || (!enif_get_uint(env, argv[2], &sub_id))
	    || (!enif_get_uint(env, argv[3], &size))) {
		return BADARG(env);
	}

	if (LT(m.size, hydro_kdf_KEYBYTES)) {
		return ERROR(env, ATOM_BAD_KEY_SIZE);
	}

	if (c.size != hydro_hash_CONTEXTBYTES) {
		return ERROR(env, ATOM_BAD_CTX_SIZE);
	}

	if (!ALLOC_BIN(size, &k)) {
		return OOM_ERROR(env);
	}

	if (0 !=
	    hydro_kdf_derive_from_key(k.data, size, sub_id,
				      (const char *)c.data, m.data)) {
		FREE_BIN(&k);
		return ENCRYPT_FAILED_ERROR(env);
	}

	return OK_TUPLE(env, MK_BIN(env, &k));

}

static ERL_NIF_TERM
enif_hydro_pwhash_keygen(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{

	if (argc != 0) {
		return enif_make_badarg(env);
	}

	ErlNifBinary mk;

	if (!enif_alloc_binary(hydro_pwhash_MASTERKEYBYTES, &mk)) {
		return hydro_error(env, "alloc_failed");
	}

	hydro_pwhash_keygen(mk.data);

	return MK_BIN(env, &mk);
}

static ERL_NIF_TERM
enif_hydro_pwhash_deterministic(ErlNifEnv * env, int argc,
				ERL_NIF_TERM const argv[])
{

	ErlNifBinary h, c, p, mk;

	unsigned size;
	unsigned ops_limit;

	if ((5 != argc)
	    || (!GET_BIN(env, argv[0], &c))
	    || (!GET_BIN(env, argv[1], &p))
	    || (!GET_BIN(env, argv[2], &mk))
	    || (!enif_get_uint(env, argv[3], &size))
	    || (!enif_get_uint(env, argv[4], &ops_limit))) {
		return BADARG(env);
	}

	if (!enif_alloc_binary(size, &h)) {
		return hydro_error(env, "alloc_failed");
	}

	hydro_pwhash_deterministic(h.data, h.size, (const char *)p.data, p.size,
				   (const char *)c.data, mk.data, ops_limit, 0,
				   1);

	return OK_TUPLE(env, MK_BIN(env, &h));
}

static ERL_NIF_TERM
enif_hydro_pwhash_create(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{

	unsigned ops_limit;
	unsigned mem_limit;
	unsigned threads;

	ErlNifBinary h, p, mk;

	if ((5 != argc)
	    || (!GET_BIN(env, argv[0], &p))
	    || (!GET_BIN(env, argv[1], &mk))
	    || (!enif_get_uint(env, argv[2], &ops_limit))
	    || (!enif_get_uint(env, argv[3], &mem_limit))
	    || (!enif_get_uint(env, argv[4], &threads))) {
		return BADARG(env);
	}

	if (!enif_alloc_binary(hydro_pwhash_STOREDBYTES, &h)) {
		return hydro_error(env, "alloc_failed");
	}

	hydro_pwhash_create(h.data, (const char *)p.data, p.size,
			    mk.data, ops_limit, mem_limit, threads);

	return OK_TUPLE(env, MK_BIN(env, &h));
}

static ERL_NIF_TERM
enif_hydro_pwhash_verify(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{

	unsigned ops_limit;
	unsigned mem_limit;
	unsigned threads;

	ErlNifBinary h, p, mk;

	if ((6 != argc)
	    || (!GET_BIN(env, argv[0], &h))
	    || (!GET_BIN(env, argv[1], &p))
	    || (!GET_BIN(env, argv[2], &mk))
	    || (!enif_get_uint(env, argv[3], &ops_limit))
	    || (!enif_get_uint(env, argv[4], &mem_limit))
	    || (!enif_get_uint(env, argv[5], &threads))) {
		return BADARG(env);
	}

	if (0 != hydro_pwhash_verify(h.data, (const char *)p.data, p.size,
				     mk.data, ops_limit, mem_limit, threads)) {
		return MK_ATOM(env, ATOM_FALSE);
	}

	return MK_ATOM(env, ATOM_TRUE);
}

static ERL_NIF_TERM
enif_hydro_pwhash_derive_static_key(ErlNifEnv * env, int argc,
				    ERL_NIF_TERM const argv[])
{

	unsigned ops_limit;
	unsigned mem_limit;
	unsigned threads;

	ErlNifBinary c, h, p, mk, sk;

	if ((7 != argc)
	    || (!GET_BIN(env, argv[0], &c))
	    || (!GET_BIN(env, argv[1], &h))
	    || (!GET_BIN(env, argv[2], &p))
	    || (!GET_BIN(env, argv[3], &mk))
	    || (!enif_get_uint(env, argv[4], &ops_limit))
	    || (!enif_get_uint(env, argv[5], &mem_limit))
	    || (!enif_get_uint(env, argv[6], &threads))) {
		return BADARG(env);
	}

	if (!enif_alloc_binary(64, &sk)) {
		return hydro_error(env, "alloc_failed");
	}

	if (0 !=
	    hydro_pwhash_derive_static_key(sk.data, sk.size, h.data,
					   (const char *)p.data, p.size,
					   (const char *)c.data, mk.data,
					   ops_limit, mem_limit, threads)) {
		return hydro_error(env, "deriv_failed");
	}

	return OK_TUPLE(env, MK_BIN(env, &sk));
}

static ERL_NIF_TERM
enif_hydro_pwhash_reencrypt(ErlNifEnv * env, int argc,
			    ERL_NIF_TERM const argv[])
{

	ErlNifBinary h1, h, mk, nmk;

	if ((3 != argc)
	    || (!GET_BIN(env, argv[0], &h))
	    || (!GET_BIN(env, argv[1], &mk))
	    || (!GET_BIN(env, argv[2], &nmk))) {
		return BADARG(env);
	}

	if (!enif_alloc_binary(h.size, &h1)) {
		return hydro_error(env, "alloc_failed");
	}

	memcpy(h1.data, h.data, h.size);

	if (0 != hydro_pwhash_reencrypt(h1.data, mk.data, nmk.data)) {
		return hydro_error(env, "incorrect_key");
	}

	return OK_TUPLE(env, MK_BIN(env, &h1));
}

static ERL_NIF_TERM
enif_hydro_pwhash_upgrade(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{

	unsigned ops_limit;
	unsigned mem_limit;
	unsigned threads;

	ErlNifBinary h1, h, mk;

	if ((5 != argc)
	    || (!GET_BIN(env, argv[0], &h))
	    || (!GET_BIN(env, argv[1], &mk))
	    || (!enif_get_uint(env, argv[2], &ops_limit))
	    || (!enif_get_uint(env, argv[3], &mem_limit))
	    || (!enif_get_uint(env, argv[4], &threads))) {
		return BADARG(env);
	}

	if (!enif_alloc_binary(h.size, &h1)) {
		return hydro_error(env, "alloc_failed");
	}

	memcpy(h1.data, h.data, h.size);

	if (0 !=
	    hydro_pwhash_upgrade(h1.data, mk.data, ops_limit, mem_limit,
				 threads)) {
		return hydro_error(env, "incorrect_key");
	}

	return OK_TUPLE(env, MK_BIN(env, &h1));
}

static ERL_NIF_TERM
enif_hydro_secretbox_keygen(ErlNifEnv * env, int argc,
			    ERL_NIF_TERM const argv[])
{

	if (argc != 0) {
		return enif_make_badarg(env);
	}

	ErlNifBinary k;

	if (!enif_alloc_binary(hydro_secretbox_KEYBYTES, &k)) {
		return hydro_error(env, "alloc_failed");
	}

	hydro_kdf_keygen(k.data);

	return MK_BIN(env, &k);
}

static ERL_NIF_TERM
enif_hydro_secretbox_encrypt(ErlNifEnv * env, int argc,
			     ERL_NIF_TERM const argv[])
{
	ErlNifBinary h, c, m, k;
	unsigned msg_id;

	if ((4 != argc)
	    || (!GET_BIN(env, argv[0], &c))
	    || (!GET_BIN(env, argv[1], &m))
	    || (!enif_get_uint(env, argv[2], &msg_id))
	    || (!GET_BIN(env, argv[3], &k))) {
		return BADARG(env);
	}

	if (LT(k.size, hydro_secretbox_KEYBYTES)) {
		return ERROR(env, ATOM_BAD_KEY_SIZE);
	}

	if (c.size != hydro_hash_CONTEXTBYTES) {
		return ERROR(env, ATOM_BAD_CTX_SIZE);
	}

	if (!ALLOC_BIN(hydro_secretbox_HEADERBYTES + m.size, &h)) {
		return OOM_ERROR(env);
	}

	if (0 !=
	    hydro_secretbox_encrypt(h.data, m.data, m.size, msg_id,
				    (const char *)c.data, k.data)) {
		FREE_BIN(&h);
		return ENCRYPT_FAILED_ERROR(env);
	}

	return OK_TUPLE(env, MK_BIN(env, &h));
}

static ERL_NIF_TERM
enif_hydro_secretbox_decrypt(ErlNifEnv * env, int argc,
			     ERL_NIF_TERM const argv[])
{

	unsigned msg_id;
	ErlNifBinary h, c, m, k;

	if ((4 != argc)
	    || (!GET_BIN(env, argv[0], &c))
	    || (!GET_BIN(env, argv[1], &h))
	    || (!enif_get_uint(env, argv[2], &msg_id))
	    || (!GET_BIN(env, argv[3], &k))) {
		return BADARG(env);
	}

	if (LT(k.size, hydro_secretbox_KEYBYTES)) {
		return ERROR(env, ATOM_BAD_KEY_SIZE);
	}

	if (c.size != hydro_hash_CONTEXTBYTES) {
		return ERROR(env, ATOM_BAD_CTX_SIZE);
	}

	if (!ALLOC_BIN(h.size - hydro_secretbox_HEADERBYTES, &m)) {
		return OOM_ERROR(env);
	}

	if (0 !=
	    hydro_secretbox_decrypt(m.data, h.data, h.size, msg_id,
				    (const char *)c.data, k.data)) {
		FREE_BIN(&h);
		return ENCRYPT_FAILED_ERROR(env);
	}

	return OK_TUPLE(env, MK_BIN(env, &m));
}

static ERL_NIF_TERM
enif_hydro_secretbox_probe_create(ErlNifEnv * env, int argc,
				  ERL_NIF_TERM const argv[])
{
	ErlNifBinary h, c, k, p;

	if ((3 != argc)
	    || (!GET_BIN(env, argv[0], &c))
	    || (!GET_BIN(env, argv[1], &h))
	    || (!GET_BIN(env, argv[2], &k))) {
		return BADARG(env);
	}

	if (LT(k.size, hydro_secretbox_KEYBYTES)) {
		return ERROR(env, ATOM_BAD_KEY_SIZE);
	}

	if (c.size != hydro_hash_CONTEXTBYTES) {
		return ERROR(env, ATOM_BAD_CTX_SIZE);
	}

	if (!ALLOC_BIN(hydro_secretbox_PROBEBYTES, &p)) {
		return OOM_ERROR(env);
	}

	hydro_secretbox_probe_create(p.data, h.data, h.size,
				     (const char *)c.data, k.data);

	return MK_BIN(env, &p);
}

static ERL_NIF_TERM
enif_hydro_secretbox_probe_verify(ErlNifEnv * env, int argc,
				  ERL_NIF_TERM const argv[])
{
	ErlNifBinary h, c, k, p;

	if ((4 != argc)
	    || (!GET_BIN(env, argv[0], &c))
	    || (!GET_BIN(env, argv[1], &h))
	    || (!GET_BIN(env, argv[2], &k))
	    || (!GET_BIN(env, argv[3], &p))) {
		return BADARG(env);
	}

	if (LT(k.size, hydro_secretbox_KEYBYTES)) {
		return ERROR(env, ATOM_BAD_KEY_SIZE);
	}

	if (c.size != hydro_hash_CONTEXTBYTES) {
		return ERROR(env, ATOM_BAD_CTX_SIZE);
	}

	if (0 != hydro_secretbox_probe_verify(p.data, h.data, h.size,
					      (const char *)c.data, k.data)) {
		return ENCRYPT_FAILED_ERROR(env);
	}

	return MK_ATOM(env, ATOM_TRUE);
}

static ERL_NIF_TERM
enif_hydro_sign_keygen(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{

	if (argc != 0) {
		return enif_make_badarg(env);
	}

	ErlNifBinary pk, sk;

	hydro_sign_keypair kp;
	hydro_sign_keygen(&kp);

	if (!ALLOC_BIN(hydro_sign_PUBLICKEYBYTES, &pk)) {
		return OOM_ERROR(env);
	}

	if (!ALLOC_BIN(hydro_sign_SECRETKEYBYTES, &sk)) {
		return OOM_ERROR(env);
	}

	memmove(pk.data, kp.pk, hydro_sign_PUBLICKEYBYTES);
	memmove(sk.data, kp.sk, hydro_sign_SECRETKEYBYTES);

	return OK_TUPLE3(env, MK_BIN(env, &pk), MK_BIN(env, &sk));
}

static ERL_NIF_TERM
enif_hydro_sign_create(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
	ErlNifBinary c, m, sk, s;

	if ((3 != argc)
	    || (!GET_BIN(env, argv[0], &c))
	    || (!GET_BIN(env, argv[1], &m))
	    || (!GET_BIN(env, argv[2], &sk))) {
		return BADARG(env);
	}

	if (c.size != hydro_hash_CONTEXTBYTES) {
		return ERROR(env, ATOM_BAD_CTX_SIZE);
	}

	if (LT(sk.size, hydro_sign_SECRETKEYBYTES)) {
		return ERROR(env, ATOM_BAD_KEY_SIZE);
	}

	if (!ALLOC_BIN(hydro_sign_BYTES, &s)) {
		return OOM_ERROR(env);
	}

	if (0 != hydro_sign_create(s.data, m.data, m.size,
				   (const char *)c.data, sk.data)) {
		return ENCRYPT_FAILED_ERROR(env);
	}

	return OK_TUPLE(env, MK_BIN(env, &s));
}

static ERL_NIF_TERM
enif_hydro_sign_verify(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
	ErlNifBinary c, m, s, pk;

	if ((4 != argc)
	    || (!GET_BIN(env, argv[0], &c))
	    || (!GET_BIN(env, argv[1], &m))
	    || (!GET_BIN(env, argv[2], &s))
	    || (!GET_BIN(env, argv[3], &pk))) {
		return BADARG(env);
	}

	if (c.size != hydro_hash_CONTEXTBYTES) {
		return ERROR(env, ATOM_BAD_CTX_SIZE);
	}

	if (LT(pk.size, hydro_sign_PUBLICKEYBYTES)) {
		return ERROR(env, ATOM_BAD_KEY_SIZE);
	}

	if (0 != hydro_sign_verify(s.data, m.data, m.size,
				   (const char *)c.data, pk.data)) {
		return MK_ATOM(env, ATOM_FALSE);
	}

	return MK_ATOM(env, ATOM_TRUE);
}

static ERL_NIF_TERM
enif_hydro_sign_init(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
	ErlNifBinary c;

	if ((1 != argc)
	    || (!GET_BIN(env, argv[0], &c))) {
		return BADARG(env);
	}

	if (c.size != hydro_hash_CONTEXTBYTES) {
		return ERROR(env, ATOM_BAD_CTX_SIZE);
	}

	hydro_sign_state *state =
	    (hydro_sign_state *) ALLOC_RESOURCE(hydro_sign_state_t,
						sizeof(struct
						       hydro_sign_state));

	if (0 != hydro_sign_init(state, (const char *)c.data)) {
		FREE_RESOURCE(state);
		return ENCRYPT_FAILED_ERROR(env);
	}

	ERL_NIF_TERM r = MK_RESOURCE(env, state);
	FREE_RESOURCE(state);

	return OK_TUPLE(env, r);
}

static ERL_NIF_TERM
enif_hydro_sign_update(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
	ErlNifBinary m;
	hydro_sign_state *state;

	if ((2 != argc)
	    ||
	    (!GET_RESOURCE(env, argv[0], hydro_sign_state_t, (void **)&state))
	    || (!GET_BIN(env, argv[1], &m))) {
		return BADARG(env);
	}

	hydro_sign_state *new_state =
	    (hydro_sign_state *) ALLOC_RESOURCE(hydro_sign_state_t,
						sizeof(struct
						       hydro_sign_state));

	memcpy(&new_state->hash_st, &state->hash_st, sizeof(*new_state));

	if (0 !=
	    hydro_sign_update(new_state, (const char *)m.data,
			      (unsigned long)m.size)) {
		return ENCRYPT_FAILED_ERROR(env);
	}

	ERL_NIF_TERM r = MK_RESOURCE(env, new_state);
	FREE_RESOURCE(new_state);

	return OK_TUPLE(env, r);
}

static ERL_NIF_TERM
enif_hydro_sign_final_create(ErlNifEnv * env, int argc,
			     ERL_NIF_TERM const argv[])
{

	ErlNifBinary sk, s;
	hydro_sign_state *state;

	if ((2 != argc)
	    ||
	    (!GET_RESOURCE(env, argv[0], hydro_sign_state_t, (void **)&state))
	    || (!GET_BIN(env, argv[1], &sk))) {
		return BADARG(env);
	}

	if (!ALLOC_BIN(hydro_sign_BYTES, &s)) {
		return OOM_ERROR(env);
	}

	if (0 != hydro_sign_final_create(state, s.data, sk.data)) {
		FREE_BIN(&s);
		return ENCRYPT_FAILED_ERROR(env);
	}

	ERL_NIF_TERM ret = enif_make_binary(env, &s);
	return OK_TUPLE(env, ret);

}

static ERL_NIF_TERM
enif_hydro_sign_final_verify(ErlNifEnv * env, int argc,
			     ERL_NIF_TERM const argv[])
{

	hydro_sign_state *state;
	ErlNifBinary s, pk;

	if ((3 != argc)
	    ||
	    (!GET_RESOURCE(env, argv[0], hydro_sign_state_t, (void **)&state))
	    || (!GET_BIN(env, argv[1], &s))
	    || (!GET_BIN(env, argv[2], &pk))) {
		return BADARG(env);
	}

	if (0 != hydro_sign_final_verify(state, s.data, pk.data)) {
		return MK_ATOM(env, ATOM_FALSE);
	}

	return MK_ATOM(env, ATOM_TRUE);

}

static ErlNifFunc nif_funcs[] = {
	{"hydro_bin2hex", 1,
	 enif_hydro_bin2hex, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_random_buf", 1,
	 enif_hydro_random_buf, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_random_buf_deterministic", 2,
	 enif_hydro_random_buf_deterministic, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_random_u32", 0,
	 enif_hydro_random_u32, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_random_uniform", 1,
	 enif_hydro_random_uniform, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_random_ratchet", 0,
	 enif_hydro_random_ratchet, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_hash_keygen", 0,
	 enif_hydro_hash_keygen, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_hash_hash", 3,
	 enif_hydro_hash_hash, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_hash_init", 2,
	 enif_hydro_hash_init, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_hash_update", 2,
	 enif_hydro_hash_update, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_hash_final", 1,
	 enif_hydro_hash_final, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_kdf_keygen", 0,
	 enif_hydro_kdf_keygen, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_kdf_derive_from_key", 4,
	 enif_hydro_kdf_derive_from_key, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_pwhash_keygen", 0,
	 enif_hydro_pwhash_keygen, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_pwhash_deterministic", 5,
	 enif_hydro_pwhash_deterministic, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_pwhash_create", 5,
	 enif_hydro_pwhash_create, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_pwhash_verify", 6,
	 enif_hydro_pwhash_verify, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_pwhash_derive_static_key", 7,
	 enif_hydro_pwhash_derive_static_key, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_pwhash_reencrypt", 3,
	 enif_hydro_pwhash_reencrypt, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_pwhash_upgrade", 5,
	 enif_hydro_pwhash_upgrade, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_secretbox_keygen", 0,
	 enif_hydro_secretbox_keygen, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_secretbox_encrypt", 4,
	 enif_hydro_secretbox_encrypt, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_secretbox_decrypt", 4,
	 enif_hydro_secretbox_decrypt, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_secretbox_probe_create", 3,
	 enif_hydro_secretbox_probe_create, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_secretbox_probe_verify", 4,
	 enif_hydro_secretbox_probe_verify, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_sign_keygen", 0,
	 enif_hydro_sign_keygen, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_sign_create", 3,
	 enif_hydro_sign_create, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_sign_verify", 4,
	 enif_hydro_sign_verify, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_sign_init", 1,
	 enif_hydro_sign_init, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_sign_update", 2,
	 enif_hydro_sign_update, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_sign_final_create", 2,
	 enif_hydro_sign_final_create, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"hydro_sign_final_verify", 3,
	 enif_hydro_sign_final_verify, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(hydro_api, nif_funcs, &hydro_load, NULL, &hydro_upgrade,
	     &hydro_unload);
