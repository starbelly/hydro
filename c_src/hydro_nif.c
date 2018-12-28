#include "erl_nif.h"
#include <string.h>
#include <hydrogen.h>

#define ATOM_OK "ok"
#define ATOM_ERROR "error"
#define ATOM_TRUE "true"
#define ATOM_FALSE "false"
#define ATOM_OOM "memory_allocation_failed"
#define ATOM_ENCRYPT_FAIL "encrypt_failed"
#define ATOM_DECRYPT_FAIL "decrypt_failed"
#define ATOM_CONVERSION_FAIL "conversion_failed"
#define ATOM_BAD_CTX_SIZE "bad_context_size"
#define ATOM_BAD_KEY_SIZE "bad_key_size"
#define HASH_STATE_NAME "hydro_hash_state"
#define SIGN_STATE_NAME "hydro_sign_state"

inline ERL_NIF_TERM raise(ErlNifEnv * env, const char *atom_arg)
{
	return enif_raise_exception(env, enif_make_atom(env, atom_arg));
}

inline ERL_NIF_TERM ok_tuple(ErlNifEnv * env, ERL_NIF_TERM ret)
{
	return enif_make_tuple2(env, enif_make_atom(env, ATOM_OK), ret);
}

inline ERL_NIF_TERM ok_tuple3(ErlNifEnv * env, ERL_NIF_TERM ret1,
			      ERL_NIF_TERM ret2)
{
	return enif_make_tuple3(env, enif_make_atom(env, ATOM_OK), ret1, ret2);
}

inline ERL_NIF_TERM error_tuple(ErlNifEnv * env, char *error_atom)
{
	return enif_make_tuple2(env, enif_make_atom(env, "error"),
				enif_make_atom(env, error_atom));
}

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

static ERL_NIF_TERM
enif_hydro_bin2hex(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
	ErlNifBinary bin, hex;

	if ((1 != argc)
	    || (!enif_inspect_binary(env, argv[0], &bin))) {
		return enif_make_badarg(env);
	}

	if (!enif_alloc_binary((bin.size * 2) + 1, &hex)) {
		return raise(env, ATOM_OOM);
	}

	if (NULL ==
	    hydro_bin2hex((char *)hex.data, (bin.size * 2) + 1, bin.data,
			  bin.size)) {
		return error_tuple(env, "conversion_failed");
	}

	return ok_tuple(env, enif_make_binary(env, &hex));
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
		return raise(env, ATOM_OOM);
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
	    || (!enif_inspect_binary(env, argv[1], &seed))) {
		return enif_make_badarg(env);
	}

	if (!enif_alloc_binary(size, &buf)) {
		return error_tuple(env, "alloc_failed");
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

	return enif_make_atom(env, ATOM_OK);
}

static ERL_NIF_TERM
enif_hydro_hash_keygen(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
	ErlNifBinary hash;

	if (argc != 0) {
		return enif_make_badarg(env);
	}

	if (!enif_alloc_binary(hydro_hash_KEYBYTES, &hash)) {
		return error_tuple(env, "alloc_failed");
	}

	hydro_hash_keygen(hash.data);

	return enif_make_binary(env, &hash);
}

static ERL_NIF_TERM
enif_hydro_hash_hash(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
	ErlNifBinary h, m, c, k;

	if ((3 != argc)
	    || (!enif_inspect_binary(env, argv[0], &m))
	    || (!enif_inspect_binary(env, argv[1], &c))
	    || (!enif_inspect_binary(env, argv[2], &k))) {
		return enif_make_badarg(env);
	}

	unsigned char *key = k.data;

	if (0 == k.size) {
		key = NULL;
	}

	if (key && k.size < hydro_hash_KEYBYTES) {
		return error_tuple(env, ATOM_BAD_KEY_SIZE);
	}

	if (c.size != hydro_hash_CONTEXTBYTES) {
		return error_tuple(env, ATOM_BAD_CTX_SIZE);
	}

	if (!enif_alloc_binary(hydro_hash_BYTES, &h)) {
		return raise(env, ATOM_OOM);
	}

	if (0 !=
	    hydro_hash_hash(h.data, h.size, (const char *)m.data,
			    (unsigned long)m.size, (const char *)c.data, key)) {
		enif_release_binary(&h);
		return error_tuple(env, ATOM_ENCRYPT_FAIL);
	}

	return ok_tuple(env, enif_make_binary(env, &h));

}

static ERL_NIF_TERM
enif_hydro_hash_init(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
	ErlNifBinary c, k;

	if ((2 != argc)
	    || (!enif_inspect_binary(env, argv[0], &c))
	    || (!enif_inspect_binary(env, argv[1], &k))) {
		return enif_make_badarg(env);
	}

	unsigned char *key = (0 == k.size) ? NULL : k.data;

	if (key && k.size < hydro_hash_KEYBYTES) {
		return error_tuple(env, ATOM_BAD_KEY_SIZE);
	}

	if (c.size != hydro_hash_CONTEXTBYTES) {
		return error_tuple(env, ATOM_BAD_CTX_SIZE);
	}

	hydro_hash_state *state =
	    (hydro_hash_state *) enif_alloc_resource(hydro_hash_state_t,
						     sizeof(struct
							    hydro_hash_state));

	if (!state) {
		return raise(env, ATOM_OOM);
	}

	if (0 != hydro_hash_init(state, (const char *)c.data, key)) {
		enif_release_resource(state);
		return error_tuple(env, ATOM_ENCRYPT_FAIL);
	}

	ERL_NIF_TERM r = enif_make_resource(env, state);
	enif_release_resource(state);

	return ok_tuple(env, r);
}

static ERL_NIF_TERM
enif_hydro_hash_update(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
	ErlNifBinary m;
	hydro_hash_state *state;

	if ((2 != argc)
	    ||
	    (!enif_get_resource
	     (env, argv[0], hydro_hash_state_t, (void **)&state))
	    || (!enif_inspect_binary(env, argv[1], &m))) {
		return enif_make_badarg(env);
	}

	hydro_hash_state *new_state =
	    (hydro_hash_state *) enif_alloc_resource(hydro_hash_state_t,
						     sizeof(struct
							    hydro_hash_state));

	memcpy(new_state->state, state->state, sizeof(*new_state));

	if (0 !=
	    hydro_hash_update(new_state, (const char *)m.data,
			      (unsigned long)m.size)) {
		return error_tuple(env, ATOM_ENCRYPT_FAIL);
	}

	ERL_NIF_TERM r = enif_make_resource(env, new_state);
	enif_release_resource(new_state);

	return ok_tuple(env, r);
}

static ERL_NIF_TERM
enif_hydro_hash_final(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
	ErlNifBinary h;
	hydro_hash_state *state;

	if ((1 != argc)
	    ||
	    (!enif_get_resource
	     (env, argv[0], hydro_hash_state_t, (void **)&state))) {
		return enif_make_badarg(env);
	}

	if (!enif_alloc_binary(hydro_hash_BYTES, &h)) {
		return raise(env, ATOM_OOM);
	}

	if (0 != hydro_hash_final(state, h.data, h.size)) {
		enif_release_binary(&h);
		return error_tuple(env, ATOM_ENCRYPT_FAIL);
	}

	ERL_NIF_TERM ret = enif_make_binary(env, &h);
	return ok_tuple(env, ret);
}

static ERL_NIF_TERM
enif_hydro_kdf_keygen(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{

	if (argc != 0) {
		return enif_make_badarg(env);
	}

	ErlNifBinary m;

	if (!enif_alloc_binary(hydro_kdf_KEYBYTES, &m)) {
		return error_tuple(env, "alloc_failed");
	}

	hydro_kdf_keygen(m.data);

	return enif_make_binary(env, &m);
}

static ERL_NIF_TERM
enif_hydro_kdf_derive_from_key(ErlNifEnv * env, int argc,
			       ERL_NIF_TERM const argv[])
{
	ErlNifBinary c, m, k;

	unsigned size;
	unsigned sub_id;

	if ((4 != argc)
	    || (!enif_inspect_binary(env, argv[0], &c))
	    || (!enif_inspect_binary(env, argv[1], &m))
	    || (!enif_get_uint(env, argv[2], &sub_id))
	    || (!enif_get_uint(env, argv[3], &size))) {
		return enif_make_badarg(env);
	}

	if (m.size < hydro_kdf_KEYBYTES) {
		return error_tuple(env, ATOM_BAD_KEY_SIZE);
	}

	if (c.size != hydro_hash_CONTEXTBYTES) {
		return error_tuple(env, ATOM_BAD_CTX_SIZE);
	}

	if (!enif_alloc_binary(size, &k)) {
		return raise(env, ATOM_OOM);
	}

	if (0 !=
	    hydro_kdf_derive_from_key(k.data, size, sub_id,
				      (const char *)c.data, m.data)) {
		enif_release_binary(&k);
		return error_tuple(env, ATOM_ENCRYPT_FAIL);
	}

	return ok_tuple(env, enif_make_binary(env, &k));

}

static ERL_NIF_TERM
enif_hydro_pwhash_keygen(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{

	if (argc != 0) {
		return enif_make_badarg(env);
	}

	ErlNifBinary mk;

	if (!enif_alloc_binary(hydro_pwhash_MASTERKEYBYTES, &mk)) {
		return error_tuple(env, "alloc_failed");
	}

	hydro_pwhash_keygen(mk.data);

	return enif_make_binary(env, &mk);
}

static ERL_NIF_TERM
enif_hydro_pwhash_deterministic(ErlNifEnv * env, int argc,
				ERL_NIF_TERM const argv[])
{

	ErlNifBinary h, c, p, mk;

	unsigned size;
	unsigned ops_limit;

	if ((5 != argc)
	    || (!enif_inspect_binary(env, argv[0], &c))
	    || (!enif_inspect_binary(env, argv[1], &p))
	    || (!enif_inspect_binary(env, argv[2], &mk))
	    || (!enif_get_uint(env, argv[3], &size))
	    || (!enif_get_uint(env, argv[4], &ops_limit))) {
		return enif_make_badarg(env);
	}

	if (!enif_alloc_binary(size, &h)) {
		return error_tuple(env, "alloc_failed");
	}

	hydro_pwhash_deterministic(h.data, h.size, (const char *)p.data, p.size,
				   (const char *)c.data, mk.data, ops_limit, 0,
				   1);

	return ok_tuple(env, enif_make_binary(env, &h));
}

static ERL_NIF_TERM
enif_hydro_pwhash_create(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{

	unsigned ops_limit;
	unsigned mem_limit;
	unsigned threads;

	ErlNifBinary h, p, mk;

	if ((5 != argc)
	    || (!enif_inspect_binary(env, argv[0], &p))
	    || (!enif_inspect_binary(env, argv[1], &mk))
	    || (!enif_get_uint(env, argv[2], &ops_limit))
	    || (!enif_get_uint(env, argv[3], &mem_limit))
	    || (!enif_get_uint(env, argv[4], &threads))) {
		return enif_make_badarg(env);
	}

	if (!enif_alloc_binary(hydro_pwhash_STOREDBYTES, &h)) {
		return error_tuple(env, "alloc_failed");
	}

	hydro_pwhash_create(h.data, (const char *)p.data, p.size,
			    mk.data, ops_limit, mem_limit, threads);

	return ok_tuple(env, enif_make_binary(env, &h));
}

static ERL_NIF_TERM
enif_hydro_pwhash_verify(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{

	unsigned ops_limit;
	unsigned mem_limit;
	unsigned threads;

	ErlNifBinary h, p, mk;

	if ((6 != argc)
	    || (!enif_inspect_binary(env, argv[0], &h))
	    || (!enif_inspect_binary(env, argv[1], &p))
	    || (!enif_inspect_binary(env, argv[2], &mk))
	    || (!enif_get_uint(env, argv[3], &ops_limit))
	    || (!enif_get_uint(env, argv[4], &mem_limit))
	    || (!enif_get_uint(env, argv[5], &threads))) {
		return enif_make_badarg(env);
	}

	if (0 != hydro_pwhash_verify(h.data, (const char *)p.data, p.size,
				     mk.data, ops_limit, mem_limit, threads)) {
		return enif_make_atom(env, ATOM_FALSE);
	}

	return enif_make_atom(env, ATOM_TRUE);
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
	    || (!enif_inspect_binary(env, argv[0], &c))
	    || (!enif_inspect_binary(env, argv[1], &h))
	    || (!enif_inspect_binary(env, argv[2], &p))
	    || (!enif_inspect_binary(env, argv[3], &mk))
	    || (!enif_get_uint(env, argv[4], &ops_limit))
	    || (!enif_get_uint(env, argv[5], &mem_limit))
	    || (!enif_get_uint(env, argv[6], &threads))) {
		return enif_make_badarg(env);
	}

	if (!enif_alloc_binary(64, &sk)) {
		return error_tuple(env, "alloc_failed");
	}

	if (0 !=
	    hydro_pwhash_derive_static_key(sk.data, sk.size, h.data,
					   (const char *)p.data, p.size,
					   (const char *)c.data, mk.data,
					   ops_limit, mem_limit, threads)) {
		return error_tuple(env, "deriv_failed");
	}

	return ok_tuple(env, enif_make_binary(env, &sk));
}

static ERL_NIF_TERM
enif_hydro_pwhash_reencrypt(ErlNifEnv * env, int argc,
			    ERL_NIF_TERM const argv[])
{

	ErlNifBinary h1, h, mk, nmk;

	if ((3 != argc)
	    || (!enif_inspect_binary(env, argv[0], &h))
	    || (!enif_inspect_binary(env, argv[1], &mk))
	    || (!enif_inspect_binary(env, argv[2], &nmk))) {
		return enif_make_badarg(env);
	}

	if (!enif_alloc_binary(h.size, &h1)) {
		return error_tuple(env, "alloc_failed");
	}

	memcpy(h1.data, h.data, h.size);

	if (0 != hydro_pwhash_reencrypt(h1.data, mk.data, nmk.data)) {
		return error_tuple(env, "incorrect_key");
	}

	return ok_tuple(env, enif_make_binary(env, &h1));
}

static ERL_NIF_TERM
enif_hydro_pwhash_upgrade(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{

	unsigned ops_limit;
	unsigned mem_limit;
	unsigned threads;

	ErlNifBinary h1, h, mk;

	if ((5 != argc)
	    || (!enif_inspect_binary(env, argv[0], &h))
	    || (!enif_inspect_binary(env, argv[1], &mk))
	    || (!enif_get_uint(env, argv[2], &ops_limit))
	    || (!enif_get_uint(env, argv[3], &mem_limit))
	    || (!enif_get_uint(env, argv[4], &threads))) {
		return enif_make_badarg(env);
	}

	if (!enif_alloc_binary(h.size, &h1)) {
		return error_tuple(env, "alloc_failed");
	}

	memcpy(h1.data, h.data, h.size);

	if (0 !=
	    hydro_pwhash_upgrade(h1.data, mk.data, ops_limit, mem_limit,
				 threads)) {
		return error_tuple(env, "incorrect_key");
	}

	return ok_tuple(env, enif_make_binary(env, &h1));
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
		return error_tuple(env, "alloc_failed");
	}

	hydro_kdf_keygen(k.data);

	return enif_make_binary(env, &k);
}

static ERL_NIF_TERM
enif_hydro_secretbox_encrypt(ErlNifEnv * env, int argc,
			     ERL_NIF_TERM const argv[])
{
	ErlNifBinary h, c, m, k;
	unsigned msg_id;

	if ((4 != argc)
	    || (!enif_inspect_binary(env, argv[0], &c))
	    || (!enif_inspect_binary(env, argv[1], &m))
	    || (!enif_get_uint(env, argv[2], &msg_id))
	    || (!enif_inspect_binary(env, argv[3], &k))) {
		return enif_make_badarg(env);
	}

	if (k.size < hydro_secretbox_KEYBYTES) {
		return error_tuple(env, ATOM_BAD_KEY_SIZE);
	}

	if (c.size != hydro_hash_CONTEXTBYTES) {
		return error_tuple(env, ATOM_BAD_CTX_SIZE);
	}

	if (!enif_alloc_binary(hydro_secretbox_HEADERBYTES + m.size, &h)) {
		return raise(env, ATOM_OOM);
	}

	if (0 !=
	    hydro_secretbox_encrypt(h.data, m.data, m.size, msg_id,
				    (const char *)c.data, k.data)) {
		enif_release_binary(&h);
		return error_tuple(env, ATOM_ENCRYPT_FAIL);
	}

	return ok_tuple(env, enif_make_binary(env, &h));
}

static ERL_NIF_TERM
enif_hydro_secretbox_decrypt(ErlNifEnv * env, int argc,
			     ERL_NIF_TERM const argv[])
{

	unsigned msg_id;
	ErlNifBinary h, c, m, k;

	if ((4 != argc)
	    || (!enif_inspect_binary(env, argv[0], &c))
	    || (!enif_inspect_binary(env, argv[1], &h))
	    || (!enif_get_uint(env, argv[2], &msg_id))
	    || (!enif_inspect_binary(env, argv[3], &k))) {
		return enif_make_badarg(env);
	}

	if (k.size < hydro_secretbox_KEYBYTES) {
		return error_tuple(env, ATOM_BAD_KEY_SIZE);
	}

	if (c.size != hydro_hash_CONTEXTBYTES) {
		return error_tuple(env, ATOM_BAD_CTX_SIZE);
	}

	if (!enif_alloc_binary(h.size - hydro_secretbox_HEADERBYTES, &m)) {
		return raise(env, ATOM_OOM);
	}

	if (0 !=
	    hydro_secretbox_decrypt(m.data, h.data, h.size, msg_id,
				    (const char *)c.data, k.data)) {
		enif_release_binary(&h);
		return error_tuple(env, ATOM_ENCRYPT_FAIL);
	}

	return ok_tuple(env, enif_make_binary(env, &m));
}

static ERL_NIF_TERM
enif_hydro_secretbox_probe_create(ErlNifEnv * env, int argc,
				  ERL_NIF_TERM const argv[])
{
	ErlNifBinary h, c, k, p;

	if ((3 != argc)
	    || (!enif_inspect_binary(env, argv[0], &c))
	    || (!enif_inspect_binary(env, argv[1], &h))
	    || (!enif_inspect_binary(env, argv[2], &k))) {
		return enif_make_badarg(env);
	}

	if (k.size < hydro_secretbox_KEYBYTES) {
		return error_tuple(env, ATOM_BAD_KEY_SIZE);
	}

	if (c.size != hydro_hash_CONTEXTBYTES) {
		return error_tuple(env, ATOM_BAD_CTX_SIZE);
	}

	if (!enif_alloc_binary(hydro_secretbox_PROBEBYTES, &p)) {
		return raise(env, ATOM_OOM);
	}

	hydro_secretbox_probe_create(p.data, h.data, h.size,
				     (const char *)c.data, k.data);

	return enif_make_binary(env, &p);
}

static ERL_NIF_TERM
enif_hydro_secretbox_probe_verify(ErlNifEnv * env, int argc,
				  ERL_NIF_TERM const argv[])
{
	ErlNifBinary h, c, k, p;

	if ((4 != argc)
	    || (!enif_inspect_binary(env, argv[0], &c))
	    || (!enif_inspect_binary(env, argv[1], &h))
	    || (!enif_inspect_binary(env, argv[2], &k))
	    || (!enif_inspect_binary(env, argv[3], &p))) {
		return enif_make_badarg(env);
	}

	if (k.size < hydro_secretbox_KEYBYTES) {
		return error_tuple(env, ATOM_BAD_KEY_SIZE);
	}

	if (c.size != hydro_hash_CONTEXTBYTES) {
		return error_tuple(env, ATOM_BAD_CTX_SIZE);
	}

	if (0 != hydro_secretbox_probe_verify(p.data, h.data, h.size,
					      (const char *)c.data, k.data)) {
		return error_tuple(env, ATOM_ENCRYPT_FAIL);
	}

	return enif_make_atom(env, ATOM_TRUE);
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

	if (!enif_alloc_binary(hydro_sign_PUBLICKEYBYTES, &pk)) {
		return raise(env, ATOM_OOM);
	}

	if (!enif_alloc_binary(hydro_sign_SECRETKEYBYTES, &sk)) {
		return raise(env, ATOM_OOM);
	}

	memmove(pk.data, kp.pk, hydro_sign_PUBLICKEYBYTES);
	memmove(sk.data, kp.sk, hydro_sign_SECRETKEYBYTES);

	return ok_tuple3(env, enif_make_binary(env, &pk),
			 enif_make_binary(env, &sk));
}

static ERL_NIF_TERM
enif_hydro_sign_create(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
	ErlNifBinary c, m, sk, s;

	if ((3 != argc)
	    || (!enif_inspect_binary(env, argv[0], &c))
	    || (!enif_inspect_binary(env, argv[1], &m))
	    || (!enif_inspect_binary(env, argv[2], &sk))) {
		return enif_make_badarg(env);
	}

	if (c.size != hydro_hash_CONTEXTBYTES) {
		return error_tuple(env, ATOM_BAD_CTX_SIZE);
	}

	if (sk.size < hydro_sign_SECRETKEYBYTES) {
		return error_tuple(env, ATOM_BAD_KEY_SIZE);
	}

	if (!enif_alloc_binary(hydro_sign_BYTES, &s)) {
		return raise(env, ATOM_OOM);
	}

	if (0 != hydro_sign_create(s.data, m.data, m.size,
				   (const char *)c.data, sk.data)) {
		return error_tuple(env, ATOM_ENCRYPT_FAIL);
	}

	return ok_tuple(env, enif_make_binary(env, &s));
}

static ERL_NIF_TERM
enif_hydro_sign_verify(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
	ErlNifBinary c, m, s, pk;

	if ((4 != argc)
	    || (!enif_inspect_binary(env, argv[0], &c))
	    || (!enif_inspect_binary(env, argv[1], &m))
	    || (!enif_inspect_binary(env, argv[2], &s))
	    || (!enif_inspect_binary(env, argv[3], &pk))) {
		return enif_make_badarg(env);
	}

	if (c.size != hydro_hash_CONTEXTBYTES) {
		return error_tuple(env, ATOM_BAD_CTX_SIZE);
	}

	if (pk.size < hydro_sign_PUBLICKEYBYTES) {
		return error_tuple(env, ATOM_BAD_KEY_SIZE);
	}

	if (0 != hydro_sign_verify(s.data, m.data, m.size,
				   (const char *)c.data, pk.data)) {
		return enif_make_atom(env, ATOM_FALSE);
	}

	return enif_make_atom(env, ATOM_TRUE);
}

static ERL_NIF_TERM
enif_hydro_sign_init(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
	ErlNifBinary c;

	if ((1 != argc)
	    || (!enif_inspect_binary(env, argv[0], &c))) {
		return enif_make_badarg(env);
	}

	if (c.size != hydro_hash_CONTEXTBYTES) {
		return error_tuple(env, ATOM_BAD_CTX_SIZE);
	}

	hydro_sign_state *state =
	    (hydro_sign_state *) enif_alloc_resource(hydro_sign_state_t,
						     sizeof(struct
							    hydro_sign_state));

	if (0 != hydro_sign_init(state, (const char *)c.data)) {
		enif_release_resource(state);
		return error_tuple(env, ATOM_ENCRYPT_FAIL);
	}

	ERL_NIF_TERM r = enif_make_resource(env, state);
	enif_release_resource(state);

	return ok_tuple(env, r);
}

static ERL_NIF_TERM
enif_hydro_sign_update(ErlNifEnv * env, int argc, ERL_NIF_TERM const argv[])
{
	ErlNifBinary m;
	hydro_sign_state *state;

	if ((2 != argc)
	    ||
	    (!enif_get_resource
	     (env, argv[0], hydro_sign_state_t, (void **)&state))
	    || (!enif_inspect_binary(env, argv[1], &m))) {
		return enif_make_badarg(env);
	}

	hydro_sign_state *new_state =
	    (hydro_sign_state *) enif_alloc_resource(hydro_sign_state_t,
						     sizeof(struct
							    hydro_sign_state));

	memcpy(&new_state->hash_st, &state->hash_st, sizeof(*new_state));

	if (0 !=
	    hydro_sign_update(new_state, (const char *)m.data,
			      (unsigned long)m.size)) {
		return error_tuple(env, ATOM_ENCRYPT_FAIL);
	}

	ERL_NIF_TERM r = enif_make_resource(env, new_state);
	enif_release_resource(new_state);

	return ok_tuple(env, r);
}

static ERL_NIF_TERM
enif_hydro_sign_final_create(ErlNifEnv * env, int argc,
			     ERL_NIF_TERM const argv[])
{

	ErlNifBinary sk, s;
	hydro_sign_state *state;

	if ((2 != argc)
	    ||
	    (!enif_get_resource
	     (env, argv[0], hydro_sign_state_t, (void **)&state))
	    || (!enif_inspect_binary(env, argv[1], &sk))) {
		return enif_make_badarg(env);
	}

	if (!enif_alloc_binary(hydro_sign_BYTES, &s)) {
		return raise(env, ATOM_OOM);
	}

	if (0 != hydro_sign_final_create(state, s.data, sk.data)) {
		enif_release_binary(&s);
		return error_tuple(env, ATOM_ENCRYPT_FAIL);
	}

	ERL_NIF_TERM ret = enif_make_binary(env, &s);
	return ok_tuple(env, ret);

}

static ERL_NIF_TERM
enif_hydro_sign_final_verify(ErlNifEnv * env, int argc,
			     ERL_NIF_TERM const argv[])
{

	hydro_sign_state *state;
	ErlNifBinary s, pk;

	if ((3 != argc)
	    ||
	    (!enif_get_resource
	     (env, argv[0], hydro_sign_state_t, (void **)&state))
	    || (!enif_inspect_binary(env, argv[1], &s))
	    || (!enif_inspect_binary(env, argv[2], &pk))) {
		return enif_make_badarg(env);
	}

	if (0 != hydro_sign_final_verify(state, s.data, pk.data)) {
		return enif_make_atom(env, ATOM_FALSE);
	}

	return enif_make_atom(env, ATOM_TRUE);

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
