/*
  Converts a private key or certificate in PEM format to a marshaled Cryptokit.RSA.key.

    pem2cryptokit [--certificate] < file.pem > file.ocaml

  Recover the marshaled key with e.g.

    input_value (open_in "file.ocaml")
*/

#include <stdio.h>
#include <string.h>

#include <openssl/rsa.h>
#include <openssl/evp.h>
#include <openssl/x509.h>
#include <openssl/pem.h>
#include <openssl/bn.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>

CAMLextern void caml_startup_code(
           code_t code, asize_t code_size,
           char *data, asize_t data_size,
           char *section_table, asize_t section_table_size,
           char **argv);

typedef long (*primitive)();
primitive caml_builtin_cprim[] = { };
char *caml_names_of_builtin_cprim[] = {};
char global_data[] = {
  /* need to stub out Out_of_memory global for caml_init_exceptions */
  0x84, 0x95, 0xA6, 0xBE, 0x0, 0x0, 0x0, 0x3, 0x0, 0x0, 0x0, 0x1,
  0x0, 0x0, 0x0, 0x3, 0x0, 0x0, 0x0, 0x3, 0xA0, 0x40, 0x40
};

value val_bn(BIGNUM *bn) {
  if (bn) {
    value v = caml_alloc_string(BN_num_bytes(bn));
    BN_bn2bin(bn, String_val(v));
    return v;
  }
  else
    caml_alloc_string(0);
}

value val_rsa(RSA *rsa) {
  CAMLparam0 ();
  CAMLlocal1 (ck_rsa);
  ck_rsa = caml_alloc(8, 0);
  Store_field(ck_rsa, 0, Val_int(BN_num_bits(rsa->n)));
  Store_field(ck_rsa, 1, val_bn(rsa->n));
  Store_field(ck_rsa, 2, val_bn(rsa->e));
  Store_field(ck_rsa, 3, val_bn(rsa->d));
  Store_field(ck_rsa, 4, val_bn(rsa->p));
  Store_field(ck_rsa, 5, val_bn(rsa->q));
  Store_field(ck_rsa, 6, val_bn(rsa->dmp1));
  Store_field(ck_rsa, 7, val_bn(rsa->dmq1));
  Store_field(ck_rsa, 8, val_bn(rsa->iqmp));
  CAMLreturn (ck_rsa);
}

int main(int argc, char **argv)
{
  RSA *rsa = NULL;
  EVP_PKEY *pkey = NULL;
  X509 *x = NULL;

  caml_startup_code(NULL, 0, global_data, sizeof global_data, NULL, 0, 0);

  if (argc > 1 && strcmp(argv[1], "--certificate") == 0)
  {
    x = PEM_read_X509_AUX(stdin,NULL,NULL,NULL);
    if (x)
      pkey = X509_get_pubkey(x);
  }
  else
    pkey = PEM_read_PrivateKey(stdin,NULL,NULL,NULL);

  if (pkey)
    rsa = EVP_PKEY_get1_RSA(pkey);

  if (rsa) {
    value ck_rsa;
    char **buf;
    int len;

    ck_rsa = val_rsa(rsa);
    caml_output_value_to_malloc(ck_rsa, Val_emptylist, &buf, &len);
    write(1, buf, len);
  }
  else
    fprintf(stderr, "Error reading PEM file; check with 'openssl rsa' or 'openssl x509'\n");

  return 0;
}
