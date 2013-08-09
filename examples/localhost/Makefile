.PHONY: all

all: public_key.pem
	ocamlbuild -use-ocamlfind client.byte server.byte

public_key.pem: certificate.pem
	openssl x509 -in certificate.pem -pubkey -noout > public_key.pem

clean:
	ocamlbuild -clean
	rm -f public_key.pem
