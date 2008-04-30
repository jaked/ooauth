all: private_key.ocaml
	ocamlbuild client.byte

private_key.ocaml: private_key.pem
	pem2cryptokit < private_key.pem > private_key.ocaml

clean:
	ocamlbuild -clean
	rm -f private_key.ocaml
