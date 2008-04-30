all: private_key.ocaml certificate.ocaml
	ocamlbuild client.byte server.byte

private_key.ocaml: private_key.pem
	pem2cryptokit < private_key.pem > private_key.ocaml

certificate.ocaml: certificate.pem
	pem2cryptokit --certificate < certificate.pem > certificate.ocaml

clean:
	ocamlbuild -clean
	rm -f private_key.ocaml certificate.ocaml
