OCAMLDIR=`ocamlfind printconf stdlib`
FILES=\
ooauth.cma ooauth.cmxa ooauth.a \
oauth_client.mli oauth_client.cmi \
oauth_server.mli oauth_server.cmi \
oauth_base32.mli oauth_base32.cmi \
oauth_util.cmi \
oauth_ocurl_http_client.cmi \
oauth_netclient_http_client.cmi \
oauth_netcgi_http.cmi

BFILES=$(addprefix _build/,$(FILES))

all: pem2cryptokit
	ocamlbuild ooauth.cma ooauth.cmxa

pem2cryptokit: pem2cryptokit.c
	gcc -g -I$(OCAMLDIR) pem2cryptokit.c -L$(OCAMLDIR) -lssl -lcamlrun -lm -o pem2cryptokit

install: all
	ocamlfind install ooauth META $(BFILES)
	cp pem2cryptokit $(OCAMLDIR)/../../bin

uninstall:
	ocamlfind remove ooauth

clean:
	ocamlbuild -clean
	rm -f pem2cryptokit
	$(MAKE) -C examples clean

dist: clean
	cd ..; tar cvfz ooauth.tar.gz --exclude .svn ooauth
