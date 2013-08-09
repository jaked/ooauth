PREFIX=/usr/local
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

all:
	ocamlbuild -use-ocamlfind ooauth.cma ooauth.cmxa

install: all
	ocamlfind install ooauth META $(BFILES)

uninstall:
	ocamlfind remove ooauth

clean:
	ocamlbuild -clean
	$(MAKE) -C examples clean

dist: clean
	cd ..; tar cvfz ooauth.tar.gz --exclude .svn ooauth
