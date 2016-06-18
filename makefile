rbac: rbac.ml
	ocamlbuild \
	-use-ocamlfind \
	-tag thread \
	-pkg opium.unix \
	-pkg postgresql \
	rbac.native
	mv rbac.native rbac
