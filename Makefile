OCAMLOPT = ocamlopt
OCAMLB = ocamlc
INCLUDE = unix.cmxa
INCLUDEB = unix.cma

NAME=satML

MLI = 	vec.mli \
	heap.mli \
	types.mli \
	solver.mli \
	parser.mli \

ML = 	vec.ml \
	heap.ml \
	types.ml \
	solver.ml \
	parser.ml \
	main.ml \

.SUFFIXES: .mli .ml .cmi .cmo .cmx .mll .mly

all:
	$(OCAMLOPT) $(MLI)
	$(OCAMLOPT) -o $(NAME) -g $(INCLUDE)  $(ML)

opt:
	$(OCAMLOPT) -O2 $(MLI)
	$(OCAMLOPT) -O2 -o $(NAME)_opt -g $(INCLUDE)  $(ML)

debug:
	$(OCAMLB) $(MLI)
	$(OCAMLB) -o $(NAME) -g $(INCLUDEB)  $(ML)


clean:
	rm -f *.o *.byte *.cmo *.cmi *.cma *.cmt *.cmti *.cmx $(NAME)
