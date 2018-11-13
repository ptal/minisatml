OCAMLOPT = ocamlopt
OCAMLB = ocamlc
INCLUDE = unix.cmxa
INCLUDEB = unix.cma
OPTIONS = -noassert

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
	$(OCAMLOPT) -Oclassic $(MLI)
	$(OCAMLOPT) -Oclassic -o $(NAME)_o0 $(INCLUDE)  $(ML)

o1:
	$(OCAMLOPT) $(MLI)
	$(OCAMLOPT) -o $(NAME)_o1 -g $(INCLUDE)  $(ML)

o2:
	$(OCAMLOPT) -O2 $(OPTIONS) $(MLI)
	$(OCAMLOPT) -O2 $(OPTIONS) -o $(NAME)_o2 $(INCLUDE)  $(ML)

o3:
	$(OCAMLOPT) -O3 $(OPTIONS) $(MLI)
	$(OCAMLOPT) -O3 $(OPTIONS) -o $(NAME)_o3 $(INCLUDE)  $(ML)

debug:
	$(OCAMLB) $(MLI)
	$(OCAMLB) -o $(NAME)_debug -g $(INCLUDEB)  $(ML)


clean:
	rm -f *.o *.byte *.cmo *.cmi *.cma *.cmt *.cmti *.cmx $(NAME) $(NAME)_*
