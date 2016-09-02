OCAMLOPT = ocamlopt

INCLUDE = unix.cmxa

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

clean:
	rm -f *.o *.byte *.cmo *.cmi *.cma *.cmt *.cmti *.cmx $(NAME)
