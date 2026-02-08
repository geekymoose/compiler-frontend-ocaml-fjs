# ------------------------------------------------------------------------------
OCAMLC = ocamlc
OCAMLY = ocamlyacc
OCAMLL = ocamllex
RM = rm
TARGET = fjs-parser
OBJS = error.cmo exp.cmo parser.cmo lexer.cmo main.cmo

FLAGS_YACC=-v
FLAGS_OC=


# ------------------------------------------------------------------------------
$(TARGET) : $(OBJS)
	$(OCAMLC) -o $(TARGET) $^

%.mli: %.mly
	$(OCAMLY) $(FLAGS_YACC) $^
%.ml: %.mli
	$(OCAMLC) -c $^

%.ml: %.mll
	$(OCAMLL) $^
%.cmo: %.ml
	$(OCAMLC) $(FLAGS_OC) -c $^


# ------------------------------------------------------------------------------
.PHONY: clean
clean:
	$(RM) -f *.cmo *.cmi *.mli
	$(RM) -f *.out *.output
	$(RM) -f lexer.ml
	$(RM) -f parser.ml
	$(RM) -f $(OBJS)
	$(RM) -f $(TARGET)
	$(RM) -rf _build

