CMOFILES=odoc_depgraph.cmo
CMXFILES=$(CMOFILES:.cmo=.cmx)
CMA=odoc_depgraph.cma
CMXS=$(CMA:.cma=.cmxs)

OCAMLOPT=ocamlopt
OCAMLC=ocamlc
OCAMLDOC=ocamldoc
OCAMLDOCOPT=ocamldoc.opt

COMPFLAGS=-annot -I +ocamldoc -I +cameleon2

all: byte opt

byte: $(CMA)
opt: $(CMXS)

$(CMXS): $(CMXFILES)
	$(OCAMLOPT) $(COMPFLAGS) -shared -o $@ odot.cmxa $^

$(CMA): $(CMOFILES)
	$(OCAMLC) $(COMPFLAGS) -a -o $@ odot.cma $^

odoc_depgraph.cmo: odoc_depgraph.ml
	$(OCAMLC) -c $(COMPFLAGS) $<

odoc_depgraph.cmx: odoc_depgraph.ml
	$(OCAMLOPT) -c $(COMPFLAGS) $<

test:
	$(OCAMLDOC) -t "Kmedian test doc" -g $(CMA) -d /tmp/ -I ../kmedian ../kmedian/*.ml

testopt:
	$(OCAMLDOCOPT) -t "Kmedian test doc" -g $(CMXS) -d /tmp/ -I ../kmedian ../kmedian/*.ml

clean:
	rm -f *.cm* *.annot *.o *.a