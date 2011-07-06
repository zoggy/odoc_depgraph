#################################################################################
#                Odoc_depgraph                                                  #
#                                                                               #
#    Copyright (C) 2011 Institut National de Recherche en Informatique          #
#    et en Automatique. All rights reserved.                                    #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU Library General Public License version       #
#    2.1 as published by the Free Software Foundation.                          #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
#    GNU Library General Public License for more details.                       #
#                                                                               #
#    You should have received a copy of the GNU Library General Public          #
#    License along with this program; if not, write to the Free Software        #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#                                                                               #
#                                                                               #
#################################################################################

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

install:byte opt
	cp -f $(CMA) $(CMXS) `ocamldoc -customdir`/

test:
	$(OCAMLDOC) -t "Kmedian test doc" -g $(CMA) -d /tmp/ -I ../kmedian ../kmedian/*.ml

testopt:
	$(OCAMLDOCOPT) -t "Kmedian test doc" -g $(CMXS) -d /tmp/ -I ../kmedian ../kmedian/*.ml

clean:
	rm -f *.cm* *.annot *.o *.a

# headers :
###########
HEADFILES= odoc_depgraph.ml Makefile
dummy:
headers: dummy
	echo $(HEADFILES)
	headache -h header -c .headache_config `ls $(HEADFILES) `

noheaders: dummy
	headache -r -c .headache_config `ls $(HEADFILES) `
