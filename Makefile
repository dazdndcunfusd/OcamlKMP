#simple Makefile for Ocaml KMP

SRCS := Kmp.ml
EXEC :=OKmp
CXX  :=/usr/bin/ocamlopt
CXXFLAGS +=

$(EXEC): $(OBJS)
	$(CXX) $(CXXFLAGS) -o _build/$@ $(SRCS)

cleanall: clean
	$(RM) $(EXEC)

clean:
	$(RM) $(SRCS)


