#simple Makefile for Ocaml KMP

SRCS := kmp.ml
EXEC :=OKmp
CXX  :=/usr/bin/ocamlopt
CXXFLAGS +=

$(EXEC): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $(SRCS)

cleanall: clean
	$(RM) $(EXEC)

clean:
	$(RM) $(SRCS)


