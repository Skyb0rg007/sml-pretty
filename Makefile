MOSMLC = mosmlc -liberal

all: prettyprint.ui prettyprint.uo

prettyprint.ui: prettyprint.sig
	$(MOSMLC) -structure -c $<

prettyprint.uo: prettyprint.sml prettyprint.ui
	$(MOSMLC) -structure -c $<

test.uo: test.sml prettyprint.ui
	$(MOSMLC) -toplevel -c $<

test: prettyprint.uo test.uo
	$(MOSMLC) $^ -o test

clean:
	$(RM) *.uo *.ui test
