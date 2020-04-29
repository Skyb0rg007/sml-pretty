
MOSMLC = mosmlc -liberal

all: test

.SUFFIXES: .ui .uo .sig .sml

.sig.ui:
	$(MOSMLC) -structure -c $<
.sml.uo:
	$(MOSMLC) -structure -c $<

test.uo: test.sml
	$(MOSMLC) -toplevel -c $<

test: Prettyprint.uo Ansi.uo test.uo
	$(MOSMLC) $^ -o test

clean:
	$(RM) *.uo *.ui test

Prettyprint.ui:
Prettyprint.uo: Prettyprint.ui
Ansi.ui: Prettyprint.ui
Ansi.uo: Prettyprint.ui Ansi.ui
test.uo: Prettyprint.ui Ansi.ui

