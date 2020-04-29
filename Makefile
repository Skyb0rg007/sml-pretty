
MOSMLC = mosmlc -conservative -I $(BUILDDIR)
RM = rm -f
MKDIR = mkdir -p
RM_D = rm -fd
MV = mv

BUILDDIR = _build

##

.DEFAULT: all
.PHONY: clean test
.SUFFIXES: 

all: $(BUILDDIR)/test

test: $(BUILDDIR)/test
	exec $(BUILDDIR)/test

clean:
	$(RM) $(BUILDDIR)/*.ui $(BUILDDIR)/*.uo $(BUILDDIR)/test
	$(RM_D) $(BUILDDIR)

$(BUILDDIR)/Prettyprint.ui: Prettyprint.sig
	@$(MKDIR) $(BUILDDIR)
	$(MOSMLC) -c Prettyprint.sig
	$(MV) Prettyprint.ui $(BUILDDIR)
$(BUILDDIR)/Prettyprint.uo: Prettyprint.sml $(BUILDDIR)/Prettyprint.ui
	$(MOSMLC) -c Prettyprint.sml
	$(MV) Prettyprint.uo $(BUILDDIR)
$(BUILDDIR)/Ansi.ui: Ansi.sig $(BUILDDIR)/Prettyprint.ui
	$(MOSMLC) -c Ansi.sig
	$(MV) Ansi.ui $(BUILDDIR)
$(BUILDDIR)/Ansi.uo: Ansi.sml $(BUILDDIR)/Prettyprint.ui $(BUILDDIR)/Ansi.ui
	$(MOSMLC) -c Ansi.sml
	$(MV) Ansi.uo $(BUILDDIR)
$(BUILDDIR)/test.ui $(BUILDDIR)/test.uo: test.sml $(BUILDDIR)/Prettyprint.ui $(BUILDDIR)/Ansi.ui
	$(MOSMLC) -toplevel -c test.sml
	$(MV) test.ui test.uo $(BUILDDIR)
$(BUILDDIR)/test: $(BUILDDIR)/Prettyprint.uo $(BUILDDIR)/Ansi.uo $(BUILDDIR)/test.uo
	$(MOSMLC) $(BUILDDIR)/Prettyprint.uo $(BUILDDIR)/Ansi.uo $(BUILDDIR)/test.uo -o $(BUILDDIR)/test

