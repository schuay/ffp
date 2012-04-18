PROG = AufgabeFFP3
TEST = Test
EXT = hs

SOURCE   = $(PROG).$(EXT)
SRC_JG   = $(PROG).jg.$(EXT)
SRC_MS   = $(PROG).ms.$(EXT)
SRC_TEST = $(TEST).$(EXT)

HOST = ffp-handin
GROUPDIR = Gruppe

SSH = ssh
RSYNC = rsync
ECHO = echo
LN = ln
RUNGHC = runghc
RUNHUGS = runhugs
RM = rm

LNFLAGS = -sf
SFLAGS = -F "../.ssh-cfg"
RFLAGS = --copy-links --chmod=o-rwxs,Dug+x,u+rw,g+r \
		-e "$(SSH) $(SFLAGS)"

# .ssh-cfg format:
# Host ffp-handin
# User ffpXXXXXXX
# HostName g0.complang.tuwien.ac.at

all: test

jg: $(SRC_JG)
	$(LN) $(LNFLAGS) $< $(SOURCE)

ms: $(SRC_MS)
	$(LN) $(LNFLAGS) $< $(SOURCE)

test: $(SOURCE)
	@$(ECHO) "Running local tests..."
	@$(RUNGHC) $(TEST)

# Note: test-remote does not overwrite group hand-ins or
# leave files on the server. $(SRC_TEST) must contain the module Main.

test-remote: $(SOURCE) $(SRC_TEST)
	@$(RSYNC) $(RFLAGS) $(SRC_TEST) "$(HOST):$(SRC_TEST)"
	@$(RSYNC) $(RFLAGS) $(SOURCE) "$(HOST):$(SOURCE)"
	@$(ECHO) "Running remote tests..."
	@$(SSH) $(SFLAGS) $(HOST) '$(RUNHUGS) $(TEST); $(RM) -f $^'

hand-in: $(SOURCE) test test-remote
	@$(ECHO) "Handing in $(SOURCE)..."
	$(RSYNC) $(RFLAGS) $(SOURCE) "$(HOST):$(GROUPDIR)/$(SOURCE)"


.PHONY: test test-remote hand-in all jg ms
