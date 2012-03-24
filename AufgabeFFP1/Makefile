PROG = AufgabeFFP1
EXT = hs

SOURCE = $(PROG).$(EXT)
SRC_JG = $(PROG).jg.$(EXT)
SRC_MS = $(PROG).ms.$(EXT)

all: test

jg: $(SRC_JG)
	ln -sf $< $(SOURCE)

ms: $(SRC_MS)
	ln -sf $< $(SOURCE)

test: $(SOURCE)
	runghc Test

hand-in: $(SOURCE)
	rsync --verbose --copy-links \
		--chmod=o-rwxs,Dug+x,u+rw,g+r \
		-e "ssh -F ../.ssh-cfg" \
		$(SOURCE) "ffp-handin:Gruppe/$(SOURCE)"

.PHONY: test hand-in all jg ms
