SHELL=/bin/sh
GENI:=geni

COMPILED_GRAMMAR_DIR:=compiled-grammar
GRAMMAR_DIR:=grammar
GRAMMAR:=Evaluations
GRAMMAR_RAW_MG:=$(GRAMMAR_DIR)/$(GRAMMAR).mg

LEXICON_DIR:=lexicon
FULL_LEXICON_PREFIX:=demo-lemma-utf8
MORPH_PREFIX:=demo-morph-utf8

MACROS_FILE:=macros.mac

SUITE_LEXICON:=$(LEXICON_DIR)/$(FULL_LEXICON_PREFIX).lex
SUITE_MORPH:=$(LEXICON_DIR)/$(MORPH_PREFIX).mph

GENI_GRAMMAR:=$(COMPILED_GRAMMAR_DIR)/$(GRAMMAR).genib
GENI_LEXICON:=$(LEXICON_DIR)/$(FULL_LEXICON_PREFIX).glex
GENI_MORPH:=$(LEXICON_DIR)/$(MORPH_PREFIX).gmorph
GENI_SUITE:=suite

GENI_OPTIMISATIONS:='pol f-sem'

GENI_LEX_FLAGS:=-l $(GENI_LEXICON) -s $(GENI_SUITE)
ifdef ENABLE_MORPH
GENI_LEX_FLAGS+=--morphlexicon $(GENI_MORPH)
endif
GENI_FLAGS:=$(GENI_LEX_FLAGS) --opt=$(GENI_OPTIMISATIONS)

# --------------------------------------------------------------------
# main targets, etc
# --------------------------------------------------------------------

all: run

ECHO_STATUS:=@echo "[XMG/GenI demo]"
ifndef VERBOSE
SILENTLY:=@
endif

mg_files := $(wildcard $(GRAMMAR_DIR)/*.mg)

.PHONY: all macros run grammar morph clean

grammar: $(GENI_GRAMMAR)
lexicon: $(GENI_LEXICON)
morph: $(GENI_MORPH)
macros: $(LEXICON_DIR)/$(MACROS_FILE)


# --------------------------------------------------------------------
# demo stuff
# --------------------------------------------------------------------

NEW_SUMMARY:=new-summary
OLD_DERIVATIONS:=old-derivations
NEW_DERIVATIONS_ORIG:=tmp-derivations
NEW_DERIVATIONS:=new-derivations

run: $(GENI_GRAMMAR) lexicon morph
	$(GENI) -m $< $(GENI_FLAGS) --nogui --batchdir=results

# --------------------------------------------------------------------
# running GenI
# --------------------------------------------------------------------

run-geni: $(GENI_GRAMMAR) lexicon morph
	$(GENI) -m $< $(GENI_FLAGS)

$(SUITE_DIR_GENI)/.geni-input:
	$(NO_SUITE_ERROR)

# --------------------------------------------------------------------
# cleaning
# --------------------------------------------------------------------

clean:
	rm -rf $(COMPILED_GRAMMAR_DIR)/*
	rm -rf $(LEXICON_DIR)/*.gmorph $(LEXICON_DIR)/*.glex $(LEXICON_DIR)/macros.mac $(LEXICON_DIR)/macros.lin $(GRAMMAR_DIR)/macros.mac $(GRAMMAR_DIR)/macros.lin
	rm -rf results/*

# --------------------------------------------------------------------
# grammar
# --------------------------------------------------------------------

# remove double entries
%.rec: %.tmprec
	$(ECHO_STATUS) removing double-entries to produce $@
	$(SILENTLY) CheckTAG $< --all -c $@ -o $(basename $@).xml

# compile the grammar
$(COMPILED_GRAMMAR_DIR)/%.tmprec: $(GRAMMAR_DIR)/%.mg
	$(ECHO_STATUS) compiling metagrammar $<
	$(SILENTLY) MetaTAG $< --chk -c $@

$(COMPILED_GRAMMAR_DIR)/$(GRAMMAR).rec: $(COMPILED_GRAMMAR_DIR)/$(GRAMMAR).tmprec
$(COMPILED_GRAMMAR_DIR)/$(GRAMMAR).tmprec: $(mg_files)

# --------------------------------------------------------------------
# grammar converted to geni format
# --------------------------------------------------------------------

%.xml : %.rec
	:

$(COMPILED_GRAMMAR_DIR):
	mkdir $@

%.geni : %.xml
	$(ECHO_STATUS) "converting to geni text format: $(basename $<)"
	$(SILENTLY) geniconvert -f tagml -t geni $< -o $@

%.genib : %.geni
	$(ECHO_STATUS) "converting to geni binary format: $<"
	$(SILENTLY) geniconvert -f geni -t genib $< -o $@

# --------------------------------------------------------------------
# lexicon
# --------------------------------------------------------------------
# compile semantic macros from SemVal.mg
# see the GNU make manual on implicit rules/automatic variables
# for $(<D), $(<F) and $(@F)
$(GRAMMAR_DIR)/$(MACROS_FILE): $(GRAMMAR_RAW_MG)
	$(ECHO_STATUS) compiling semantic macros only
	$(SILENTLY) MetaTAG $< --mac -s $(basename $@)

$(LEXICON_DIR)/$(MACROS_FILE): $(GRAMMAR_DIR)/$(MACROS_FILE)
	$(SILENTLY) cp -f $< $@

#convert lexicon to geni format using semantic macros
%.glex: %.lex $(LEXICON_DIR)/$(MACROS_FILE)
	$(ECHO_STATUS) lexically converting $<
	$(SILENTLY) lexConverter -L -g -i $< -o $@

#convert lexicon to geni format
%.gmorph: %.mph
	$(SILENTLY) sed -e 's/pos/cat/g' -e 's/=/:/g' -e 's/;/ /g' < $< > $@
