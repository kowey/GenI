# basicParaphrases makefile
GRAMMAR=headSem


# common lexicon file 
LEX_COMMON=lex/commonLexicon.lex

GDE=$(HOME)/phd/tools/GDE/runLexicalTranslator.sh

# --------------------------------------------------------------------
# nothing else to configure from here on!
# --------------------------------------------------------------------

SHELL=/bin/sh
GRAMMAR_TMP=$(GRAMMAR).tmp
LEX_TMP=tmp.lex

mg_files := $(wildcard gram/*.mg)
lex_files :=$(wildcard lex/*.lex)
prefil_files := $(patsubst lex/%.lex,prefil/%.prefil,$(lex_files))
.PHONY: all grammar

all: grammar lexicons 

grammar: gram/$(GRAMMAR_TMP).rec
lexicons: $(prefil_files)

# --------------------------------------------------------------------
# grammar
# --------------------------------------------------------------------

# remove double entries
gram/$(GRAMMAR).rec: gram/$(GRAMMAR_TMP).rec
	cd gram; CheckTAG $(GRAMMAR_TMP).rec --chk -c $(GRAMMAR).rec 
	rm gram/$(GRAMMAR).rec

# compile the grammar 
gram/$(GRAMMAR_TMP).rec: $(mg_files)
	cd gram; MetaTAG $(GRAMMAR).mg --chk -c $(GRAMMAR_TMP).rec 

# --------------------------------------------------------------------
# lexicon 
# --------------------------------------------------------------------

prefil/%.prefil : lex/%.lex $(LEX_COMMON)
	cat $< $(LEX_COMMON) > $(LEX_TMP)
	$(GDE) -a $(LEX_TMP) $@
