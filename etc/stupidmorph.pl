:
eval 'exec perl -w -S $0 ${1+"$@"}'
 if 0; 

use strict;
use JSON;

# --------------------------------------------------------------------
# idiotic morphological generator
#
# this outputs for each lemma the name of the lemma and its category
# --------------------------------------------------------------------

# --------------------------------------------------------------------
# from GenI
# --------------------------------------------------------------------

# we return a list of lists of hashes
# - each item in the outer list corresponds to a sentence
# - each item in the inner list corresponds to a word
# - each hash represents the features read from GenI plus a special "__lemma__" feature
#   which holds the lemma
sub read_morph_request {
  my $json_str = shift;
  my $allR = from_json $json_str;
  my @r_sentences = (); # one request per sentence

  foreach my $sentenceR (@$allR) {
    my @r_words = ();
    foreach my $wordR (@$sentenceR) {
      my $lemma = $wordR->{lemma};
      my $featstr = $wordR->{"lemma-features"};
      $featstr =~ s/^\[//;
      $featstr =~ s/\]$//;

      my %feat = ();
      my $av = "";
      for (split(/ /, $featstr)) {
        $av = $_;
        my ($attr, $val) = split(/:/,$av);
        $feat{$attr} = $val;
      }
      $feat{"__lemma__"} = $lemma;
      push @r_words, \%feat;
    }
    push @r_sentences, \@r_words;
  }
  return \@r_sentences
}

# --------------------------------------------------------------------
# morph
# --------------------------------------------------------------------

sub morph {
  my $lemma = shift;
  my $featsR = shift;
  my $cat = $featsR->{"cat"};
  return "$lemma:$cat";
}

# --------------------------------------------------------------------
# main
# --------------------------------------------------------------------

# slurp STDIN to $buf (copied from web)
my $holdTerminator = $/;
undef $/;
my $buf = <STDIN>;
$/ = $holdTerminator;

my @output = ();
my $reqsR = read_morph_request $buf;
foreach my $sentenceR (@$reqsR) {
  my @output_words = ();
  foreach my $wordR (@$sentenceR) {
    my $inflected = morph($wordR->{"__lemma__"}, $wordR);
    push @output_words, $inflected;
  }
  my $output_sentence = join(" ",@output_words);
  my @singleton = ( $output_sentence );
  push @output, \@singleton;
}

print to_json(\@output);
