:
eval 'exec perl -w -S $0 ${1+"$@"}'
 if 0; 

use strict;

# --------------------------------------------------------------------  
# very stupid morphological generator for french
#
# Our intention here is not to build a useful (or correct) 
# morphological generator but to demonstrate what kind of inputs 
# and outputs GenI expects from third party generators.
# --------------------------------------------------------------------  
  
my $in_sentence = 0;

while (<STDIN>) {
  my $lemma = "";
  my %has = ();
  my %hasav = ();
  my %feat = ();

  if (/(.*?)\s*\[(.*)\]/) {   # parse out values
    $lemma = $1;
    my $featstr = $2;
    # store the features into a hash
    # note: this is not feature -> attribute but a simple
    # hash with feat:attr as key
    %has = ();
    my $av = "";
    for (split(/ /, $featstr)) { 
      $av = $_;
      my ($attr, $val) = split(/:/,$av);
      $has{$attr} = 1;
      $hasav{$av} = 1;
      $feat{$attr} = $val
    }
  }

  # control of spaces
  if ($lemma eq "----") { 
    print "\n"; 
    $in_sentence = 0;
    next;
  } elsif ($in_sentence) {
    print " ";
  } else { # start new sentence
    $in_sentence = 1;
  }

  # determiners
  if ($lemma eq "le") {
    if    ($hasav{"num:pl"}) { print "les" }
    elsif ($hasav{"gen:f"})  { print "la"; }
    else  { print "le"; }
  }
  elsif ($lemma eq "un") {
    if    ($hasav{"num:pl"}) { print "des"; }
    elsif ($hasav{"gen:f"})  { print "une"; }
    else  { print "un"; }
  }
  # verbs (-er and -re verbs)
  elsif ($hasav{"cat:v"}) {
    my $stem = $lemma;
    if ($stem =~ s/er$//) {
      if ($hasav{"num:pl"} and $has{"pers"}) {
        my $pers = $feat{"pers"};
        print $stem."ons" if ($pers eq "1");
        print $stem."ez"  if ($pers eq "2");
        print $stem."ent" if ($pers eq "3");
      } else {
        if ($hasav{"pers:2"}) { print $stem."es" }
        else { print $stem."e" }
      }
    }
    if ($stem =~ s/re$//) {
      $stem =~ s/tt$/t/;
      if ($hasav{"num:pl"} and $has{"pers"}) {
        my $pers = $feat{"pers"};
        print $stem."ons" if ($pers eq "1");
        print $stem."ez"  if ($pers eq "2");
        print $stem."ent" if ($pers eq "3");
      } else {
        if ($hasav{"pers:3"}) { print $stem }
        else { print $stem."s" }
      }
    }
  }
  # nouns ˆ l'anglaise
  elsif ($hasav{"cat:n"}) {
    my $stem = $lemma;
    if ($hasav{"num:pl"}) {
      print $stem."s";
    } else { print $stem; }
  }
  
  else { print $lemma }
}
