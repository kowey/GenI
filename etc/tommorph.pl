:
eval 'exec perl -w -S $0 ${1+"$@"}'
 if 0; 

# GenI surface realiser
# Copyright (C) 2005 Carlos Areces and Eric Kow
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

# ********************************************************************** 
# This is a wrapper for the mmorph generator 
#
# Note: mmorph is not free software.  You'll have to contact the
# developers to request a copy.
# ********************************************************************** 

use strict;

# ---------------------------------------------------------------------- 
# Parameterisation
# ---------------------------------------------------------------------- 

my $mmorph_cmd = "java -jar grammars/fst.jar grammars/french.fst -g";

# [geni category]
my @geni_cats = ("n", "v");

# looks up attributes that each geni category should have
# to form a complete mmorph input
# geni category -> [ geni attribute ]
my %mmorph_cat_to_attrs = (
  "n" => [ "gen","num"],
  "pro" => [ "gen","num","pers","type_s","case" ],
  "v" => [ "mode","tense","num","pers" ],
);

# geni to mmorph attribute name translator
# geni category -> mmorph category
my %mmorph_names = (
  "gen"  => "gender",
  "num"  => "number",
  "pers" => "person",
);

# geni to mmorph value name translator
# geni category -> geni value -> mmorph value 
my %mmorph = ( 
  cat => {
    ""   => "",
    "n"  => "Noun",
    "v"  => "Verb",
    "pn" => "Pronom",
  },

  case => {
    "" => "nom",
  },

  gen => {
    ""   => "masculine",
    "m"  => "masculine",
    "f"  => "feminine",
  },

  mode => { 
    ""  => "indicative", 
  },

  num => {
    ""   => "singular",
    "sg" => "singular",
    "pl" => "plural",
  },

  pers => {
    ""  => "1",
  },

  tense => {
    ""  => "present",
  },

  type_s => {
    ""   => "perso"
  }
);

my $SENTENCE_SEPARATOR = "SENTENCE_SEPERATOR";

# ---------------------------------------------------------------------- 
# Subroutines 
# ---------------------------------------------------------------------- 

# lookup_morph_list:
# looks up a list of attributes from the geni-to-mmorph
# conversion tables
#   $lemma  - the lemma this is for (for error messages)
#   @$attrs - list ref of attributes to look up
#   %$has   - hash ref: if the current lemma has $attr or not
#   %$feat  - hash ref: the value of attribute $attr for the current lemma
sub lookup_morph_list {
  my ($lemma,$attrs_ref,$has_ref,$feat_ref) = @_;
  my $out = "";
  foreach $a (@$attrs_ref) {
    $out .= lookup_morph($lemma,$a,$has_ref,$feat_ref);
  }
  return $out;
}


# lookup_morph: 
# looks up an attribute from the geni-to-mmorph
# if an attribute (or a value) does not exist, it spits out a 
# warning and chooses a default value
# conversion tables
#   $lemma - the lemma this is for (for error messages)
#   $attr  - attribute to look up
#   %$has  - hash ref: if the current lemma has $attr or not
#   %$feat - hash ref: the value of attribute $attr for the current lemma
sub lookup_morph {
  my ($lemma,$attr,$has_ref,$feat_ref) = @_;
  my $val_in = "";

  if (not $$has_ref{$attr}) {
    $val_in = "";
    #print STDERR "Warning! attribute '".$attr."' missing for '".$lemma."'\n";
  } else {
    $val_in = $$feat_ref{$attr};
  }
  
  my $val_out = "";
  if ( not exists($mmorph{$attr}) ) {
    $val_out = "";
    print STDERR "Warning! unknown attribute '".$attr."' in '".$lemma."'\n";
  } else {
    my $table = $mmorph{$attr};
    if ( $val_in ne "" and not exists($$table{$val_in}) ) {
      $val_out = $val_in;
    } else {
      $val_out = $$table{$val_in};
    }
  }

  my $attr_out = $attr;
  if ( exists($mmorph_names{$attr}) ) {
    $attr_out = $mmorph_names{$attr};
  }

  my $out = $attr_out."=".$val_out."+";
  return $out;
}

# ---------------------------------------------------------------------- 
# Converting from GenI to Mmorph 
# ---------------------------------------------------------------------- 

my $in_sentence = 0;
my $request     = "";
my @lemmas; # we save the lemmas into a list for later steps

while (<STDIN>) {
  my $lemma = "";
  my %has = ();
  my %hasav = ();
  my %feat = ();

  if (/^\s*$/) {
    next;
  } elsif (/(.*?)\s*\[(.*)\]/) {   # parse out values
    $lemma = $1;
    push @lemmas, $lemma;
    my $featstr = $2;
    # store the features into a hash
    # note: this is not feature -> attribute but a simple
    # hash with feat:attr as key
    %has = ();
    my $av = "";
    for (split(/ /, $featstr)) { 
      $av = $_;
      my ($attr, $val) = split(/:/,$av);
      $val = "" if ($val =~ /^[A-Z]/); # ignore variable values 
      $has{$attr} = 1;
      $hasav{$av} = 1;
      $feat{$attr} = $val;
    }
  }

  # control of spaces
  if ($lemma eq "----") { 
    $request .= $SENTENCE_SEPARATOR." ";
    $in_sentence = 0;
    next;
  } elsif ($in_sentence) {
  } else { # start new sentence
    $in_sentence = 1;
  }
    
  $request .= "'".$lemma;
  # translate the lemma and its features into an mmorph input
  foreach my $cat (@geni_cats) {
    if ($hasav{"cat:".$cat}) {
      my $cat_out = $mmorph{"cat"}{$cat};
      my @attrs   = @{ $mmorph_cat_to_attrs{$cat} };
      $request .= "+".$cat_out."[+";
      $request .= lookup_morph_list($lemma,\@attrs,\%has,\%feat);
      $request .= "]";
      last;
    } 
  }
  $request .= "' ";
}

# ---------------------------------------------------------------------- 
# Using the MMorph results 
# ---------------------------------------------------------------------- 

# run mmorph and read the results into a list of lists
my $command_out = "$mmorph_cmd $request";
print STDERR "$command_out\n";
my (@results,@tmp);
open(FROM_MMORPH,"$command_out|");
while (<FROM_MMORPH>) { 
  chomp;
  if (/^\s*$/) {
    push @results, [ @tmp ];
  } elsif (/^----/) {
    @tmp = ();
    push @tmp, $SENTENCE_SEPARATOR if (/$SENTENCE_SEPARATOR/);
  } else {
    push @tmp, $_;
  }
}

# consistency check
if ($#lemmas != $#results) {
  print STDERR "Error: number of input lemmas (".$#lemmas.") != number of mmorph results (".$#results.")\n";
  exit 1;
}

# convert the results list of list into the format GenI wants.
# Each sublist represents the set of possible inflections for 
# the given lemma; 
# FIXME: for now, we dumbly pick the first one and discard the rest
$in_sentence = 0;
my $final_out = "";
for my $i ( 0 .. $#lemmas ) {
  my $lemma = $lemmas[$i];
  my @sublist = @{$results[$i]};
  my $inflected_form = "";

  if ($#sublist < 0) { # no results from mmorph
    print STDERR "Warning: No inflected form for lemma '".$lemma."'\n";
    $inflected_form = $lemma;
  } else {
    $inflected_form = $sublist[0];
  }

  if ($inflected_form eq $SENTENCE_SEPARATOR) {
    $final_out .= "\n";
    $in_sentence = 0;
  } else {
    $final_out .= " " if ($in_sentence);
    $final_out .= $inflected_form;
    $in_sentence = 1;
  }
}

# write the output in GenI format
print "$final_out\n";
