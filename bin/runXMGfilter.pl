: # *-*-perl-*-*
eval 'exec perl -w -S  $0 ${1+"$@"}' 
  if 0;  # if running under some shell

# ----------------------------------------------------------------------  
# What's this?
#
# An alternative to GenI's runXMGselector - it acts as a filter by
# using the GenI lexical selection to produce and compile the 
# metagrammar valuations list.  The purpose of this script is help
# the grammar hacker avoid recompiling the entire XMG metagrammar
# everytime she makes an edit.  Only the relevant subset is compiled.
#
# It tries to be reasonably clever by only compiling the metagrammar
# if there have been any modifications.
#
# ----------------------------------------------------------------------  
# How do I use it?
#
# Run GenI from this directory, passing in addition to your regular
# commands -t stub_file selectcmd=runXMGfilter.pl
#
# Note: 
# 1. you need to have an actual working XMG metagrammar 
#    all this script does is produce a list XMG values
# 2. the stub file should be in the same directory as 
#    your XMG metagrammar... it should consist of some
#    XMG includes 
# ----------------------------------------------------------------------  

use strict;
use File::Basename;

# command line arguments
if ($#ARGV != 0) {
  printf STDERR "usage: runXMGfilter.pl grammar_file\n";
  exit 1;
}
# infer the GRAMMAR_DIR from the grammar_file
my $STUB=$ARGV[0];
my $GRAMMAR_DIR=dirname($STUB);

# environment stuff
my $USERNAME=`whoami`; chomp $USERNAME;
my $TMP_FILE_PREFIX="/tmp/geni_to_sel-$USERNAME";
my $TMP_RES_PREFIX="/tmp/geni_from_sel-$USERNAME";
my $TMP_MG_FILE_PRESTUB="filtered-$USERNAME";

my $TMP_MG_FILE_STUB="filtered-$USERNAME-$$";
my $TMP_MG_FILE="$GRAMMAR_DIR/$TMP_MG_FILE_STUB.mg";
my $TMP_FIL_FILE="$TMP_FILE_PREFIX-$$.fil";
my $TMP_RES_FILE="$TMP_RES_PREFIX-$$.geni";

# the latest results
my $LATEST_MG="$GRAMMAR_DIR/filtered-latest.mg";
my $LATEST_REC="$GRAMMAR_DIR/filtered-latest.rec";

# delete previous results
my @delete_me = glob "$GRAMMAR_DIR/$TMP_MG_FILE_PRESTUB* $TMP_FILE_PREFIX* $TMP_RES_PREFIX*";
unlink @delete_me;

# have the mg source files been modified since the last 
# grammar generated?
my $is_mg_modified = 1;
if (-e $LATEST_REC) {
  $is_mg_modified = 0;
  my $rec_mod_age = -M $LATEST_REC;
  for my $mg_file (glob "$GRAMMAR_DIR/*.mg") {
    if (-M $mg_file < $rec_mod_age) {
      $is_mg_modified = 1;
      last;
    }
  }
}

# print out the stub contents
open MG, ">$TMP_MG_FILE";
open STUB, $STUB;
while (<STUB>) { print MG; }
close STUB;

# print out all the values
open FIL, ">$TMP_FIL_FILE";
my @values = ();
while (<STDIN>) {
  print FIL;
  chomp;
  if (/family:(.*).*]/) {
    my $family = $1;
    push @values, "value $family\n";
  }
}
# remove duplicate entries
my %seen = ();
my @uniq_values = grep { ! $seen{$_} ++ } @values;
# print all values to the MG file
foreach my $v (@uniq_values) { print MG $v; }
close FIL;
close MG;

# is there any difference between the last metagrammar
# generated and this one?
my $the_diff=`diff $TMP_MG_FILE $LATEST_MG`;
my $CURRENT_REC;

# if either one of the mg source files has been modified
# or we are using a different set of values, recompile 
# the metagrammar
if ($is_mg_modified or $the_diff ne "") {
  # generate the grammar and perform XMG selection
  my $TMP_REC="$TMP_MG_FILE_STUB.tmp.rec";
  my $the_cmd = "";
  $the_cmd .= "cd $GRAMMAR_DIR;";
  $the_cmd .= "MetaTAG $TMP_MG_FILE_STUB.mg --chk -c $TMP_REC 1>&2;";
  $the_cmd .= "CheckTAG $TMP_REC --chk -c $TMP_MG_FILE_STUB.rec 1>&2;";
  system $the_cmd;

  # remove the old copies of the latest mg/rec files
  unlink $LATEST_MG if (-e $LATEST_MG);
  unlink $LATEST_REC if (-e $LATEST_REC);

  # copy the latest results over for safe keeping
  link "$GRAMMAR_DIR/$TMP_MG_FILE_STUB.mg",  $LATEST_MG;
  link "$GRAMMAR_DIR/$TMP_MG_FILE_STUB.rec", $LATEST_REC;
}
  

# run the selector
system "SelectTAG $LATEST_REC $TMP_FIL_FILE --geni -o $TMP_RES_FILE > /dev/null;";

# print the results to stdout
open SELECTED, $TMP_RES_FILE;
while (<SELECTED>) { print };
close SELECTED
