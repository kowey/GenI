#!/usr/bin/env perl

my $readiness = 0;
my $ready = 0;

while (<STDIN>) {
  if (/COST/) {
    $readiness+=1;
    $ready=1 if $readiness==2;
    next;
  }

  if ($ready and /(\s*)([[:^space:]]*)\s*(.*)/) {   # parse out values
    my $levelstr = $1;
    my $name = $2;
    my $rest = $3;
    my $level = length $levelstr;

    # gruop spaces into a single tab
    $rest =~ s/\s+/\t/g;

    # check how much memory this function accounts for
    my $alloc = $rest;
    $alloc =~ s/.*\t//;

    # convert '.' to ',' for french excel
    $rest =~ s/\./,/g;

    # print out the nicely formatted string
    printf $levelstr.$name."\t".$level."\t".$rest."\n" unless $alloc lt 1;
  }

}


