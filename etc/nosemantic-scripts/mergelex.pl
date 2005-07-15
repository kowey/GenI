#!/usr/bin/perl -w

## merge semantic lexicon and syntactic lexicon into one file
## take two arguments, semantic and then syntactic.

## semantic file format: (n is category used for identification with syntax file)
##   hat n (Entity X Y ! agr:sg3)
##   semantics:[hat(_ Entity) x() y()]

## syntax file format:
##   hat    n Cn

## word (hat) and category (n) match up.
## there may be multiple matchups on both sides.
## for example:
##   hat n (A B ! agr:sg3)
##   semantics:[a() hat(_ A)]
## or:
##   hat n Pn

if( scalar(@ARGV) != 2 ) {
    die( "error, need two arguments: semantic_lexicon and syntactic_lexicon\n" );
}

## run through syntactic lexicon and store in hash?

open( SYNLEX, $ARGV[1] ) || die( "can't open file $ARGV[1]: $!" );
my %synlexicon = ();
#verify syntactic file?

#store in hash of arrays of families
# "$word $categ" => [ $family1, $family2, ... ]
while( $line = <SYNLEX> ) {
    if( $line =~ /(\w+)\s+(\w+)\s+(\w+)/ ) {
	my $word   = $1;
	my $categ  = $2;
	my $family = $3;
	my $famlistref = $synlexicon{"$word $categ"};
	if( $famlistref ) {
	    $famlistref->[scalar @{$famlistref}] = $family;
	} else {
	    $synlexicon{"$word $categ"} = [ $family ];
	}
    }
}

close( SYNLEX );

## read semantic lexicon and compare to hashmap.

open( SEMLEX, $ARGV[0] ) || die( "can't open file $ARGV[0]: $!" );

$word = "";
$param = "";
@famlist = [];
while( $line = <SEMLEX> ) {
    if( $line =~ /^(\w+)\s(\w+)\s+(.*)/ ) {
	$word  = $1;
	my $categ = $2;
	$param = $3;
	my $key   = "$word $categ";
	my $famlistref = $synlexicon{$key};
	die( "non existent entry: $key" ) unless $famlistref;
	@famlist = @{$famlistref};
    } elsif( $line =~ /semantics:.*/ ) {
	foreach $fam (@famlist) {
	    print "$word $fam $param\n";
	    print $line;
	}
    } else { print $line; }
}

close( SEMLEX );
