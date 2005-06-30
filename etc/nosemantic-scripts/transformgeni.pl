#!/usr/bin/perl -w

# input utf-8, output utf-8
# transforms files to be used in geni
# removes accents because geni cannot handle accents yet
# lowercases categories because uppercase indicates variable in geni.


$charset = "UTF-8";
if( scalar(@ARGV) < 1 || scalar(@ARGV) > 2 ) {
    die( "error, need one argument: <accented file>\n in UTF-8!" );
} elsif( scalar(@ARGV) == 2 ) {
    $temp = $ARGV[1];
    if( $temp == "UTF-8" || $temp == "ISO-5589-1" ) {
	$charset = $temp;
    }
}

open( IN, $ARGV[0] ) || die( "can't open $ARGV[0] $!" );

while( defined( $line = <IN> ) ) {
    $line = rmAccents($line);
    $line = lcCategs($line);
    print $line;
}

close( IN );

sub rmAccents {
    ($line) = @_;
    $line =~ s/à/a/g;
    $line =~ s/â/a/g;
    $line =~ s/ä/a/g;
    $line =~ s/ç/c/g; # don't have uppercase version
    $line =~ s/é/e/g;
    $line =~ s/è/e/g;
    $line =~ s/ê/e/g;
    $line =~ s/ë/e/g;
    $line =~ s/î/i/g;
    $line =~ s/ï/i/g;
    $line =~ s/ñ/n/g;
    $line =~ s/ô/o/g;
    $line =~ s/ö/o/g;
    $line =~ s/ù/u/g;
    $line =~ s/û/u/g;
    $line =~ s/ü/u/g;
    $line =~ s/À/A/g;
    $line =~ s/Â/A/g;
    $line =~ s/Ä/A/g;
    $line =~ s/É/E/g;
    $line =~ s/È/E/g;
    $line =~ s/Ê/E/g;
    $line =~ s/Ë/E/g;
    $line =~ s/Î/I/g;
    $line =~ s/Ï/I/g;
    $line =~ s/Ñ/N/g;
    $line =~ s/Ô/O/g;
    $line =~ s/Ö/O/g;
    $line =~ s/Ù/U/g;
    $line =~ s/Û/U/g;
    $line =~ s/Ü/U/g;
    return $line;
}

sub lcCategs {
    ($line) = @_;
    if( $line =~ m /\[cat:.*\]/ ) {
	$line =~ s/([a-zA-Z]*)/lc($1)/ge;
    }
    return $line;
}
