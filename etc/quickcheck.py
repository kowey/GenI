#!/usr/bin/env python
# vim: sw=4 sts=4 ai expandtab: 
# -*- mode: python; coding: utf-8 -*-
# 
# Produces instructions for ghci to run quickCheck on all the properties 
# of a literate haskell file.
#
# You run it like
#   ./quickcheck.py src/foo.lhs | ghci 
# Note: you might need your standard ghci arguments as well
#
# This is written in python (as opposed to haskell) because I couldn't figure
# out how to get the quickcheck script to run from the ghci intepreter.
#
# ToDo: 
# generalise to Bird style .lhs files and non .lhs files
# Eric Kow (kow at loria point fr)
# This software is released in the public domain.
# Do whatever you want with it.

import os
import re
import string
import sys

def processFile(filename):
    # open the file for reading
    try:
        file = open( filename, 'r' )	# open the file
    except:
        print "Error opening",filename
        return

    propPat = re.compile('(?P<p>^prop_.*?) ')
    beginCodePat = re.compile('\\\\begin\{code\}')
    endCodePat   = re.compile('\\\\end\{code\}')
    
    output = ":l " + filename + "\n" # will be added to below 
    inCodeblock = False # flag is set when we should pay attention 

    lines = file.readlines()			# read it
    for i in range(0,len(lines)):		# process it...
        if inCodeblock:
            m = propPat.search(lines[i]);
            if m != None:
                prop = m.group('p')
                output += "putStrLn \"" + prop + "\"\n"
                output += "QuickCheck.quickCheck " + prop + "\n"
            if endCodePat.match(lines[i]) != None:
                inCodeblock = False
        else: # if not inCodeblock
            m = beginCodePat.search(lines[i]);
            if m != None:
                inCodeblock = True
    
    print output 
# end of processFile() function

# main
if len(sys.argv) == 2:
    processFile(sys.argv[1])		# accept a command-line filename
else:
    print "usage: " + sys.argv[0] + " file.lhs"
    sys.exit(1)

