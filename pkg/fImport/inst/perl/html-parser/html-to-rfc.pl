#!/bin/env perl

# Program to generate ASCII text for an Internet Draft
# according to the standards in
# ftp://nis.nsf.net/internet/documents/ietf/1id-guidelines.txt
# ftp://nis.nsf.net/internet/documents/rfc/rfc1543.txt
#
# Created by  James R. Davis, July 15 1994

# get directory where this file is.  
{$0 =~ /^(.*)\/.*$/;  $my_dir = $1; 
 if ($my_dir !~ ?^/?) {$my_dir = $ENV{PWD} . "/" . $my_dir;}
 if ($my_dir =~ ?/$?) {chop ($my_dir);}}
push(@INC, $my_dir);


# Parse command line arguments.
$file = "";
$flush_last_page = 1;

while ($#ARGV >=0) {
    $arg = shift;
    if ($arg =~ /^-./) {
	if ($arg =~ /-flush/) {
	    if ($#ARGV == -1) {die "Missing value for $arg\nStopped ";}
	    # If 0, don't add newlines to the last page.
	    # Hmm, might not want page numbering at all in that case...
	    $flush_last_page = shift;
	}
	else {
	    die "Unrecognized switch $arg.\nStopped";}}
    else {
	$file = $arg;
    }}

if ($file eq "") {
    die "Missing argument (HTML input file)\n";}

require "parse-html.pl" || die "Could not load parse-html.pl";
require "html-ascii.pl" || die "Could not load html-ascii.pl";
require "rfc.pl" || die "Could not load rfc.pl";

&parse_html ($file);


