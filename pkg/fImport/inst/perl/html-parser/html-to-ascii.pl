#!/bin/env perl

# Program to generate ASCII text for HTML
# Created by  James R. Davis, July 15 1994

# get directory where this file is.  
{$0 =~ /^(.*)\/.*$/;  $my_dir = $1; 
 if ($my_dir !~ ?^/?) {$my_dir = $ENV{PWD} . "/" . $my_dir;}
 if ($my_dir =~ ?/$?) {chop ($my_dir);}}
push(@INC, $my_dir);

# Parse command line arguments.
$file = "";

while ($#ARGV >=0) {
    $arg = shift;
    if ($arg =~ /^-./) {
	if ($arg =~ /-width/) {
	    if ($#ARGV == -1) {die "Missing value for $arg\nStopped ";}
	    $columns_per_line= shift;}
	else {
	    die "Unrecognized switch $arg.\nStopped";}}
    else {
	$file = $arg;
    }}

if ($file eq "") {
    die "Missing argument (HTML input file)\n";}

require "parse-html.pl" || die "Could not load parse-html.pl";
require "html-ascii.pl" || die "Could not load html-ascii.pl";

&parse_html ($file);

