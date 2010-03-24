# Routines for HTML handling of an RFC
# load these after loading html-to-ascii.pl because they redefine some things.
# Gosh, it sure would be nice to have an object oriented language for this,
# so I didn't have to duplicate code in both files.

# Jim Davis, July 15 1994
# 3 Aug 94 changed META tag handling.

$lines_per_page = 58;
$columns_per_line = 72;

# Need this info to generate header lines.
$author = "(no author)";
$status = "Internet Draft";
$title =  "(no title)";
$date = "(no date)";

# The values are read from META elements in the HEAD, e.g.
# <META NAME="author" content="Davis, Lagoze">
# <META NAME="status" content="Internet Draft">
# <META NAME="title" content="Dienst">

# number of blank lines after header, before text.
$top_skip = 2;

# blank lines before footer
$bottom_skip = 2;

$bottom_margin = $lines_per_page - $bottom_skip - 1 ;

$End{"HEAD"} = "end_head";

sub end_head {
    local ($element) = @_;
    &set_header_variables_from_meta_tags();
    $ignore_text = 0;}

sub set_header_variables_from_meta_tags {
    $author = $Variable{"author"};
    $status = $Variable{"status"};
    $title = $Variable{"title"};
    $date = $Variable{"date"};}

# Called by tformat
sub do_header {
    local ($save_left) = $left_margin;
    local ($save_right) = $right_margin;
    $left_margin = 1; $right_margin = $columns_per_line;
    &print_lcr_line ($status, $title, $date);
    $left_margin = $save_left; $right_margin = $save_right;
    &print_blank_lines ($top_skip);}

sub do_footer {
    &print_blank_lines ($bottom_skip);
    local ($save_left) = $left_margin;
    local ($save_right) = $right_margin;
    $left_margin = 1; $right_margin = $columns_per_line;
    &print_lcr_line ($author, "", "[Page $page]");
    $left_margin = $save_left; $right_margin = $save_right;
    print "\014\n";
    $page++;}

$End{"BODY"} = "end_document";

sub end_document {
    local ($element) = @_;
    # might not want to fill the last page
    $fill_page_length = $flush_last_page;
    &finish_page ();}


1;
