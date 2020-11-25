# GFDL 1.2 Rosetta Code 
# https://rosettacode.org/wiki/Read_a_file_character_by_character/UTF8#Perl

binmode STDOUT, ':utf8';  # so we can print wide chars without warning
 
open my $fh, "<:encoding(UTF-8)", "input.txt" or die "$!\n";
 
while (read $fh, my $char, 1) {
    printf "got character $char [U+%04x]\n", ord $char;
}
 
close $fh;
