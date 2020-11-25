# GFDL 1.2 Rosetta Code
# https://rosettacode.org/wiki/Read_a_file_character_by_character/UTF8#Python

def get_next_character(f):
    """Reads one character from the given textfile"""
    c = f.read(1)
    while c: 
        yield c
        c = f.read(1)
 
# Usage: 
with open("input.txt", encoding="utf-8") as f:
    for c in get_next_character(f):
        print(c, sep="", end="")
