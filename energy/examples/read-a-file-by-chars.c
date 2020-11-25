/* GFDL 1.2 Rosetta Code
 * https://rosettacode.org/wiki/Read_a_file_character_by_character/UTF8#C
 */
#include <stdio.h>
#include <wchar.h>
#include <stdlib.h>
#include <locale.h>
 
int main(void)
{
    /* If your native locale doesn't use UTF-8 encoding 
     * you need to replace the empty string with a
     * locale like "en_US.utf8"
     */
    char *locale = setlocale(LC_ALL, "");
    FILE *in = fopen("input.txt", "r");
 
    wint_t c;
    while ((c = fgetwc(in)) != WEOF)
        putwchar(c);
    fclose(in);
 
    return EXIT_SUCCESS;
}
