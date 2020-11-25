/* GFDL 1.2 RosettaCode
 * https://rosettacode.org/wiki/Read_a_file_line_by_line#C
/* From manpage for "getline" */
 
#include <stdio.h>
#include <stdlib.h>
 
int main(void)
{
	FILE *stream;
	char *line = NULL;
	size_t len = 0;
	ssize_t read;
 
	stream = fopen("input.txt", "r");
	if (stream == NULL)
		exit(EXIT_FAILURE);
 
	while ((read = getline(&line, &len, stream)) != -1) {
		printf("%s", line);
	}
 
	free(line);
	fclose(stream);
	exit(EXIT_SUCCESS);
}
