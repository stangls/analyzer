// PARAM: --set ana.activated[0][+] "'file'" --enable  ana.file.optimistic

#include <stdio.h>

FILE *fp;

int main(){
	int b;
	fp = fopen("test.txt", "a");

	if (b)
		fclose(fp);
	else
		fprintf(fp, "Testing...\n");
	
	fclose(fp); // WARN: MAYBE closeing already closed file handle fp
}
