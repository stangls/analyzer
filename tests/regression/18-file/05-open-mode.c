// PARAM: --set ana.activated[0][+] "'file'" --enable  ana.file.optimistic

#include <stdio.h>

FILE *fp;

int main(){
	fp = fopen("test.txt", "r");
	fprintf(fp, "Testing...\n"); // WARN: writing to read-only file handle fp
	fclose(fp);
}
