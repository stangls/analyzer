// PARAM: --set ana.activated[0][+] "'file'" --enable  ana.file.optimistic

#include <stdio.h>

FILE *fp;

int main(){
	int i;
	fp = fopen("test.txt", "a");

	while (i){
		fprintf(fp, "Testing...\n");
		i++;
	}

	fclose(fp);
}

// All ok.
