#include <stdlib.h>
#include  <time.h>
#include <stdio.h>


int main(int argn, char ** argv) {
	int n = atoi(argv[1]);
	int i;
	srand(time(NULL));
	for(i = 0; i < n; i++)
	{
		int a = rand() % 51;
		int b = rand() % 15;
		int c = rand() % 15;
		int d = 2 * a + 3 * b + 4 * c + 6;
		d = 2 * c + 5;
		switch(a)
		{
			case 0: b = 10; break;
			case 1: b = 6; break;
			case 2: b = 4; break;
			case 3: b = 22; break;
			case 4: b = 100; break;
			default: b = 1; break;
		}
		
		printf("%d %d %d %d\n",a,b,c,d);
	}
	return 0;
}
