//Creates data for trials 
//usage: coltest num_rows num_deps dep_type num_deps dep_type ...
//where each num_deps specifies a number of dependencies and
//each dep_typ specifies the type 0 for a random number 1 
//for a a -> b dep, 2 for {a,b}->c etc
//Doesn't do error checking so a Bus Error means the arguments
//were wrong
#include <stdlib.h>
#include <time.h>
#include <stdio.h>
#include <math.h>

int range(int rows, int cols)
{
   double m = (double) rows;
   double c = (double) cols;
   double p = (- log(1 / (m * (m - 1))) - 1.790335)/c;
   double a = exp(p);
   return round(a);
}
int main(int argn, char ** argv) {
	int n_cols = atoi(argv[1]);
	int rows = 0;
	int i,j,k,l;
	for(i = 2; i < argn; i+=2)
	 rows += atoi(argv[i]) * (atoi(argv[i+1]) + 1);
	int rand_range = range(n_cols,rows);
	if(rand_range <= 1)
	
	{ fprintf(stderr,"TextIO.output(TextIO.stdErr,\"~~~RAND RANGE IS TOO SMALL~~~\");"); return 0; }
	srand(time(NULL));
	for(i = 0; i < n_cols; i++) {
		for(j = 2; j < argn; j+=2)
		{
			int num_deps = atoi(argv[j]);
			int dep_size = atoi(argv[j+1]);
			for(k = 0; k < num_deps; k++)
			{
				int sum = 0;
				for(l = 0; l < dep_size; l++)
				{
					int n = rand() % (rand_range);
					sum += (l+1) * n;
					printf("%d ",n);
				}
				sum = sum % rand_range; 
				if(dep_size > 0)
					printf("%d ",sum);
				else 
					printf("%d ", rand() % (rand_range));
			}
		}
		printf("\n");
	}

	fprintf(stderr,"Trials.go(\"%d\",%d);\nOS.Process.exit(OS.Process.success);",n_cols,rows); 
	return 0;
}
