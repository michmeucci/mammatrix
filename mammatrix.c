#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>

#define IN 1
#define OUT 0

struct matrix{
    size_t rows;
    size_t cols;
    double *data;
};

void f(double x); /*truncate a double */
char get_dim(char *file, size_t *r, size_t *col);
void read_matrix(char *file, struct matrix *A, struct matrix *B);
void print_matrix(struct matrix *A);
void multiply(struct matrix *A, struct matrix *B, struct matrix *C);
void add(struct matrix *A, struct matrix *B, struct matrix *C, int n);
int determinant(int n, struct matrix *A);
struct matrix makematrix(int a, int b);
void scalar_product(double n, struct matrix *B); /*product of a matrix by a scalar*/

int main (int argc, char *argv[])
{
    size_t rows[2]= {0,0};
    size_t cols[2]= {0,0};
    char op; /* type of operation */

    op = get_dim(argv[1], rows, cols);   

    struct matrix A;
    struct matrix B;
    struct matrix C;
    
    A = makematrix(rows[0], cols[0]);
    B = makematrix(rows[1], cols[1]);
        
    A.data = malloc(sizeof(double) * A.rows * A.cols);
    B.data = malloc(sizeof(double) * B.rows * B.cols); 
       
    read_matrix(argv[1],&A,&B);
    
    print_matrix(&A);
    
    if(op =='?'){
        printf("%d\n", determinant(A.rows, &A));
        free(A.data);
        free(B.data);
        return 0;
    }
    
    if(A.rows==1 && A.cols==1 && op=='*'){
        printf("%c\n", op);
        print_matrix(&B);
        printf("=\n");
        scalar_product(*(A.data),&B);
        print_matrix(&B);
        free(A.data);
        free(B.data);
        return 0;
    }
    
    C = makematrix(rows[0], cols[1]);
    C.data = malloc(sizeof(double) * A.rows * B.cols);    

    printf("%c\n", op);
    print_matrix(&B);
    printf("=\n");
    
    switch(op){
        case '*':
            multiply(&A,&B,&C);
            break;
        case '+': 
            add(&A,&B,&C,1);
            break;
        case '-': 
            add(&A,&B,&C,-1);
            break;    
    }   
   
    print_matrix(&C);
    
    free(A.data);
    free(B.data);
    free(C.data);
    
    return 0;
}

void read_matrix(char *file, struct matrix *A, struct matrix *B){
    
    int i,j;
    FILE *fp;
    int c=1;
   
    if((fp = fopen(file, "r")) != NULL ){
         
        for(i=0; i < A->rows; i++)
            for(j=0; j < A->cols; j++)
                fscanf(fp, "%lf", &A->data[i * A->cols + j]);
                 
        /*skip the character operator line */
        if(B->rows !=0){
            while(!isdigit(c))
                c=fgetc(fp);
            
            ungetc(c,fp);
        
            for(i=0; i < B->rows; i++)
                for(j=0; j < B->cols; j++)
                    fscanf(fp, "%lf", &B->data[i * B->cols + j]);
        }
    }
    fclose(fp);
}

char get_dim(char *file, size_t *r, size_t *col)
{    
    FILE *fp;
    double a;
    int c =1;
    int n = OUT;
    char op = '?';
    
    if((fp = fopen(file, "r")) == NULL ){
        fprintf(stderr, "matrix: I cannot open %s\n",file);
        exit(1);
    }
    
    while(fscanf(fp,"%lf",&a)){
        
            if(n==OUT)
                col[0]++;
        
            c=fgetc(fp);
            
            if(c ==']'){
                r[0]++;
                break;
            }
            
            else if(isdigit(c))
                ungetc(c,fp);      
            
            else if(c =='\n'){
                r[0]++;
                n=IN;
            }
    }
    
    if (c != ']'){
        
    fscanf(fp,"%c\n",&op);
    
    n=OUT;   

    while(fscanf(fp,"%lf",&a)){
        
        if(n==OUT)
            col[1]++;
        
        c=fgetc(fp);
            
        if(isdigit(c))
            ungetc(c,fp);
            
        else if(c =='\n'){
            r[1]++;
            n=IN;                
        }
        
        else if(c == ']'){
                r[1]++;
                break;    
        }
    }
}
    
    fclose(fp);
    return op;
} 

void print_matrix(struct matrix *A){
    
    int i,j;

/*printing the matrices*/

     double *tmp = A->data;
     
     for(i=0; i < A->rows; i++){
        for(j=0; j < A->cols; j++){
                f(*(tmp++));
        }
        printf("\n");
    }    
}

void multiply(struct matrix *A, struct matrix *B, struct matrix *C) 
{ 
    int i, j, k;
    
    /*initialize C to 0*/
    
   for (i=0; i< C->rows; i++){
        for (j=0; j < C->cols; j++)
            C->data[i * C->cols + j]=0;
   }
// Multiplying matrix A and B and storing in C.
   for(i = 0; i < A->rows; ++i)
        for(j = 0; j < B->cols; ++j)
            for(k=0; k < A->cols; ++k)
                C->data[i * C->cols + j] += A->data[i * A->cols + k] * B->data[k * B->cols + j];
}

void f(double x)
{
    double i,f= modf(x,&i);
    
    if(f<.00001)
        printf("%.f ",i);
    
    else printf("%f ",x);    
}

/* Adding matrix A and B and storing in C*/
void add(struct matrix *A, struct matrix *B, struct matrix *C, int n) 
{ 

    int i, j;
    
    for (i=0; i< C->rows; i++)
        for (j=0; j < C->cols; j++)
            C->data[i * C->cols + j] = A->data[i * A->cols + j] + n *B->data[i * B->cols + j];
}

int determinant(int n, struct matrix *A)
{                                    
    int i,j,i_count,j_count, count=0;
    
    int det=0;
 
	if(n < 1)
	{
		printf("Wrong dimension");
		return 1;
	}
	
	if(n==1) return A->data[0];
    
    if(n==2) return (A->data[0]* A->data[1 * A->cols + 1] - A->data[0 * A->cols + 1] * A->data[1 * A->cols + 0]);
    
    struct matrix D;
    
    D.rows = A->rows-1;
    D.cols = A->cols-1;
    
    D.data = malloc(sizeof(double) * (A->rows-1) * (A->cols-1));
 
	for(count=0; count < n; count++)
	{
		//Creating array of Minors
		i_count=0;
		for(i=1; i<n; i++)
		{
			j_count=0;
			for(j=0; j<n; j++)
			{
				if(j == count)
                    continue;
				D.data[i_count * D.cols + j_count] = A->data[i * A->cols + j];
				j_count++;
			}
			i_count++;
		}
		det += pow(-1, count) * A->data[0 * A->cols + count] * determinant(n-1,&D);	//Recursive call
	}
 	free(D.data);
	return det;
}

struct matrix makematrix(int a, int b)
{
    struct matrix temp;
    
    temp.rows = a;
    temp.cols = b;
    
    return temp;
}

void scalar_product(double n, struct matrix *B)
{
     int i,j;
     for(i=0; i < B->rows; i++)
            for(j=0; j < B->cols; j++)
                 B->data[i * B->cols + j] = n * B->data[i * B->cols + j];
}
