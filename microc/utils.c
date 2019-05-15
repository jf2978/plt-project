#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#define LIVE "o" // a black square
#define DEAD "."     // a space
#define clear() printf("\033[H\033[J")
#define get_symbol(i) (i == 0 ? DEAD : LIVE)
#define mat_entry(mat, n, i, j) (*((mat + i * n) + j))

// #define DEBUG

/* Given a board of Conway's Game of life, pretty print it.
 * @mat: the board
 * @m  : the width of the matrix
 * @n  : the height of the matrix
 * @sleep: The time interval between updates, in milliseconds
 */
void print_board(int* mat, int m, int n, int sleep) {
    int len = m * (n + 1) + 1;
    char buf[len];
    char *bp = buf;
    buf[len - 1] = 0;

    for(int i = 0; i < m; ++i) {
        for(int j = 0; j < n; ++j) {
            int entry = mat_entry(mat, n, i, j);
            if(j == n - 1) {
                sprintf(bp, "%s\n", get_symbol(entry));
                bp += 2;
            }
            else {
                sprintf(bp, "%s", get_symbol(entry));
                bp++;
            }
        }
    }
    clear();
    printf("%s", buf);
    fflush(stdout);
    usleep(sleep * 1000); // Sleep for 0.5s
}


/* Pretty-print out an integer matrix to stdout
 * @mat: the pointer to the starting address of the matrix
 * @m  : the width of the matrix
 * @n  : the height of the matrix
 */
void printm_int(int* mat, int m, int n) {
    printf("[\n");
    fflush(stdout);
    for(int i = 0; i < m; ++i) {
        for(int j = 0; j < n; ++j) {
            if(j == n - 1) {
                printf("%d;\n", *((mat+i*n) + j));
                fflush(stdout);
            }
            else {
                printf("%d, ", *((mat+i*n) + j));
                fflush(stdout);
            }
        }
    }
    printf("]\n");
}


/* Pretty-print out a float matrix to stdout
 * @mat: the pointer to the starting address of the matrix
 * @m  : the width of the matrix
 * @n  : the height of the matrix
 */
void printm_float(double* mat, int m, int n) {
    printf("[");
    fflush(stdout);
    for(int i = 0; i < m; ++i) {
        for(int j = 0; j < n; ++j) {
            if(j == n - 1) {
                printf("%f; ", *((mat+i*n) + j));
                fflush(stdout);
            }
            else {
                printf("%f, ", *((mat+i*n) + j));
                fflush(stdout);
            }
        }
    }
    printf("]\n");
}

/* Read in an integer matrix from a file designated by a path.
 * The input file should be a linear listing of entries in the
 * matrix in row-major order.
 * @path: the path to the input file
 * @mat : the pointer to the starting address of the output matrix
 * @row : the width of the matrix
 * @col : the height of the matrix
 */
int matread_int (char* path, int* mat, int row, int col){
	FILE* fd = fopen(path,"r");
	if(fd==NULL)
		return -1;
	int count = 0;
 	while(fread(mat, 1, 4, fd)==4){
 		mat++;
		count++;
		if(count==row*col){
			fclose(fd);
			return 0;
		}
	}
	fclose(fd);
	return -2;
}

/* Read in a float matrix from a file designated by a path.
 * The input file should be a linear listing of entries in the
 * matrix in row-major order.
 * @path   : the path to the input file
 * @mat    : the pointer to the starting address of the output matrix
 * @row    : the width of the matrix
 * @col    : the height of the matrix
 * @return : 0 on success, -1 on file-not-fount error
 */
int matread_float (char* path, double* mat, int row, int col){
	FILE* fd = fopen(path,"r");
	if(fd==NULL)
		return -1;
	int count = 0;
 	while(fread(mat, 1,sizeof(double), fd)==sizeof(double)){
		count++;
		mat++;
		if(count==row*col){
			fclose(fd);
			return 0;
		}
	}
 	fclose(fd);
	return -2;	// FIXME: file size not enough
}

/* Write to an int matrix from a file designated by a path.
 * The output file should be a linear listing of entries in the
 * matrix in row-major order.
 * @path   : the path to the output file
 * @mat    : the pointer to the starting address of the input matrix
 * @row    : the width of the matrix
 * @col    : the height of the matrix
 * @return : 0 on success, -1 on any I/O error
 */
int matwrite_int (char *path, int *mat, int row, int col) {
	FILE* fd = fopen(path, "w");
	if(fd == NULL)
		return -1;
    int size = row * col * sizeof(int);
    if(fwrite(mat, size, 1, fd) != 1) {
        printf("should not happen\n");
        return -1;
    }
    fclose(fd);
    return 0;
}

/* Write to an float matrix from a file designated by a path.
 * The output file should be a linear listing of entries in the
 * matrix in row-major order.
 * @path   : the path to the output file
 * @mat    : the pointer to the starting address of the input matrix
 * @row    : the width of the matrix
 * @col    : the height of the matrix
 * @return : 0 on success, -1 on any I/O error
 */
int matwrite_float (char *path, double *mat, int row, int col) {
	FILE* fd = fopen(path, "w");
	if(fd == NULL)
		return -1;
    int size = row * col * sizeof(double);
    if(fwrite(mat, size, 1, fd) != 1) {
        printf("should not happen\n");
        return -1;
    }
    fclose(fd);
    return 0;
}

int pbmread(char* path, int *mat, int row, int col){
    char buff[16];
    int c, x, y;
    FILE *fd = fopen(path,"r");
    //open file descriptor
    if(fd==NULL){
        fclose(fd);
        return -1;
    }
    //read image format
    if(!fgets(buff,sizeof(buff),fd)){//If you fail to read in file
        fclose(fd);
        return -1;
    }
    //Check the image format, pbm must be P1 or P4 (not too sure about the difference)
    if(buff[0]!='P' || (buff[1]!='1' && buff[1]!='4')){
        fclose(fd);
        return -1;
    }
    //Read until you skip the comments
    c = getc(fd);
    while(c == '#'){
        while(getc(fd) != '\n');
        c = getc(fd);
    }
    ungetc(c,fd);
    //read file size info
    if( fscanf(fd, "%d %d", &x, &y) !=2){ //Invalid file size
        fclose(fd);
        return -1;
    }
    //Check that the x and y match the file
    if(x!=row || y!=col){
        fclose(fd);
        return -1;
    }

    //I think this will move you to when the numbers actually start
    while (fgetc(fd) != '\n') ;

	int count = 0;
 	//while(fread(mat, 1, 4, fd)==4){
    while(1){
        fscanf(fd, "%d",mat);
 		mat++;
		count++;
		if(count==row*col){
			fclose(fd);
			return 0;
		}
	}
	fclose(fd);
	return -2;

}

int pgmread(char* path, int *mat, int row, int col){
    char buff[16];
    int c, x, y, d;
    FILE *fd = fopen(path,"r");
    //open file descriptor
    if(fd==NULL){
        fclose(fd);
        return -1;
    }
    //read image format
    if(!fgets(buff,sizeof(buff),fd)){//If you fail to read in file
        fclose(fd);
        return -1;
    }
    //Check the image format, pbm must be P1 or P4 (not too sure about the difference)
    if(buff[0]!='P' || (buff[1]!='2' && buff[1]!='5')){
        fclose(fd);
        return -1;
    }
    //Read until you skip the comments
    c = getc(fd);
    while(c == '#'){
        while(getc(fd) != '\n');
        c = getc(fd);
    }
    ungetc(c,fd);
    //read file size info
    if( fscanf(fd, "%d %d", &x, &y) !=2){ //Invalid file size
        fclose(fd);
        return -1;
    }
    //Check that the x and y match the file
    if(x!=col || y!=row){
        fclose(fd);
        return -1;
    }

    //Gotta read the depth component //For writing, we'll just assume it's 255
    if(fscanf(fd, "%d", &d) != 1){ //Failed to read the depth
        fclose(fd);
        return -1;
    }

    //I think this will move you to when the numbers actually start
    while (fgetc(fd) != '\n') ;

	int count = 0;
 	//while(fread(mat, 1, 4, fd)==4){
    while(1){
        fscanf(fd, "%d",mat);
 		mat++;
		count++;
		if(count==row*col){
			fclose(fd);
			return 0;
		}
	}
	fclose(fd);
	return -2;
}

int ppmread(char* path, int *mr, int *mg, int *mb, int row, int col){
    char buff[16];
    int c, x, y, d;
    FILE *fd = fopen(path,"r");
    //open file descriptor
    if(fd==NULL){
        fclose(fd);
        return -1;
    }
    //read image format
    if(!fgets(buff,sizeof(buff),fd)){//If you fail to read in file
        fclose(fd);
        return -1;
    }
    //Check the image format, pbm must be P1 or P4 (not too sure about the difference)
    if(buff[0]!='P' || (buff[1]!='3' && buff[1]!='6')){
        fclose(fd);
        return -1;
    }
    //Read until you skip the comments
    c = getc(fd);
    while(c == '#'){
        while(getc(fd) != '\n');
        c = getc(fd);
    }
    ungetc(c,fd);
    //read file size info
    if( fscanf(fd, "%d %d", &x, &y) !=2){ //Invalid file size
        fclose(fd);
        return -1;
    }
    //Check that the x and y match the file
    if(x!=row || y!=col){
        fclose(fd);
        return -1;
    }

    //Gotta read the depth component //For writing, we'll just assume it's 255
    if(fscanf(fd, "%d", &d) != 1){ //Failed to read the depth
        fclose(fd);
        return -1;
    }

    //I think this will move you to when the numbers actually start
    while (fgetc(fd) != '\n') ;

	int count = 0;
 	//while((fread(mr, 1, 4, fd)==4)&&(fread(mg,1,4,fd)==4)&&(fread(mb,1,4,fd)==4)){
    while(1){
        fscanf(fd, "%d",mr);
        fscanf(fd, "%d",mg);
        fscanf(fd, "%d",mb);
 		mr++;
 		mg++;
 		mb++;
		count++;
		if(count==row*col){
			fclose(fd);
			return 0;
		}
	}
	fclose(fd);
	return -2;
}

int pbmwrite(char* path, int *mat, int row, int col){
    FILE *fd = fopen(path,"w");
    //open file descriptor
    if(fd==NULL){
        fclose(fd);
        return -1;
    }

    //write the image format
    fprintf(fd, "P1\n");


    //Write a comment
    fprintf(fd, "#Autogenerated by MPL\n");

    //Write the file size
    fprintf(fd, "%d %d\n", row, col);

    //Write the actual bits in
    int count = 0;
    while(1){
        if(count%row != 0){
            fprintf(fd,"%d ", *mat);
        }
        else if(count%row==0){
            fprintf(fd,"%d\n",*mat);
        }
        count++;
        mat++;
        if(count==row*col){
			fclose(fd);
			return 0;
		}
    }

}

int pgmwrite(char* path, int *mat, int row, int col){
    FILE *fd = fopen(path,"w");
    //open file descriptor
    if(fd==NULL){
        fclose(fd);
        return -1;
    }

    //write the image format
    fprintf(fd, "P2\n");


    //Write a comment
    fprintf(fd, "#Autogenerated by MPL\n");

    //Write the file size
    fprintf(fd, "%d %d\n", col, row);

    //write the depth (assumed to be 255)
    fprintf(fd, "255\n");

    //Write the actual bits in
    int count = 0;
    while(1){
        if(count%row != 0){
            fprintf(fd,"%d ", *mat);
        }
        else if(count%row==0){
            fprintf(fd,"%d\n",*mat);
        }
        count++;
        mat++;
        if(count==row*col){
			fclose(fd);
			return 0;
		}
    }

	fclose(fd);
	return -2;
}

int ppmwrite(char* path, int *mr, int *mg, int *mb, int row, int col){
    FILE *fd = fopen(path,"w");
    //open file descriptor
    if(fd==NULL){
        fclose(fd);
        return -1;
    }

    //write the image format
    fprintf(fd, "P3\n");


    //Write a comment
    fprintf(fd, "#Autogenerated by MPL\n");

    //Write the file size
    fprintf(fd, "%d %d\n", row, col);

    //write the depth (assumed to be 255)
    fprintf(fd, "255\n");

    //Write the actual bits in
    int count = 0;
    while(1){
        if(count%row != 0){
            fprintf(fd,"%d ", *mr);
            fprintf(fd,"%d ", *mg);
            fprintf(fd,"%d ", *mb);
        }
        else if(count%row==0){
            fprintf(fd,"%d ", *mr);
            fprintf(fd,"%d ", *mg);
            fprintf(fd,"%d\n",*mb);
        }
        count++;
        mr++;
        mg++;
        mb++;
        if(count==row*col){
			fclose(fd);
			return 0;
		}
    }
	fclose(fd);
	return -2;
}

#ifdef DEBUG
int main() {
    int mat[3][3] = {
        {0, 0, 1},
        {0, 1, 0},
        {1, 0, 0},
    };
    print_board((int *)mat, 3, 3);
    return 0;
}
#endif
