#include <time.h>
#include <stdlib.h>
#include <stdio.h>

void unifySets(int** maze, int x, int y, int toChange, int changeTo);


//the program is explained at the end.

int main(){

    int ** maze = (int**)malloc(30*sizeof(int*)); //a 30X30 maze
    for(int i = 0; i< 30; ++i){
        *(maze+i) = (int*)malloc(30*sizeof(int));
    }
    for(int i = 0; i< 30; ++i){
        for(int j = 0; j< 30; ++j){
            maze[i][j] = i + 30*j;
        }
    }
    
    char** graphicMaze = (char**)malloc(121*sizeof(char*));
    for(int i = 0; i< 121; ++i){
        *(graphicMaze+i) = (char*)malloc(121*sizeof(char));
    }
    for(int i = 0; i< 121; ++i){ //121 = 30*4+1 (length/height*4chars, +1 for the borders)
        for(int j = 0; j< 121; ++j){
            graphicMaze[i][j] = ' ';
            if(i%4==0 && j%4==0){
                graphicMaze[i][j]='O';
            }
            else if(i%4==0 || j%4==0){
                graphicMaze[i][j]='#';
            }
        }
    }
    

    int boolean_done = 0;
    int checkIfDoneCounter = 0; //used for efficiency. We'll check if we're done only once every 50 edge-removing attemps.
    srand(time(NULL)); //not actually sure what this does, I need to read-up... but apparently it's needed once when you use rand()

    while(!boolean_done){
        int pos1_x = rand()%30;
        int pos1_y = rand()%30;
        int direction_for_pos2 = rand()%4;
        int pos2_x;
        int pos2_y;

        if(direction_for_pos2==0){
            pos2_x = pos1_x;
            pos2_y = pos1_y+1;
        }
        else if(direction_for_pos2==1){
            pos2_x = pos1_x+1;
            pos2_y = pos1_y;
        }
        else if(direction_for_pos2==2){
            pos2_x = pos1_x;
            pos2_y = pos1_y-1;
        }
        else if(direction_for_pos2==3){
            pos2_x = pos1_x-1;
            pos2_y = pos1_y;
        }
        /*in the last four if/else if we might've accidentally set pos2_x or pos2_y to an illegal value. 
        If so, in the next four if's we'll adjust the value of pos2_x or pos2_y to a legal one (obviously it depends which is the illegal one...)*/
        if(pos2_x==-1)pos2_x= pos1_x+1;
        if(pos2_x==30)pos2_x= pos1_x-1;
        if(pos2_y==-1)pos2_y= pos1_y+1;
        if(pos2_y==30)pos2_y= pos1_y-1;
        
            
        if(maze[pos1_x][pos1_y] != maze[pos2_x][pos2_y]){
            unifySets(maze, pos2_x, pos2_y, maze[pos2_x][pos2_y] ,maze[pos1_x][pos1_y]);
            
            //draw:
            if(abs(pos1_x-pos2_x)){//if you're removing a '|' edge
                if(pos1_x>pos2_x){ //swap
                    int tmp = pos1_x;
                    pos1_x=pos2_x;
                    pos2_x=tmp;
                }
                for(int i = 1+4*pos1_y;    i <= 3+4*pos1_y;    ++i){
                    graphicMaze[4*pos2_x][i]=' ';
                }
            }
            else{//if you're removing a '-' edge
                if(pos1_y>pos2_y){ //swap
                    int tmp = pos1_y;
                    pos1_y=pos2_y;
                    pos2_y=tmp;
                }
                for(int i = 1+4*pos1_x;    i <= 3+4*pos1_x;    ++i){
                    graphicMaze[i][4*pos2_y]=' ';
                }
            }
            
        }
        ++checkIfDoneCounter;//each time it's intialized to zero, then 1 is added here, so the result is that it de-facto runs from 1 to 50.
        if(checkIfDoneCounter==50){
            boolean_done = 1;
            int compare = maze[0][0];
            for(int i = 0; i<30; ++i){
                for(int j = 0; j<30; ++j){
                    if(compare != maze[i][j])boolean_done=0;
                }
            }
            checkIfDoneCounter = 0;
        }

    }
    
    FILE* lm; 
    lm = fopen("last_maze.txt", "w");
    
    /////////////////////////
    //////////PRINT://///////
    /////////////////////////
    
    printf("\n");
    for(int i = 0; i<121; ++i){
        for(int j = 0; j<121; ++j){
            printf("%c ",graphicMaze[i][j]);
            fprintf(lm,"%c ",graphicMaze[i][j]);
        }
        printf("\n");
        fprintf(lm,"\n");
    }
    printf("\n");
    
    fclose(lm);
    
    return 0;
}

void unifySets(int** maze, int x, int y, int toChange, int changeTo){
    if(maze[x][y] == toChange){
        maze[x][y] = changeTo;
        if(y<29){
            unifySets(maze, x, y+1, toChange, changeTo);
        }
        if(x<29){
            unifySets(maze, x+1, y, toChange, changeTo);
        }
        if(0<y){
            unifySets(maze, x, y-1, toChange, changeTo);
        }
        if(0<x){
            unifySets(maze, x-1, y, toChange, changeTo);
        }
    }
}
/*

Intuition behind the algorithm:
    1. We create a 30X30 grid.
    2. We assigen for each block in the grid a unique number.
    3. While(not all blocks have the same number){
        3.a. Choose a block randomly. ("b1")
        3.b. Choose one of its 4 sorrounding blocks randomly ("b2")
        3.c. If (the two have different numbers){
            3.c.1. change all the blocks that share the same number with b1
                    so that they contain the number in b2 
            3.c.2. remove the edge (border of the grid) that seperates the two blocks
        }
    }
    4. remove the numbers from the maze (unrequired in my implementation...).
    5. display the grid: it is now a maze, and you can reach any point in it from any other point in it.

Notes:
    *We could've used a bigger/smaller grid in step1.
    *We could've reversed "b1" and "b2" in step 3.c.2, it doesn't matter.

Matching the intuition with the implementation:
    We impement the maze with two arrays:
        1. a two-dimensional int array that remembers which number is contained by each block 
        2. a two-dimensional char array that serves as the graphical representation
    The second array ("graphicMaze" in implementation) is initialized as a "grid", and each time removes the appropriate edge - when needed.
    Our while loop (step 3.) is performed until the first array ("maze" in implementation) contains the same number in all positions.
    
 */