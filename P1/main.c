#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "list.h"

void generateList(int iterations) 
{

    srand(time(NULL));
    for(int i = 0; i < iterations; i++) {
        // formula = rand() % ((nMax + 1) - nMin) + nMin;
        int randnumber = rand() % 11 + 0;
        addNode(randnumber);
    }

}

int main(int argc, char * argv[]) 
{

    if(argc != 2) 
    {
        printf("ERROR: Invalid number of arguments.\n");
        printf("Please only specify the number of elements the list will have.\n");
        return 1;
    }

    generateList(atoi(argv[1]));
    printList();

    return 0;

}
