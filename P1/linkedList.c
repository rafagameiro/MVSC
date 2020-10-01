#include <stdio.h>
#include <stdlib.h>
#include "list.h"

struct Node {
    
    int number;
    struct Node* next;

};

struct Node* head = NULL;
struct Node* tail = NULL;

struct Node* getMiddlePrev(struct Node* start, struct Node* last)
{
    if(start == NULL)
        return NULL;

    printf("getMiddlePrev method\n");    
    struct Node* previous = start;
    struct Node* middle = start->next;
    struct Node* fast = start->next->next;
    
    if(start == head) {
        previous = NULL;
        middle = start;
        fast = start->next;
    } 
    
    printf("fast %d\n", fast->number);
    if(last != NULL) {
        printf("got in!\n");
        printf("last %d\n", last->number);
    }
    while(fast != last) {
        
        fast = fast->next;
        if(fast != last) {
            previous = middle;
            middle = middle->next;
            fast = fast->next;
        }
    }
    
    return previous;
}

struct Node* binarySearch(int element)
{

    struct Node* start = head;
    struct Node* last = tail;
    struct Node* previous = NULL;
    do {
        previous = getMiddlePrev(start, last);
        printf(">>>middle %d\n", previous->next->number);        
        if(previous->next->number == element)
            break;

        if(previous->next->number > element) {
            last = previous->next;
        }

        if(previous->next->number < element) {
            start = previous->next->next;
        }

    } while(last == NULL || last != start);

    return previous;
}

void addNode(int element)
{

    struct Node* node;
    struct Node* previous;
    struct Node* current;
    node = (struct Node*) malloc(sizeof(struct Node));
    previous = NULL;
    current = head;

    while(current != NULL && current-> number < element) {
        previous = current;
        current = previous->next;
    }

    if(previous != NULL)
        previous->next = node;

    node->number = element;
    node->next = current;
    
    if(previous == NULL || head == NULL)
        head = node;

    if(current == NULL)
        tail = node;
}

void printList()
{

    printf("List:\n");
    printf("%d ", head->number);
    
    struct Node* current = head->next;
    while(current != NULL) {
        printf("-> %d ", current->number);
        current = current->next;
    }
    printf("\n");

    struct Node* prev = binarySearch(7);
    printf("previous middle %d\n", prev->number);
}
