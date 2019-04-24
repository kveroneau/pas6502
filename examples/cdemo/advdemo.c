#include <string.h>
#include <6502.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct test test;

struct test{
    char c;
    int i;
    char name[20];
    char age;
};

/* These should really go into an include file */
extern void testit(test *t);
extern test* testme(test *t);
extern void lf();

void showtest(test *t){
    puts(t->name);
    lf();
}

void showint(int i, char y){
    char buf[3];
    itoa(i, buf, y);
    puts(buf);
    lf();
}

int main(){
    struct test t; /* Place this onto our local C stack */
    struct test *tp; /* A Pointer we can use to allocate memory from heap. */
    struct test *tx; /* Ditto */
    puts("Program starting...\n");
    t.c = sizeof(test); /* Generate some test data. */
    t.i = 6500; /* Ditto */
    strcpy(t.name,"Kevin"); /* Why are strings so complicated in C? */
    t.age = 35; /* Is this my real age? */
    tp = malloc(sizeof(test)); /* Allocate memory from the heap. */
    tp->c = 65;
    BRK();
    tp->i = 1024;
    strcpy(tp->name,"Amy");
    tp->age = 26;
    showtest(tp);
    showtest(&t);
    testit(tp); /* Call Host application API call with C struct! */
    testit(&t); /* Ditto, but from C stack. */
    tx = malloc(sizeof(test));
    testme(tx); /* Have our Host application populate this C struct for us. */
    showtest(tx);
    BRK(); /* Tells the host application to give us a bit of debug output */
    return(0); /* We're done this demo! */
}

