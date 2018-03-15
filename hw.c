#include <stdio.h>

const int BIT_WIDTH = 32;
const int debug = 0;

int ip = 0;
int mem[8192] = {0};

void displayMem (void) {
    for (int i = 0; i < 34; i++) {
        if (ip == i) {
            printf ("----v");
        }
        else {
            printf ("     ");
        }
    } 
    printf ("ip: %d\n", ip);

    for (int i = 0; i < 34; i++) {
        printf ("%5d", mem[i]);
    }
    printf (" ||");
    for (int i = 0; i < 7; i++) {
        printf ("%5d", mem[i + 127]);
    }
    printf ("\n");
}

const int HELLO = 0;
const int WORLD = 1;
const int BRANCH_AND_HALT = 2;
const int CONTINUE_AND_HALT = 3;
const int SUBLEQ = 4;
const int HALT = 5;


int get (addr) {
    return addr >= 0 ? mem[addr] : getchar ();
}

void set (addr, value) {
    if (addr >= 0) {
        mem[addr] = value;
    }
    else {
        putchar (value);
    }
}

int subleq (a, b, c) {
    debug ? printf ("ip: %d, a: %d, b: %d, c: %d\n", ip, a, b, c) : 0;
    int aDeref = get(a);
    int bDeref = b >= 0 ? mem[b] : 0;
    int newB = bDeref - aDeref;

    set (b, newB);
    
    if (newB <= 0) {
        if (c < 0) {
            ip += 3;
            return HALT;
        }
        else {
            ip = c;
            return SUBLEQ;
        }
    }
    else {
        ip += 3;
        return SUBLEQ;
    }
}

int h (int a, int b, int c) {
    
    int aDeref = get(a);
    int bDeref = get(b);
    int bit = aDeref % BIT_WIDTH;
    int mask = 0x01 << bit;
    int newB = mask ^ get(bDeref);
    int newA = aDeref + 1;
    int isOne = mask & newB;
    int modulo = newA % BIT_WIDTH;

    set(bDeref, newB);
    set(a, modulo);
    
    if (newA != modulo) {
        set(b, get(b) + 1);
    }
    
    if (isOne) {    
        return BRANCH_AND_HALT;
    }
    
    return CONTINUE_AND_HALT;
}

int w (int a, int b, int c) {

    int aDeref = get(a);
    int bDeref = get(b);
    int bit = aDeref % BIT_WIDTH;
    int mask = 0x01 << bit;
    int newB = mask ^ get(bDeref);
    int isOne = mask & newB;

    set (bDeref, newB);
    
    if (isOne == 0) {    
        return BRANCH_AND_HALT;
    }
    
    return CONTINUE_AND_HALT;
}

void step (int instruction) {
    for (;;) {

        int a = mem[ip];
        int b = mem[ip + 1];
        int c = mem[ip + 2];

        switch (instruction) {
        case HELLO: 
            debug ? displayMem(): 0;
            instruction = h (a, b, c);
            break;

        case WORLD:
            debug ? displayMem(): 0;
            instruction = w (a, b, c);
            break;

        case BRANCH_AND_HALT: 
            if (c < 0) {
                ip += 3;
                instruction = SUBLEQ;
                break;
            }
            instruction = HALT;
            ip = c;
            break;
        
        case CONTINUE_AND_HALT:
            ip += 3;
            instruction = HALT;
            break;

        case SUBLEQ:
            instruction = subleq (a, b, c);
            break;

        case HALT:
            return;
        }
    }
}


void hello (void) {
    step (HELLO);
    return;
}

void world (void) {
    step (WORLD);
    return;
}

int main () {

    //bootstrap program
    world();hello();world();world();hello();world();hello();world();hello();world();hello();world();hello();world();hello();world();hello();world();hello();world();hello();world();hello();world();hello();world();hello();world();hello();world();hello();world();hello();world();hello();world();hello();world();hello();world();hello();world();hello();world();hello();world();hello();world();hello();world();hello();world();hello();world();world();
    hello();world();world();world();hello();world();hello();world();world();
    world();hello();world();hello();world();hello();world();hello();world();
    world();world();world();world();world();hello();world();hello();world();hello();world();world();world();world();hello();world();hello();world();hello();world();world();world();world();hello();world();hello();world();hello();world();world();world();world();hello();world();world();world();world();hello();world();hello();world();hello();world();world();world();world();hello();world();hello();world();world();world();world();world();hello();world();hello();world();hello();world();world();world();world();hello();world();hello();world();hello();world();world();world();world();hello();world();hello();world();hello();world();hello();hello();
    world();world();hello();world();hello();world();hello();world();world();world();
   
        // 128: 0
    world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();
    // 129: -1
    hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();
    // 130: 139
    hello();hello();world();world();world();hello();world();world();hello();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();hello();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();
    // 131: -1
    hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();
    // 132: -1
    hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();
    // 133: 129
    hello();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();hello();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();
    // 134: 130
    world();world();world();hello();world();world();hello();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();hello();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();
    // 135: -1
    hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();
    // 136: 0
    world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();
    // 137: 0
    world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();
    // 138: 130
    world();world();world();hello();world();world();hello();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();hello();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();
    // 139: -72
    world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();hello();hello();hello();world();world();world();hello();world();world();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();
    // 140: -101
    hello();hello();world();world();world();hello();world();world();hello();hello();world();world();world();hello();world();world();world();world();world();hello();world();world();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();
    // 141: -108
    world();world();world();hello();world();world();world();world();world();hello();world();world();hello();world();world();world();hello();world();world();hello();world();world();world();hello();world();world();world();world();world();hello();world();world();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();
    // 142: -108
    world();world();world();hello();world();world();world();world();world();hello();world();world();hello();world();world();world();hello();world();world();hello();world();world();world();hello();world();world();world();world();world();hello();world();world();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();
    // 143: -111
    hello();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();hello();world();world();world();hello();world();world();world();world();world();hello();world();world();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();
    // 144: -32
    world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();
    // 145: -119
    hello();world();world();world();hello();world();world();world();world();world();hello();world();world();hello();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();
    // 146: -111
    hello();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();hello();world();world();world();hello();world();world();world();world();world();hello();world();world();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();
    // 147: -114
    world();world();world();hello();world();world();hello();hello();hello();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();
    // 148: -108
    world();world();world();hello();world();world();world();world();world();hello();world();world();hello();world();world();world();hello();world();world();hello();world();world();world();hello();world();world();world();world();world();hello();world();world();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();
    // 149: -100
    world();world();world();hello();world();world();world();world();world();hello();world();world();hello();hello();hello();world();world();world();hello();world();world();world();world();world();hello();world();world();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();
    // 150: -33
    hello();hello();hello();hello();hello();world();world();world();hello();world();world();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();
    // 151: -10
    world();world();world();hello();world();world();hello();hello();world();world();world();hello();world();world();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();hello();
    // 152: 0
    world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();world();world();world();hello();world();world();


    // initialise
    hello ();
    hello ();
    hello ();
    hello ();
    world ();
    world ();
    hello ();
    hello ();
    world ();
    hello ();
    world ();
    world ();
    hello ();
    world ();
    hello ();
    world ();
    world ();
    hello ();
    world ();
    hello ();
    world ();
    hello ();
    world ();
    hello ();
    world ();
    hello ();
    world ();
    hello ();
    world ();
    hello ();
    world ();
    hello ();
    world ();
    hello ();
    world ();
    hello ();
    world ();
    hello ();
    world ();
    hello ();
    world ();
    hello ();
    world ();
    hello ();
    world ();
    hello ();
    world ();
    hello ();
    world ();
    world ();
}
