#include <stdio.h>

int program[] = {
    0, -1,       //128
    139, -1, -1,     //130
    129, 130, -1,   //133
    0, 0, 130,      //136
    -'H', -'e', -'l', -'l', -'o', -' ', -'w', -'o', -'r', -'l', -'d', -'!', -'\n', -'\0' //139
};

int column = 0;

void putInst (char * instruction) {
    if (column == 0) {
        printf ("\n    ");
    }
    printf ("%s ", instruction);
    column = column >= 11 ? 0 : column + 1;
}

int main (void) {
    
    for (int byte = 0; byte < 25; byte++) {
        printf ("\n    ");
        column = 0;
        for (int bit = 0; bit < 32; bit ++) {
            int mask = 0x01 << bit;
            
            if (0 != (mask & program[byte])) {
                putInst ("hello"); 
            }
            else {
                putInst ("world");
                putInst ("world");
                putInst ("world");
                putInst ("hello");
                putInst ("world");
                putInst ("world");
            }
        }
    }
    printf ("\n");
}
