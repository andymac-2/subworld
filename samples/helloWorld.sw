start :
    # putchar until null terminator
    helloString -1 -1;
    
    # increment text pointer
    data.1 start;

    # goto start
    data.0 data.0 start;

helloString:
    "Hello World!\n\0";

data:
    [0x00, -0x01];
