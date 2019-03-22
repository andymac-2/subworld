# Subworld

(C) 2018 Andrew Pritchard (MIT License)

An esoteric programming language using only two symbols. "hello" and "world". Skip to "Rationale" if you just are curious on what the hell this is all about.

## Usage

### Installation from source

You will need several things to run this language in full: A POSIX compliant `C` compiler to run the "hello world" source (eg: Clang or gcc), and the Haskell tool stack if you wish to compile the compiler. A very spartan compiler is provided in the `samples` directory written in C, which will only produce the `hello` s and `world` s from the second to the second-last paragraph of the output. You will have to copy the first and last paragraphs yourself. Some insight to this process is discussed below.

### QuickStart

Compilation is done simply with stack:

```sh
$ cd installation/directory
$ stack build
```

The subworld compiler: `sc`, takes input in the subworld language from `STDIN`, and produces valid C code to `STDOUT`. The C code will only have the symbols `hello` and `world` in the body of `main`.

```sh
$ stack exec sc < ./samples/helloWorld.sw > helloWorld.c
```

You can then compile the `c` file with your favourite compiler:

```sh
$ c99 -o helloWorld.out helloWorld.c && ./helloWorld.out
```

If you wish to run `sc` globally without `stack exec` you can run:

```sh
$ stack install
$ sc < infile > outfile
```

Since the language itself is quite limited, more so than assembly, the `basicIO.swc` file has been preprocessed using `m4`, and includes some helpful macros to do things such as moving around variables, printing and receiving strings, and arithmetic. On a unix like system, `m4` should be installed by default. An additional step will be required:

```sh
m4 -E < ./samples/basicIO.swc | stack exec sc > helloWorld.c
c99 -o helloWorld.out helloWorld.c && ./helloWorld.out
```

The `-E` flag stops the execution of `m4` when an error occurs. You may wish to separate the execution of `m4` with the execution of `sc`.

For more information, skip to "The Subworld language".

### Rationale

I have always been fascinated by the offerings of the IOCCC, and some of the more amusing esoteric languages. One thing that fascinated me was the concept of an OISC, or a One Instruction Set Computer. It is indeed possible to create an architecture that only has a single instruction and still be turing complete! Judicious choice of instruction means that any computation can be performed, and an example of such an instruction would be `subleq`. `subleq` is short for "SUBtract and Branch if Less than or EQual to zero".

I still felt I could do better. The program for a `subleq` machine must be loaded into storage first in order for it to run. A language such as `brainfuck` is different. All data is stored in the language itself, and in `brainfuck` all memory is initialised to zero. Any strings, or memory initialisation must be done while the machine is running. At the same time I was thinking about this, I had also seen a terribly obfuscated version of a "Hello world" program written in `C++` which, according to the author, had to be compiled by their baffled professor in order to verify it did indeed print "Hello World!".

### Introduction to the language and runtime

It had occurred to me that `brainfuck` uses six symbols without memory initialisation, and an OISC needs one instruction at a minimum, but required memory initialisation first. I figured that it would require a minimum of two carefully chosen symbols to produce a language that would not need memory initialisation first. Repeatedly writing the same symbol would be trivially impossible to initialise the memory correctly, so what better way to describe a "Hello World" program than to write it only using two symbols: `hello` and `world`. Anyway, let's have a look at some of the source code. The following is an excerpt from `samples/hwOriginalObfuscated.c`. Try compiling this code to confirm it does indeed print "Hello World!".

```C
#include <stdio.h>
#define hello t(0);
#define world t(1);
```

Well this looks simple enough. I wanted this clearly displayed here to demonstrate that there is no fancy macro trickery happening, and that `hello` and `world` are simply shorthand for a function invocation with set arguments. I am certainly not trying to deceive you on this front

```C
int p=0;int m[8192]={0};int g(int a){return a>=0?m[a]:getchar();}void s(int
a,int v){if(a>=0){m[a]=v;}else{putchar(v);}}void t(int i){for(;;){int a=m[p
],b=m[p+1],c=m[p+2];switch(i){case 0:{int aD=g(a),bD=g(b),bi=aD%32,ma=1<<bi
,nB=ma^g(bD),nA=aD+1,O=ma&nB,mo=nA%32;s(bD,nB);s(a,mo);if(nA!=mo){s(b,g(b)+
1);}if(O){i=2;} else{i=3;}};break;case 1:{int aD=g(a),bD=g(b),bi=aD%32,ma=1
<<bi,nB=ma^g(bD),O=ma&nB;s(bD,nB);if(O==0){i=2;}else{i=3;}}break;case 2:if(
c<0){p+=3;i=4;break;}i=5;p=c;break;case 3:p+=3;i=5;break;case 4:{int aD=g(a
),bD=b>=0?m[b]:0,nB=bD-aD;s(b,nB);if(nB<=0){if(c<0){p+=3;i=5;break;}else{p=
c;i=4;break;}}else{p+=3;i=4;}}break;case 5:return;}}}

int main () {
```

Now you might be thinking you've figured me out. Surely the string "Hello World!" must be stored in that maze of code above somehow. I guarantee you that it is not, and again, I am not trying to deceive you! The above code is simply a machine runtime. It contains the code necessary to actually run the program made entirely of `hello` and `world`. You can see on the first line the variable `int p = 0;`. `p` is the instruction pointer. which will point somewhere in the array `int m[8192] = {0}`. As you can see, the array `m` for memory is initialised to all zeroes.

The file `samples/hwOriginal.c` shows the above code de-obfuscted. Feel free to review it to ensure I have not stored the "Hello World!" string there. So the real code isn't in the macros, it isn't in the poorly obfuscated header, so where is it? There's only one place left:

### The body

```
...
world hello world world hello world hello world hello world hello world
hello world hello world hello world hello world hello world hello world
hello world hello world hello world hello world hello world hello world
hello world hello world hello world hello world hello world hello world
hello world hello world hello world world hello world world world hello
world hello world world world hello world hello world hello world hello
...
```

So somehow we are expected to decipher the above 'code' for lack of a better word to find the truth behind all of this. You'll notice that the source code is separated into paragraphs. As a joke, I could say that this is for readability. The most interesting of these paragraphs are the first and last paragraphs.

It's about time to tell you what "hello" and "world" do. These are instructions which I picked very carefully in order to be able to initialise memory and run a meaningful program with it. Each instruction is fixed at three cells wide, and using 32 bit integers for each memory cell, each instruction is 12 bytes wide. The instructions are laid out as follows:

```
A B C
```

The entire instruction including `A`, `B`, and `C` is read into memory. Before anything else is done, we read from our memory array `m[B]` and call it 'bDeref' if `B` is negative, we can't read from before the array so, we receive a default value of `0`. We dereference `B` first because there is the possibility that an instruction will modify it's own data. We have two instructions which are described as follows:

- `world`: take the value at `m[A]` toggle bit `m[A] % 32` of `m[bDeref]`. If the bit was switched to `1` go to the next instruction 3 cells along. If the bit was switched to `0`, then branch to the instruction at `C`.

- `hello`: take the value at `m[A]` toggle bit `m[A] % 32` of `m[bDeref]`. now increment `m[A]`. If the bit was toggled to `0` go to the next instruction 3 cells along. If the bit was switched to `1`, then branch to the instruction at `C`.

"hello" and "world" are similar but slightly different. They brach at opposite times, and "hello" performs an increment which "world" does not. The following are some further details about the runtime.

- When trying to **read** data from a negative index, instead of letting the C runtime perform an out of bounds reference, we return the value from a call to `getchar()` instead. This is how we perform input.
- When trying to **write** to a negative index, instead of letting the C runtime perform an out of bounds reference, we send the data to a call of 'putchar()' instead. This is how we perform output.
- While we are running code, if the code tries to **branch** to a negative index, we instead go to `subleq` mode. This mode will repeatedly run the `subleq` instruction until the code performs another branch to a negative value.

The `subleq` instruction deserves some further attention. The istruction is describes as follows:

- Read `A`, `B` and `C` into memory the same as before. Subtract the value at m[A] from the value at `m[B]`. Store the result in m[B]. If the result was less than or equal to zero, goto C, otherwise proceed to the next instruction. Loop until the machine tries to branch to a negative index, then halt.

This is basically the entirety of the runtime. The first and last paragraph of "hello" and "world" was written by myself **by hand**, and believe me, it is not a trivial process.

### The Bootstrap

The first paragraph is hand coded garbage designed to get some control over the machine in order for us to be able to load a sensible program. The runtime initialises all memory to zero, and with only very coarse control over the state of the machine, we somehow have to be able to wrestle some compliance out of it. With only two symbols at my disposal, I was not coding in assembly, *I was coding in binary.* I had a taste of what early computing pioneers were creating when they started all of this.

The bootstrap has several phases which I'll describe briefly. I'm not expecting anybody to truly understand what happens:

#### Initial Control

The first few instructions are used just to avoid the machine getting into a loop or segfaulting. There is nothing particularly special about this. The instruction `0 0 0` which is all that the momory has to offer initially is "set bit `m[0]` of `m[m[0]]`" with an optional increment of 'm[0]' and poissibly branch to `0`.

#### Create a Useful instruction.

We can increment the value at `m[0]` by alternating `hello` and `world`. Alternating `hello` and `world` will set and then unset a bit, but then increment `m[0]` once, so the total effect is just an increment. After this, we can choose to branch back to `0` with the appropriate instruction. Since we have modified the instruction at `m[0]` we can use this to set bit zero of `m[m[0]]`. Since we have the control over incrementing `m[0]` we have the control over which address we write to.

This means that we can generate the instruction `0 1 0` which will allow us to write a single bit of our choosing to the address at `m[1]`. However, we currently have no control over what gets written to `m[1]`, so we have no control over the address. In order to gain that control we have to run the instruction `1 0 0` at address `0`. This allows us to write the zero bit of `m[1]`. Once we have written a single `1` to `m[1]`, we now have access to the instruction `n 1 0` which will allow us to write any bit of our choosing to `m[1]`.

We increment `m[0]` by alternating `hello` and `world`. This allows us to control which bit we write to `m[1]`. I choose to write bit `5` which gives the instruction `5 1 0` which makes `m[1]` equal to `33`. This means we can now write whatever bit we want to `m[33]` using the `0 1 0` instruction we created right at the start. I decide to write the value `128` to address `33` The bootstrap is now complete, and we can begin writing our program to address `128`

#### Loading the program

The bootstrap is now complete. I can write out our program a single bit at a time using `hello` and `world`. To write a `1` I can simply write `hello`. Writing a zero requires more effort. A zero can be written using `world world world hello world world`. A different bootstrap program would require different instructions to write data, but I'm not jumping to do it, it was hard enough the first time. The vast bulk of the source code is loading the data for the program.

#### Initialising the program

The program initialisation is the last paragraph of the C code.

In order to actually run the program I've loaded, I have to advance the instruction pointer to address `128`. Unfortunately for me, my own bootstrap program gets in the way and will cause a segfault if I try to advance through it. I use one of the values I manipulated earlier to give me a branching instruction which will allow me to advance past the bootstrap program. Unfortunately this means that the instruction pointer is not aligned with the start of the program proper. This is an easy fix, I simply write a few extra bytes and start the program at address `130` instead.

I simply advance the program to address `130` and branch to a negative value in order to enter `subleq` mode. From then on, the program is controlled by whatever the user wrote. Note that during the process of advancing the instruction pointer, I will have written some garbage values to memory outside of the bounds of the program, so you cannot rely on these values being initialised to zero.

## The Subworld Language

The subworld code as it is written in the C file is so notoriously difficult to write in it's own right, that it's easier to considered it a compilation target rather than a language. The first compiler that I wrote was quite bare in it's functionality. All values had to be literals. Resolving `goto` destinations and memory addresses had to be done manually. The original compiler is found at `samples/spartanCompiler.c` and shall remain unmodified. This compiler is hard coded to produce a "Hello World!" program as output. The output of this program should be pasted where the comment lies in `samples/cRuntime.c` saying `insert your own code here`

The second compiler that I wrote, I used a high level language: Haskell. Haskell has some excellent tools available to make parsers using EBNF style syntax. You just have to specify your production rules in this pseudo EBNF format, and what you want the result to be, and the libraries take care of the rest. It wasn't long before I had a functioning compiler with automatic label resolution. Here is the hello world program written in this new language:

```Make
start:
    # putchar until null terminator
    helloString -1 -1;

    # increment text pointer
    data.1 start;

    # goto start
    data.0 data.0 start;

helloString:
    "Hello World!\n\0";

data:
    [0x00, -1];
```

This works beautifully. If you were to strip away the comments, this "Hello world" program is probably shorter than one written in `C++`.

### Syntax and usage.

A Subworld program consists of a number of *labels* and *statements*. Whitespace is ignored except to separate certain tokens and inside of strings. A *label* consists of an *identifier* followed by a colon. Valid *identifiers* are a combination of alphanumeric characters and underscores. *Identifiers* must start with a letter.

```Make
helloString:                # a label
hello_String:               # labels can have underscores
String_Hello_5:             # labels can have numbers, but cannot begin with them
```

A *label* indicates a particular location in memory. You can branch to a *label* or access data at a *label*. *Labels* do not take up any memory in a program, they are only there for convenience. The compiler will give an error if you try to reference a *label* that does not exist.

```Make
# the three kinds of statements:

    data.0 data.0 start;            # an instruction
    "Hello World!\n\0";             # a string
    [0x00, -1];                     # data
    [-1];                           # data can be a single value
```

*Statements* however take up memory space. There are three types of *statements*: *instructions*, *strings*, and *data*. *Statements* end with a semicolon, and will give an error if this is not followed.

```Make
    # a comment
```

The only other construct in the language are *comments*. *Comments* begin with a `#`, and finish at the end of a line. Anything in a comment is ignored, and the `#` character is not valid anywhere else.

```Make
    # A value can be a reference to a label or a number
    helloString -1 -1;

    # A number can be decimal or hexadecimal
    [0x20, -1];

    # You can add a period and a number after a label reference to specify
    # an offset. "data.0" is the same as "data". "data.1" will be the
    # next cell along. "data.2" will be two cells further along etc.
    data.1 start;           
```

*Instructions* consist of three *Values* separated by whitespace. A *value*. A *value* is either a decimal number, a hexadecimal number preceded by `0x` or a label reference. A label reference may have an optional offset, which will specify how far past the label you wish to reference. offsets are specified by a period (`.`) followed by a decimal or hexadecimal number. For example `label.0` is the same as `label`. `label.1` represents the first address after `label.0`. `label.2` is after `label.1` and so on. In the hello world program above, we use `data.0` and `data.1` to reference `0` and `-1` respectively.

All instructions all have the same opcode, so they are ommitted. Instructions are of the format:

```Make
A B C           # m[B] -= m[A]; if m[B] <= 0 goto C;
A B             # m[B] -= m[A]; branching disabled.
-1 B            # m[B] -= getchar();
A -1            # putchar(-m[A]);
```

Each instruction does the same thing. Subtract the value from memory address `A`, from the value in memory address `B`. Store the result in memory address `B`. If the result is less than or equal to zero, branch to `C`. If it is positive, go to the next instruction. As a convenience, because not every instruction will need to branch, the third argument is optional. If you omit it, the compiler will put the branch address to the next instruction, so if you branch, you'll end up at at the same place as if you hadn't branched.

Input and output are performed by reading from, or writing to negative memory addresses. if `A` is `-1` then the value of `getchar()` will be subtracted from the value at the address at `B`. if `B` is `-1` then the negative of the value at address `A` will be used as an argument to `putchar()`. `getchar()` and `putchar()` are documented alongside other standard `C` library functions.

```Make
    "Hello World!\n\0";             # a string

    "Multiline Strings are disabled because\n";
    "Writing one string after another is\n";
    "Exactly the same thing\n\0"

    # these two strings are the same
    "a"; "b"; "c"; "d";
    "abcd";
```

*Strings* are enclosed by double quotes. You can use a backslash to escape a character in a string. Normal escape sequences apply. Strings are not null terminated by default, so you may have to put a `\0` yourself after a string if you want it to stop printing. Strings take up adjacent areas in memory, so you can write multiple strings right next to each other and they will be joined in memory.

One quirk of strings is that they are stored as *negative values*. That means for example the character `H` is stored a `-72` rather than `72`. This makes it easier for IO, and makes the above "Hello World" program work as expected.

```Make
    # All data is composed of integers or labels
    [1, 3, -4, -5, 0x0006, -0x01, label, anotherLabel.2];

    # A number can be decimal or hexadecimal
    [0x10, -0x04, 16, -20];

    # data can consist of a single value
    [hello];
```

*Data* is composed of a series of sequential values in memory. *data* can either be a decimal number, a hexadecimal number, or a *label reference*. *data* can be written as negative values by preceding the number with a `-`. Hexadecimal numbers are written by preceding the number with an `0x`.

### Preprocessing
```bash
# output can be piped from m4 straight to sc
m4 -E < ./samples/basicIO.swc | stack exec sc > helloWorld.c

# you may wish to run the two separately.
m4 -E < ./samples/basicIO.swc > ./samples/basicIO.sw
stack exec sc < ./samples/basicIO.sw > helloWorld.c
```

Since the language only has a single instruction, it is quite limited in it's functionality. It's recommended to use a text preprocessor prior to sending the output to a compiler. A suitable macro preprocessor exists on POSIX compliant systems for textual replacement: `m4`. For maximal portability, this preprocessor has been used. The usage of `m4` is covered elsewhere. The `basicIO.swc` must be preprocessed before execution. This allows us to use higher level functions when writing programs using `subleq` only.

## Conclusion

Please submit any feature requests, regular requests or bugs to Github. I hope you enjoyed this silly little demo!
