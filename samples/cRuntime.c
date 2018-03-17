#include <stdio.h>
#define hello t(0);
#define world t(1);

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

    /* bootstrap program */

    world hello world world hello world hello world hello world hello world
    hello world hello world hello world hello world hello world hello world
    hello world hello world hello world hello world hello world hello world
    hello world hello world hello world hello world hello world hello world
    hello world hello world hello world world hello world world world hello
    world hello world world world hello world hello world hello world hello
    world world world world world world hello world hello world hello world
    world world world hello world hello world hello world world world world
    hello world hello world hello world world world world hello world world
    world world hello world hello world hello world world world world hello
    world hello world world world world world hello world hello world hello
    world world world world hello world hello world hello world world world
    world hello world hello world hello world hello hello world world hello
    world hello world hello world world world

    /* insert your own code here */
    
    hello hello hello hello world world hello hello world hello world world
    hello world hello world world hello world hello world hello world hello
    world hello world hello world hello world hello world hello world hello
    world hello world hello world hello world hello world hello world hello
    world world

}
