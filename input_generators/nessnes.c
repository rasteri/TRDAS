/*

Program to generate an input file for use on the NES and SNES TRDAS roms.
Compile with : 
	gcc nessnes.c -o nessnes
Then run, piping output to file :
	./nessnes > testinput.r16m
Then run test : 
	python tastm32.py --console snes --players 1,2,5,6 testinput.r16m

*/

#include <stdio.h>

int main(){
    for (unsigned int j=0; j<1000; j++)
    for (unsigned int i=0; i<255; i++){
        printf("%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c", i, 0, i, 0, 0, 0, 0, 0, i, 0, i, 0, 0, 0, 0, 0);
    }
}