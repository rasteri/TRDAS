/*

Program to generate an input file for use on the NES TRDAS roms
Compile with : 
	gcc nes.c -o nes
Then run, piping output to file :
	./nes > testinput.r16m
Then run test : 
	python tastm32.py --console snes --players 1,2,5,6 testinput.r16m
    (yes it really needs to be run in snes mode)

*/
// algo from https://codebase64.org/doku.php?id=base:small_fast_8-bit_prng
/*
        lda seed
        beq doEor
         asl
         beq noEor ;if the input was $80, skip the EOR
         bcc noEor
doEor:    eor #$1d
noEor:  sta seed
*/
#include <stdio.h>
#include <stdint.h>

int main(){
    uint16_t seed = 0;
    uint8_t constant = 0x1D;

    for (unsigned int j=0; j<3; j++){
    for (unsigned int i=0; i<255; i++){

        if (seed == 0) goto doEor;
        seed <<= 1;
        if (!(seed & 0xFF) || seed & 0x100) goto noEor;
        doEor:
            seed = (seed ^ constant);
        noEor:
            seed &= 0xff;
        //printf("%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c", i, 0, i, 0, 0, 0, 0, 0, i, 0, i, 0, 0, 0, 0, 0);
        printf("%2x,", seed);
    }
    printf("\n\n");
    }
}