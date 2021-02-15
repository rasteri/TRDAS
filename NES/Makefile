build:
	ca65 reset.s
	ld65 -C controltest.cfg -o controltest.nes reset.o

clean:
	rm -f ./$(NAME).s
	rm -f ./*.o
	rm -f ./$(NAME).nes
