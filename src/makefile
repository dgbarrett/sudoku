CC = gnatmake -Ipackages -gnatwA

all: main.adb packages/sudoku_solver.adb
	make clean
	$(CC) $^

clean: 
	cd packages; make clean
	rm -rf *.ali
	rm -rf *.o
	rm -rf *.exe
	rm -rf *.out
	