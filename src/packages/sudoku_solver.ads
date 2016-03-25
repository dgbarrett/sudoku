package Sudoku_Solver is
	
	type Sudoku is array (1 .. 9, 1 .. 9) of Integer;

	type SudokuPkg is record
		result : Boolean := false;
		puzzle : Sudoku;
	end record;

	function isValidSudokuFilename( name : String ) return Boolean;
	procedure readSudokuFile( puzzle : out Sudoku ; filename : String );
	procedure getNextUnsolvedCell(puzzle : Sudoku ; row : out Integer ; col : out Integer);
	procedure solveSudoku( pkg : out SudokuPkg );
	function getLocalSquareNumber( x:Integer ; y:Integer) return Integer; 
	function sudokuIsSolved(puzzle: Sudoku) return Boolean;
	function isLegal(puzzle:Sudoku; row : Integer; col:Integer; value:Integer) return Boolean;
	procedure printSudoku( puzzle : Sudoku );

end Sudoku_Solver;