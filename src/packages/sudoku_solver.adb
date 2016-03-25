with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Task_Identification;  use Ada.Task_Identification;

package body Sudoku_Solver is

	--Function checks to see if the passed name ends in .txt
	function isValidSudokuFilename( name : String ) return Boolean
	is 
	begin
		for i in Integer range 1..50 loop
			if (name(i) = '.') then --find the . in the filename
				--Make sure what follows the '.' is 'txt'
				if name(i+1) = 't' and name(i+2) = 'x' and name(i+3) = 't' then
					return true;
				end if;
			end if;
		end loop;
		return false;
	end isValidSudokuFilename;

	-- Read a sudoku in from text file.
	procedure readSudokuFile( puzzle : out Sudoku ; filename : String ) 
	is
		fp : File_Type;
		line : String(1..9);
	begin
		Open(fp, In_File, filename);

		while (not End_Of_File(fp)) loop
			for line_num in Integer range 1..9 loop
				--get each line from file
				line := Get_Line(fp);
				for i in Integer range 1..9 loop
					--convert the numeric char into an integer
					puzzle(line_num, i) := Character'Pos(line(i)) - 48;
					if puzzle(line_num, i) > 9 or puzzle(line_num, i) < 0 then
						New_Line; Put("Error. Invalid character in sudoku file. Exiting");
						Abort_Task(Current_Task);
					end if;
				end loop;
			end loop;
		end loop;
	exception
		--If input file not found.
		when name_error =>
			New_Line; Put("Error. Could not find input file. Exiting");
			Abort_Task(Current_Task);
	end readSudokuFile;

	--Attempt to solve a sudoku.Returns true if sudoku solved. False otherwise.
	procedure solveSudoku( pkg : out SudokuPkg )
	is
		row, col : Integer := 0;
	begin

		--Look for cell that has yet to be solved in sudoku if there is not 
		-- one, the sudoku is solved.
		--Otherwise, the row and column value of the next cell to be worked 
		-- on are stored in row and col.
		if sudokuIsSolved(pkg.puzzle) then
			pkg.result := true;
			return;
		else
			getNextUnsolvedCell(pkg.puzzle, row,col);
		end if;


		--Attempt to assign every possible value (1-9) to each unassingned square.
		for i in Integer range 1..9 loop
			--Check if the assingment of value i to square(row,col) is allowed by the rules
			-- of sudoku.
			if isLegal(pkg.puzzle, row, col, i) then
				--If it is, the we assign i to square(row,col).  This is tenative as it may
				-- end up this is not the true value for the cell as conflicts are revealed 
				-- as the solution progresses.
				pkg.puzzle(row, col) := i;

				--Recurse back into this solveSudoku function to attempt to solve the 
				-- sudoku with square(row,col) assigned to i.
				solveSudoku(pkg);

				if pkg.result = true then
					return;
				else
					pkg.puzzle(row,col) := 0;
				end if;

				--Recurse back into this solveSudoku function to attempt to solve the 
				-- sudoku with square(row,col) assigned to i.
			end if;
		end loop;
		-- This means there is no solution to the sudoku
		pkg.result := false;
		return;
	end solveSudoku;

	procedure getNextUnsolvedCell(puzzle : Sudoku ; row : out Integer ; col : out Integer)
	is
	begin
		for i in Integer range 1..9 loop
			for j in Integer range 1..9 loop
				if puzzle(i,j) = 0 then 
					--found unsolved cell, so set the row and col so it can be worked on.
					row := i;
					col := j;
					return;
				end if;
			end loop;
		end loop;
	end getNextUnsolvedCell;

	--Tests if the sudoku is solved by checking to see if every cell contains 
	-- a value (eg. cell /= 0).
	function sudokuIsSolved(puzzle: Sudoku) return Boolean
	is
	begin
		for i in Integer range 1..9 loop
			for j in Integer range 1..9 loop
				if puzzle(i,j) = 0 then 
					return false;
				end if;
			end loop;
		end loop;
		return true;
	end sudokuIsSolved;

	--Checks whether the insertion of i at cell(row,col) is permitted by sudoku rules.
	function isLegal(puzzle:Sudoku; row : Integer; col:Integer; value:Integer) return Boolean
	is 
		anchor_x, anchor_y : Integer := 0;
	begin
		-- Eliminate possibilities if they are present in the Row
		for i in Integer range 1..9 loop
			if puzzle(row,i) = value then
				--If passed value present in specified row, return false.
				return false;
			end if;
		end loop;

		-- Eliminate possibilities if they are present in the Column
		for i in Integer range 1..9 loop
			if puzzle(i, col) = value then
				--If passed value present in specified column, return false.
				return false;
			end if;
		end loop;

		-- Anchor position for searching 'local square'
		case getLocalSquareNumber(row,col) is
			when 1 => anchor_x := 1 ; anchor_y := 1;
			when 2 => anchor_x := 1 ; anchor_y := 4;
			when 3 => anchor_x := 1 ; anchor_y := 7;
			when 4 => anchor_x := 4 ; anchor_y := 1;
			when 5 => anchor_x := 4 ; anchor_y := 4;
			when 6 => anchor_x := 4 ; anchor_y := 7;
			when 7 => anchor_x := 7 ; anchor_y := 1;
			when 8 => anchor_x := 7 ; anchor_y := 4;
			when 9 => anchor_x := 7 ; anchor_y := 7;
			when others => 
				New_Line; 
				Put_Line("Error - Invalid Local Square value computed. Exiting");
				Abort_Task (Current_Task);
		end case;

		-- Eliminating possibilities from the local square
		if anchor_x > 0 and anchor_y > 0 then
			for i in Integer range 0..2 loop
				for j in Integer range 0..2 loop
					if puzzle((anchor_x+i), (anchor_y+j)) = value then
						--If passed value present in local square return false;
						return false;
					end if;
				end loop;
			end loop;
		end if;

		return true;
	end isLegal;


	--Print the solved sudoku to STDOUT
	procedure printSudoku( puzzle : Sudoku ) 
	is 
	begin
		New_Line; New_Line; New_Line;
		Put("+---------------------------------+---------------------------------+---------------------------------+"); 
		for i in Integer range 1..9 loop
			New_Line;Put("|");
			for j in Integer range 1..9 loop
				Put(puzzle(i,j));
				if (j mod 3 = 0) then
					Put("|"); 
				end if;
			end loop;
			New_Line;
			if(i mod 3 = 0) then
				Put_Line("+---------------------------------+---------------------------------+---------------------------------+");
			end if;
		end loop;
	end printSudoku;

	--Check to see if a the sudoku is solved.
	function isSolved( puzzle : Sudoku ) return Boolean
	is
	begin
		for i in Integer range 1..9 loop
			for j in Integer range 1..9 loop
				if puzzle(i,j) = 0 then 
					-- If a cell has the value of 0 then the puzzle is not finished.
					return false; 
				end if;
			end loop;
		end loop;
		return true;
	end isSolved;


	--Get the local square number of a coresponding cell in the sudoku
	--Numbers start at one for the top left square, and end at 9 for the 
	-- bottom right square (numbered moving across then down).
	function getLocalSquareNumber( x:Integer ; y:Integer) return Integer
	is 
	begin
		if x > 0 and y> 0 and x < 10 and y< 10 then
			if x <= 3 then
				if y<= 3 then return 1;
				elsif y<= 6 then return 2;
				elsif y<= 9 then return 3;
				end if;
			elsif x <= 6 then
				if y<= 3 then return 4;
				elsif y<= 6 then return 5;
				elsif y<= 9 then return 6;
				end if;
			elsif x <= 9 then
				if y<= 3 then return 7;
				elsif y<= 6 then return 8;
				elsif y<= 9 then return 9;
				end if;
			end if;
		end if;
		return 0;
	end getLocalSquareNumber;
end Sudoku_Solver;