with Sudoku_Solver; use Sudoku_Solver;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure main 
is 
	s : Sudoku ;
	pkg : SudokuPkg;
	infile, outfile : String(1..50);
	last:Natural;
	output_fp : File_Type;
begin
	New_Line;
	--get infile name
	loop
		Put("Enter a sudoku input file name: ");
		Ada.Text_IO.Get_Line(infile, last);
		exit when isValidSudokuFilename(infile);
	end loop;

	--get outfile name
	loop
		Put("Enter an output file name: ");
		Ada.Text_IO.Get_Line(outfile, last);
		exit  when isValidSudokuFilename(outfile);
	end loop;

	readSudokuFile(s, infile) ;
	New_line;Put_Line("Before:");
	printSudoku(s);

	pkg.puzzle := s;
	pkg.result := false;

	solveSudoku(pkg);
	-- redirect STDOUT to outfile
	Ada.Text_IO.Create(output_fp, Ada.Text_IO.Out_File, outfile);
	Ada.Text_IO.Set_Output(output_fp);
	
	if pkg.result then 
		printSudoku(pkg.puzzle);
		Put("Solved Sudoku!");
	end if;

	-- set STDOUT back to STDOUT
	Ada.Text_IO.Close(output_fp);
	Ada.Text_IO.Set_Output(Ada.Text_IO.Standard_Output);	

	-- print results to STDOUT as well
	if pkg.result then 
		New_Line;Put("After:");
		printSudoku(pkg.puzzle);
		Put("Solved Sudoku!");
	else
		Put("No solution!");
	end if;

end main;