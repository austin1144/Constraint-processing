========================================================Sudoku=====================
There are 2 files for Sudoku
1.ECLiPSe_Sudoku.pl
2.CHR_Sudoku.pl

***Important: sudex_toledo.pl has to be in the same folder!!!

%=ECLiPSe========================ECLiPSe_Sudoku.pl
Predicate: sudoku\3(Search Heuristic,Puzzle Name,Perspective)

The first argument can take “1” or “2”: 1 means input_order, 2 means first_fail.
The second argument can take puzzle name:e.g. "lambda", "extra2", "sudowiki_nb49".
The third argument Perspective can take input 1-4:
1 is classic perspective P,			
2 is new perspective Q,			
3 is after channeling, search in P’s variable.			
4 is channeling search in Q’s variable.

%=CHR============================CHR_Sudoku.pl
Predicate:solve(Name,Perspective)

Same as above but without Search Heuristic.

========================================================Hitori=====================
There are 4 files for Hitori
1.ECLiPSe_Hitori_basic.pl
2.ECLiPSe_Hitori_additional.pl
3.CHR_Hitori_basic.pl
4.CHR_Hitori_addtitional.pl

***Important: The puzzles.pl has to be in the same folder!!!

%=ECLiPSe========================ECLiPSe_Hitori_basic.pl/ECLiPSe_Hitori_additional.pl
Predicate hitori\3: hitori(Num of Puzzle, Size of Puzzle, Search Heuristic).

The third argument can take “1” or “2”: 1 means input_order, 2 means first_fail.
How to use it:
1. hitori(Num,_,1).
2. hitori(Num,Size,1).
3. hitori(_,Size,_). 
...

even can take hitori(_,_,_).
The puzzle shows by array, each number 0 represents a blacked out cell.

%=CHR============================CHR_Hitori_basic.pl/CHR_Hitori_addtitional.pl
Predicate hitori\2: hitori(Num of Puzzle, Size of Puzzle).

3 ways to use it, by input num of puzzle or size of puzzle or both:
1. hitori(Num,_).
2. hitori(_,Size).
3. hitori(Num,Size).

The Puzzle shows by list, each number 0 represents a blacked out cell.