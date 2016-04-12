/* Adrian Tousignant
   40017029
*/

/* Take the file with the numberlink problem as input */
main(File):-
	open(File,read,Str), 
	read(Str,List),
	close(Str),
	term_string(List, L),
	split_string(L,['(',')',':',','],"",L1),
	format_input(L1,Lst,Dimension,Terminals),
	initMatrix(Dimension,Dimension,Matrix),
	putTerminals(Lst,Matrix),
	findSolution(Lst,Matrix,Solution),
	output(Solution,Dimension,Terminals,Output),
	write(Output).

	
/* format_input and store_terminals are helper methods taking the input and transforming it
	in list form for the other methods to use */
format_input([Dimension,Terminals|LIn],Lst,IntDimension,IntTerminals):-
	atom_number(Dimension,IntDimension),
	atom_number(Terminals,IntTerminals),
	store_terminals(LIn,Lst).
	
store_terminals([],[]).
store_terminals([_,Number,_,Ri,Ci,_,Rf,Cf,_|L],[[IntNumber,IntRi,IntCi,IntRf,IntCf]|Lst]):-
	atom_number(Number,IntNumber),
	atom_number(Ri,IntRi),
	atom_number(Ci,IntCi),
	atom_number(Rf,IntRf),
	atom_number(Cf,IntCf),
	store_terminals(L,Lst).
store_terminals([Number,_,Ri,Ci,_,Rf,Cf|L], [[IntNumber,IntRi,IntCi,IntRf,IntCf]|Lst]):-
	atom_number(Number,IntNumber),
	atom_number(Ri,IntRi),
	atom_number(Ci,IntCi),
	atom_number(Rf,IntRf),
	atom_number(Cf,IntCf),
	store_terminals(L,Lst). 	

/* Initialize an empty matrix of size Dimension*Dimension */
initMatrix(Rows,Cols,Matrix):-
	initMatrix(Rows,Cols,Matrix,[]).
initMatrix(0, _,Matrix,Matrix ):- !.
initMatrix(Rows,Cols,Matrix,Acc):-
	Next is Rows - 1,
	length(H,Cols),
	initMatrix(Next,Cols,Matrix,[H|Acc]).

/* Initialize the terminals of the problem */
putTerminals([],_Matrix).
putTerminals([H|T],Matrix) :-
	H = [Number,R1,C1,R2,C2],	
	row(Matrix,R1,Row1),
	nth1(C1,Row1,Number),
	row(Matrix,R2,Row2),
	nth1(C2,Row2,Number),
	putTerminals(T,Matrix).

/* Assign numbers to the cells in Matrix */
putNumber(Matrix,[H|T],Number):-
	H = (R,C),
	row(Matrix,R,Row),
	nth1(C,Row,Number),
	putNumber( Matrix, T, Number ). 
putNumber(Matrix,[],_).

/* Recursively connect every pair of terminals */
findSolution([],Matrix,[]) :- !.
findSolution([H|T],Matrix,[Path|Solution]) :- 
	H = [Number,R1,C1,R2,C2],
	connected(R1,C1,R2,C2,Path,Number,Matrix), 
	putNumber(Matrix,Path,Number),
	findSolution(T,Matrix,Solution).

/* This is the constraint connecting terminals (R1,C1) and (R2,C2) */
connected(R1,C1,R2,C2,[(R1,C1)|Path],Number,Matrix):-
	connected(R1,C1,R2,C2,Path,[(R2,C2)],Number,Matrix).

connected(R1,C1,R2,C2,Path,Path,Number,Matrix) :-
	[R1,C1] \== [R2,C2], 
	neighbor(R1,C1,R2,C2,Number,Matrix).

connected(R1,C1,R2,C2,Path,Visited,Number,Matrix) :-
	[R1,C1] \== [R2,C2],	
	neighbor(NextR,NextC,R2,C2,Number,Matrix),
	\+ member((NextR,NextC),Visited), 
	connected(R1,C1,NextR,NextC,Path,[(NextR,NextC)|Visited],Number,Matrix).  

/* Return the Nth Row as Lst */ 
row([_|T],N,Lst):-
	N > 1,
	Next is N - 1,
	row(T,Next,Lst).
row([H|_],1,H).	

/* All the possible neighbors of the cell (R1,C1) */	
neighbor(R1,C1,R2,C2,Number,Matrix):- 
	row(Matrix,R2,Row),
	nth1(C2,Row,Number),
	R1 is R2, 
	C1 is C2 - 1.
neighbor(R1,C1,R2,C2,Number,Matrix):- 
	row( Matrix,R2,Row),
	nth1(C2,Row,Number),
	R1 is R2,
	C1 is C2 + 1.
neighbor(R1,C1,R2,C2,Number,Matrix):- 
	row(Matrix,R2,Row),
	nth1(C2,Row,Number),
	R1 is R2 + 1,
	C1 is C2.
neighbor(R1,C1,R2,C2,Number,Matrix):- 
	row(Matrix,R2,Row),
	nth1(C2,Row,Number),	
	R1 is R2 - 1,
	C1 is C2.
	
/* Read the Solution list and return the desired output */	
output(Solution,Dimension,Terminals,[Dimension,Terminals|Output]):-
	output(Solution,Output,1).
	
output([H|T],[[Acc : H]|Output], Acc):-	
	Next is Acc + 1,
	output(T,Output,Next).
	
output([],[],_).
