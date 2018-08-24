:- use_module(library(ic)).
:- import element/3 from gfd.
:- import alldifferent/1 from ic_global.
:- import concat/2 from matrix_util.%for func concat/2
:-[sudex_toledo].

%====== Experiment start ========%
test(Perspective):-
sudoku(1,lambda,Perspective),
sudoku(2,lambda,Perspective).

maintest(Perspective):-
sudoku(1,lambda,Perspective),
sudoku(2,lambda,Perspective),
sudoku(1,extra2,Perspective),
sudoku(2,extra2,Perspective),
sudoku(1,sudowiki_nb49,Perspective),
sudoku(2,sudowiki_nb49,Perspective).

extratest(Perspective):-
sudoku(1,extra4,Perspective),
sudoku(2,extra4,Perspective),
sudoku(1,hard17,Perspective),
sudoku(2,hard17,Perspective),
sudoku(1,eastermonster,Perspective),
sudoku(2,eastermonster,Perspective).


%====== run by channelling perspective ========%	
sudoku(Method,Name,3):-
take_method(Method,Heur),
cputime(T0),
puzzles(P,Name),
convert_list_array(P,Parray),
solve(Parray),
transfer(Parray,Q,N),
solve2(Q,N),
matchnew(Q,Parray,N),%This is the channeling
search([Parray],0,Heur,indomain,complete, [backtrack(Back)]),
T is cputime - T0,
        write('Under heuristic:\s'),write(Heur),write('\s:\s'),write(T),
        writeln('seconds.'),
        write('Backtrack:\s'),writeln(Back).
%====== run by 2nd perspective ========%		
sudoku(Method,Name,2):-
take_method(Method,Heur),
cputime(T0),
puzzles(P,Name),
convert_list_array(P,Parray),
solve(Parray),
transfer(Parray,Q,N),
solve2(Q,N),
search(Q,0,Heur,indomain,complete, [backtrack(Back)]),
T is cputime - T0,
        write('Under heuristic:\s'),write(Heur),write('\s:\s'),write(T),
        writeln('seconds.'),
        write('Backtrack:\s'),writeln(Back).
%====== run by 1st perspective ========%		
sudoku(Method,Name,1):-
take_method(Method,Heur),
cputime(T0),
puzzles(P,Name),
convert_list_array(P,Parray),
solve(Parray),
search(Parray,0,Heur,indomain,complete, [backtrack(Back)]),
T is cputime - T0,
        write('Under heuristic:\s'),write(Heur),write('\s:\s'),write(T),
        writeln('seconds.'),
        write('Backtrack:\s'),writeln(Back).		
		
%====== Given Heuristic ========%	
take_method(Method,Heur):-
Method = 1,Heur = input_order;
Method = 2,Heur = first_fail.

%====== solve in 1st perspective ========%	
% solve in 3 different constraints.
% row, column, block are all different.
solve(P):-
dim(P,[N,N]),
P :: 1..N,
(for(I,1,N),param(P)
do
print("0"),
alldifferent(P[I,*]),
alldifferent(P[*,I])
),
(multifor([M,N],0,2),param(P)
do
alldifferent(concat(P[1+3*M..3+3*M,1+3*N..3+3*N]))
).

%====== convert list to array ========%	
convert_list_array(Plist,Parray):-
convert_listwitharray(Plist,P1dim),
eval_to_array(P1dim,Parray).

convert_listwitharray([],[]).
convert_listwitharray([Head|Rest],[Headlist|RestList]):-
Headlist =..[[]|Head],
convert_listwitharray(Rest,RestList).

%====== From P([Row,Col])=Number transfer to Q([Number,Block])= Index ========%	
%==Domain of index{1..90}
% 1st index are all different in domain{1..90}.
transfer(P,Q,N):-
dim(P,[N,N]),
dim(Q,[N,N]),
NN is N*N,
Q :: 1..NN,
alldifferent(concat(Q[1..N,1..N])),
(multifor([I,J],1,N),param(P,Q)
do
V is P[I,J],
((nonvar(V),
A is (J-1)//3+3*((I-1)//3)+1,
Q[V,A]#=9*I+J-9)
;var(V))
).

%====== solve in 2nd perspective ========%	
% There are 5 constraints, the 1st one is in the above line.
% 2nd index in each block has specific domain.
solve2(Q,N):-
(multifor([K,L],1,3),param(Q)
do
AC is Q[*,K*3+L-3],
AC :: [
K*27+L*3-29,K*27+L*3-28,K*27+L*3-27,
K*27+L*3-20,K*27+L*3-19,K*27+L*3-18,
K*27+L*3-11,K*27+L*3-10,K*27+L*3-9]
),
dim(Clist,[N,N]),
dim(Rlist,[N,N]),
Clist :: 1..N,
Rlist :: 1..N,
% 3rd&4th row&col for the same number is differnt.
(multifor([I,J],1,N),param(Q,N,Clist,Rlist)
do
Q[I,J] #= Clist[I,J]*N+Rlist[I,J]-N
)%end multifor
,
(for(X,1,N),param(Clist,Rlist)
do
alldifferent(Clist[X,*]),
alldifferent(Rlist[X,*])
)%end for
.
% tth constraints implement is this perspective automatically, the same number should be in differnt block.

%====== channelling ========%	
matchnew(Q,P,N):-
array_flat(1,P,Pflat),
array_list(Pflat,Plist),
(multifor([I,J],1,N),param(Plist,Q)
do
Q[I,J] #= Pos,
gfd:element(Pos,Plist,I)
).


