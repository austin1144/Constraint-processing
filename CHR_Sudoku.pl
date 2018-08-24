:- use_module(library(chr)).
:- chr_constraint sep/1,row/1,sep_c/1,col/1,temp/2,sep_b/1,temp_b/2,block/1.
:- chr_constraint indomain/1, in/2,ins/2, diff/2, enum/1, equation/1,searchvalue/1,solve/1,p_empty/3,channel/2.
:- chr_option(debug, off). % on - off
:- chr_option(optimize, full). % full - off
:-op( 700,xfx,in).
:-op( 600,xfx,'..').
:-op( 700,xfx,ins).
%from second perspective
:- chr_constraint emptyq/2,q_empty/3,fillq/3,q_fact/3,channelrow/2.
:- chr_constraint channelcolumn/2,q_row/3,q_column/3,row_sum/3,column_sum/3.
:- chr_constraint combine/3,combine_diff/1,combine_end/1,searchflag/1,find/1.
:- chr_constraint diffcolumn/3,diffrow/3,test/3,test2/3,clean_flag/1,p_block/2.
:-[sudex_toledo].

%====== Experiment start ========%
test(Perspective):-
sudoku(lambda,Perspective).


maintest(Perspective):-
sudoku(lambda,Perspective),
sudoku(extra2,Perspective),
sudoku(sudowiki_nb49,Perspective).

extratest(Perspective):-
sudoku(extra4,Perspective),
sudoku(hard17,Perspective),
sudoku(eastermonster,Perspective).


%====== run by channelling perspective ========%	
sudoku(Name,3):-
statistics(cputime,T0),
puzzles(P,Name),channel(P,0),
sep(P),sep_c(P),sep_b(P),
generateblocks(P),
combineblk(1),
searchvalue(P),
statistics(cputime,T1),T is T1 - T0,
writeln("This is my Perspective1: "),writeln(P),
write('Channeling Time spend is: '),write(T),writeln('seconds.'),
clean_flag(1).

%====== run by channelling perspective *but search in Q========%	
sudoku(Name,4):-
statistics(cputime,T0),
puzzles(P,Name),channel(P,0),
sep(P),sep_c(P),sep_b(P),
generateblocks(P),
combineblk(1),
searchflag(1),
statistics(cputime,T1),T is T1 - T0,
writeln("This is my Perspective1: "),writeln(P),
write('Channeling Time spend is: '),write(T),writeln('seconds.'),
clean_flag(1).

%====== run by 1st perspective ========%	
sudoku(Name,1):- 
statistics(cputime,T0), puzzles(B,Name), sep(B),sep_c(B),sep_b(B),
searchvalue(B),
statistics(cputime,T1),
writeln("This is my Perspective1: "),writeln(B),
T is T1 - T0,write('Time spend is: '),write(T),writeln('seconds.').


%====== run by 2nd perspective ========%	
solve(Name,2):-
statistics(cputime,T0),
puzzles(P,Name),
generateblocks(P),
combineblk(1),
searchflag(1),
statistics(cputime,T1),T is T1 - T0,write('Time spend is: '),write(T),writeln('seconds.'),
clean_flag(1),
writeln("This is my Perspective1: "),writeln(P).

%====== channelling ========%	

channel([],_) <=> true.
channel([[]|T],Index) <=> channel(T,Index).
channel([[H1|T1]|Tail],Index) <=> NewIndex is Index+1, R is (NewIndex-1)//9+1,C is NewIndex-9*(R-1), Block is (C-1)//3+3*((R-1)//3)+1,
								channel([T1|Tail],NewIndex),p_empty(H1,Block,NewIndex).

p_empty(Number,B,Index),q_empty(Number,B,Index2) ==> ground(Number),var(Index2)|Index2 = Index.
p_empty(Number,B,Index),q_empty(N,B,Index)==> ground(Index),var(Number)|Number = N.
%we are curious about which method is more correct.
%p_empty(Number,B,Index),q_empty(Number,B,Index2) <=> ground(Number),var(Index2)|Index2 is Index, p_empty(Number,B,Index),q_empty(Number,B,Index2).
%p_empty(Number,B,Index),q_empty(N,B,Index) <=> ground(Index),var(Number)|Number is N, p_empty(Number,B,Index),q_empty(N,B,Index).

clean_flag(1)\p_empty(_,_,_)<=>true.




%====== solve in 1st perspective ========%	
% ----------- Seperate Row----------------
sep_row1	@sep([]) <=> true.
sep_row2	@sep([H|T]) <=> not(list_empty(H))| row(H), sep(T).

% ----------- Seperate Colume----------------
%temp (L,H1) L is the list length, H1 is the element
sep_col1	@sep_c([[]|T]) <=> sep_c(T).
sep_col2	@sep_c([[H1|T1]|Tail]) <=> listlength([H1|T1],L), temp(L,H1), sep_c([T1|Tail]).
sep_col3	@sep_c([]) <=> true.

com_el2el	@temp(L,H1),temp(L,H2) <=> not(is_list(H1)),not(is_list(H2))|temp(L,[H1,H2]).
com_el2list	@temp(L,H1),temp(L,H2) <=> is_list(H2)|append([H1],H2,NewT), temp(L,NewT).
trasfer2col @temp(_,List) <=> listlength(List,N),N is 9| col(List).

% ----------- Seperate Block----------------
sep_B1		@sep_b([[]|T]) <=> sep_b(T).
sep_B2		@sep_b([[H1|T1]|Tail]) <=> listlength([[H1|T1]|Tail],R),listlength([H1|T1],C),B is (C-1)//3+3*((R-1)//3)+1, temp_b(B,H1),sep_b([T1|Tail]).
sep_B3		@sep_b([]) <=> true.

com_el2el	@temp_b(B,H1),temp_b(B,H2) <=> not(is_list(H1)),not(is_list(H2))|temp_b(B,[H1,H2]).
com_el2list	@temp_b(B,H1),temp_b(B,H2) <=> is_list(H2)|append([H1],H2,NewT), temp_b(B,NewT).
trasfer2col @temp_b(_,List) <=> listlength(List,N),N is 9| block(List).

% -------All Different--------------
row(Sol1),col(Sol2),block(Sol3) <=> solve(Sol1),solve(Sol2),solve(Sol3).
solve(Sol) <=> Sol ins 1..9, all_different(Sol).





%====== solve in 2nd perspective ========%	
%=== generate viewpointQ's blocks
generateblocks(P):-
emptyq(1,1),
fillq(P,1,1).
%=CHR== geneqs+row+column
end		@emptyq(10,1)<=>true.
rowend	@emptyq(R,9)<=>R1 is R+1, q_empty(R,9,Index),Index in 1..81,emptyq(R1,1).
normal	@emptyq(R,C)<=>C1 is C+1, q_empty(R,C,Index),Index in 1..81,emptyq(R,C1).

%=CHR== add evidences
end		@fillq([[H]],9,9)<=>var(H),p_block(H,81);
					nonvar(H),q_fact(H,9,81),p_block(H,81).
rowend	@fillq([[H]|Rest],R,9)<=>R1 is R+1,(var(H),Index is R*9,p_block(H,Index);
					nonvar(H),Index is R*9,Block is 3*((R-1)//3)+3,q_fact(H,Block,Index),p_block(H,Index)),fillq(Rest,R1,1).
normal	@fillq([[H|T]|Rest],R,C) <=>C1 is C+1,(var(H),Index is R*9-9+C,p_block(H,Index);
					nonvar(H),Index is R*9-9+C,Block is (C-1)//3+3*((R-1)//3)+1,q_fact(H,Block,Index),p_block(H,Index)),fillq([T|Rest],R,C1).
%=CHR== absorb evidence
takefact@q_empty(Num,Block,Index1)\q_fact(Num,Block,Index2)<=>Index1 = Index2.
inblock	@q_empty(_,Block,Index)==>var(Index)|K is (Block+2)//3,L is 1+((Block-1) mod 3), 
		N1 is K*27+L*3-29,N2 is K*27+L*3-28,N3 is K*27+L*3-27,N4 is K*27+L*3-20,N5 is K*27+L*3-19,
		N6 is K*27+L*3-18,N7 is K*27+L*3-11,N8 is K*27+L*3-10,N9 is K*27+L*3-9,Index in [N1,N2,N3,N4,N5,N6,N7,N8,N9].
%===!!!check every number only appear once in every row / column.
%=CHR== !!!very important this:::
rowfinish	@diffrow(_,_,10)<=>true.
rowdiff		@diffrow(R,Index,N)<=>Nnew is N+1,Num is R*9-9+N,diff(Num,Index),diffrow(R,Index,Nnew).

colfinish	@diffcolumn(_,_,10)<=>true.
coldiff		@diffcolumn(C,Index,N)<=>Nnew is N+1,Num is N*9-9+C, diff(Num,Index),diffcolumn(C,Index,Nnew).


bothground	@q_empty(Num,_,Index1),q_empty(Num,_,Index2)==>ground(Index1),ground(Index2)|
			RNum1 is (Index1+8)//9,CNum1 is 1+((Index1-1) mod 9),RNum2 is (Index2+8)//9,CNum2 is 1+((Index2-1) mod 9),
			RNum1\==RNum2,CNum1\==CNum2.
oneground	@q_empty(Num,_,Index1),q_empty(Num,_,Index2)==>ground(Index1),var(Index2)|
			RNum1 is (Index1+8)//9,CNum1 is 1+((Index1-1) mod 9),diffrow(RNum1,Index2,1),diffcolumn(CNum1,Index2,1).


%===Combine all Q togethre
combineblk(1):-
combine(9,9,[]).

%=CHR==combine
printresult	@clean_flag(1)\combine(0,0,L)<=>ground(L)|writeln("This is my Perspective2: "),writeln(L).
search		@combine(0,0,L)\searchflag(1)<=>searchvalue(L).
end			@q_empty(1,1,Index)\combine(1,1,L)<=>add_diff(Index,L),combine(0,0,[Index|L]).
changerow	@q_empty(Num,1,Index)\combine(Num,1,L)<=>NewNum is Num -1,add_diff(Index,L),combine(NewNum,9,[Index|L]).
normal		@q_empty(Num,Block,Index)\combine(Num,Block,L)<=>NewBlock is Block -1,add_diff(Index,L),combine(Num,NewBlock,[Index|L]).

%=CHR==give back to P
pairPQ		@clean_flag(1)\q_empty(N1,_,Index),p_block(N2,Index)<=>N1 = N2.
clearflag	@clean_flag(1)<=>true.




%======CHRBASIC=========================
%===define in ..
inchange@	_ in A..B <=> A>B| fail.
inchange@	X in A..B <=> changetolist(A,B,[],L)|
								X in L.
inschange@	_ ins A..B <=> A>B| fail.
inschange@  X ins A..B <=> changetolist(A,B,[],L)|
								X ins L.
%===define in []
indelete		@ X in L <=> nonvar(X),member(X,L)|true.
infail			@ _ in [] <=> fail.
infail2			@ X in L <=> nonvar(X),not(member(X,L))|fail.
insetvalue		@ X in [V] <=> X = V.
inreducedomain	@ X in L1, X in L2 <=> intersection(L1,L2,L3)|
                     X in L3.
%===define ins []
instoinend	@ [] ins _ <=> true.
insnohead	@ [[]|T] ins L <=> T ins L.
instoin		@ [H|T] ins L <=> H in L, T ins L.

%===search&set value, the input can be anything

listempty		@searchvalue([]) <=> true.
listinlist		@searchvalue([[H|T]|L])<=>searchvalue([H|T]),searchvalue(L).
listnotempty	@searchvalue([X|L])<=>indomain(X),searchvalue(L).
onlynumorvar	@searchvalue(X)<=>indomain(X).

%===reduce domain by input order

valueset@	indomain(X)<=>ground(X)|
				true.
valuefind@	indomain(X),X in [H|T] <=> T =[_|_]|
			(X in [H]; X in T,indomain(X)).

%===diff element_add two new diff!!! can only take two elements!! no structure!!!
bothset@	diff(X,Y) <=> nonvar(X), nonvar(Y) | X \== Y.
yset@		diff(Y,X),X in L <=> nonvar(Y), select(Y,L,NL) | X in NL.
xset@		diff(X,Y),X in L <=> nonvar(Y), select(Y,L,NL) | X in NL.
ynotx_new@	X in L\diff(Y,X)<=> nonvar(Y), not(select(Y,L,_)) | true.
xnoty_new@	X in L\diff(X,Y)<=> nonvar(Y), not(select(Y,L,_)) | true.
%======PROLOG=====================
%===change .. to []
changetolist(A,A,L,[A|L]):-!.
changetolist(A,B,L,Lacc):-
Bnew is B-1, Bnew >= A,
changetolist(A,Bnew,[B|L],Lacc).

%===all_different
all_different([_]).
all_different([H|T]) :-
add_diff(H,T),
all_different(T).
%===add_different to list
add_diff(_A,[]).
add_diff(A,[H|T]):-
diff(A,H),
add_diff(A,T).



%--------------------prolog code----------------%
list_empty([]):-!.

% Gives the length of a list.
listlength([], 0 ).
listlength([_|Xs] , L ):- 
    listlength(Xs,N), 
    L is N+1. 


