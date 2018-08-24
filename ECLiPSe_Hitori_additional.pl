:- use_module(library(ic)).
:- use_module(library(lists)).
%:- import alldifferent/1 from ic_global.
:-[puzzles].

take_method(Method,Heur):-
Method = 1,Heur = input_order;
Method = 2,Heur = first_fail.
        
        
hitori(Difficulity,Size,Method):-
take_method(Method,Heur),
puzzle(Difficulity,Size,Puzzles),
cputime(T0),
convert_list_array(Puzzles,Parray),
solution(Parray,Size,NewArray),
instantiate(Size,NewArray),
search(NewArray,0,Heur,indomain,complete,[backtrack(Back)]),
T is cputime - T0,
write('Under heuristic:\s'),write(Heur),
write('\s:\s'),write(T),writeln('seconds.'),
write('Backtrack:\s'),writeln(Back),
printpuzzle(NewArray,Size),
writeln('0 represent the blacked out cells').

printpuzzle(NewArray,Size):-
(for(I,1,Size),param(NewArray,Size)
do
(for(J,1,Size),param(NewArray,Size,I)
do
Num is NewArray[I,J],(Num>Size->write("  0");printf(" %2d",[Num]))
), nl
), nl.


convert_list_array(Puzzles,Parray):-
convert_listwitharray(Puzzles,P1dim),
eval_to_array(P1dim,Parray).

convert_listwitharray([],[]).
convert_listwitharray([Head|Rest],[Headlist|RestList]):-
Headlist =..[[]|Head],
convert_listwitharray(Rest,RestList).


solution(P,S,New):-
dim(New,[S,S]),
S2 is S*S+S,%maximum value can appear
New :: 1..S2,
(for(I,1,S),param(New)
do
alldifferent(New[I,*]),
alldifferent(New[*,I])
),
(multifor([I,J],1,S),param(P,S,New)
do
Ele is P[I,J],%take element
AC is P[I,*],array_list(AC,LColum),%take colum
AR is P[*,J],array_list(AR,LRow),%take row
%after delete this element, the same number still exsit.
%%start here
(
(delete(Ele,LColum,Temp),member(Ele,Temp);
delete(Ele,LRow,Temp),member(Ele,Temp))
->
(
#=(New[I,J],P[I,J],B1),
#=(New[I,J],I*S+J,B2),
B1+B2#=1
);
New[I,J]#=P[I,J]
)
)%%end multifor
,


%more constrains2:sandwich triple
Ssandwich is S-2,
(multifor([I,J],1,Ssandwich),param(S,New,P)
do
%start sandwich triple
(%horizontal
(P[I,J]#=P[I+1,J],P[I,J]#=P[I+2,J])->
(New[I,J]#>S,New[I+2,J]#>S,New[I+1,J]#=P[I+1,J]);true
),
(%vertical
(P[I,J]#=P[I,J+1],P[I,J]#=P[I,J+2])->
(New[I,J]#>S,New[I,J+2]#>S,New[I,J+1]#=P[I,J+1]);true
)
)%end multifor
%end sandwich triple
,
%more constrains3:Quad Corner
Squad is S-1,
( foreach(I,[1,1,Squad,Squad]), foreach(J,[1,Squad,1,Squad]),param(P,New,S) 
do
%writeln(I),writeln(J))
 (P[I,J]#=P[I+1,J],P[I,J]#=P[I,J+1],P[I,J]#=P[I+1,J+1])->
(New[I,J]#>S,New[I+1,J+1]#>S,New[I+1,J]#=P[I+1,J],New[I,J+1]#=P[I,J+1]);true)
%end Quad corner
,
%more constraints4:Triple Corner
((P[1,1]#=P[1,2],P[1,1]#=P[2,1])->
(New[1,1]#>S,New[1,2]#=P[1,2],New[2,1]#=P[2,1]);true),
((P[1,S]#=P[1,S-1],P[1,S]#=P[2,S])->
(New[1,S]#>S,New[1,S-1]#=P[1,S-1],New[2,S]#=P[2,S]);true),
((P[S,1]#=P[S-1,1],P[S,1]#=P[S,2])->
(New[S,1]#>S,New[S-1,1]#=P[S-1,1],New[S,2]#=P[S,2]);true),
((P[S,S]#=P[S-1,S],P[S,S]#=P[S,S-1])->
(New[S,S]#>S,New[S-1,S]#=P[S-1,S],New[S,S-1]#=P[S,S-1]);true)
%end Triple corner
,
%%more constraints1:check every black surronds by white
(multifor([I,J],1,S),param(S,New)
do
(%start black surround by white
New[I,J]#>S,
(I =1;I\=1,New[I-1,J]#=<S),
(I =S;I\=S,New[I+1,J]#=<S),
(J =1;J\=1,New[I,J-1]#=<S),
(J =S;J\=S,New[I,J+1]#=<S)
)%end black surround by white
;

(%start white at least one white
New[I,J]#=<S,
(I=1,B6=0;I\=1,#=<(New[I-1,J],S,B6)),
(I=S,B7=0;I\=S,#=<(New[I+1,J],S,B7)),
(J=1,B8=0;J\=1,#=<(New[I,J-1],S,B8)),
(J=S,B9=0;J\=S,#=<(New[I,J+1],S,B9)),
B6+B7+B8+B9#>=1
)%ebd white at least one white
)%end multifor
.

instantiate(S,New):-
dim(TempArray,[S,S]),
(multifor([I,J],1,S),param(S,New,TempArray)
do
#=<(New[I,J],S,B),
TempArray[I,J] #= B*(I*S+J-S)
),%multifor end

array_flat(1,TempArray,CheckArray),
array_list(CheckArray,CL),
delete_all_blacks(CL,[],CLnew),
check_all_connected(CLnew,[],S).

check_all_connected([],_,_).
check_all_connected([H|Rest],[],S):-
check_all_connected(Rest,[H],S).

check_all_connected(Rest,Done,S):-
delete(A,Rest,NewRest),%use as a generator
A1 is A-1, A2 is A+1,
A3 is A-S, A4 is A+S,
(
memberchk(A1,Done),!;memberchk(A2,Done),!;
memberchk(A3,Done),!;memberchk(A4,Done),!),
check_all_connected(NewRest,[A|Done],S).

delete_all_blacks([],CLnew,CLnew).
delete_all_blacks([A|Rest],Old,ACC):-
(
A =0,
delete_all_blacks(Rest,Old,ACC)
);
(
A \= 0,
delete_all_blacks(Rest,[A|Old],ACC)
).
