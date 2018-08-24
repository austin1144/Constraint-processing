:- use_module(library(chr)).
:- chr_constraint in/2,ins/2,searchvalue/1,indomain/1,diff/2.
:- chr_constraint genblk/3,block/3,cut_r/2,cut_c/2,row_org/2,column_org/2,row_gen/2,column_gen/2.
:- chr_constraint puzzlesize/1,newblock/3,checkc/4,checkr/4,blackblock/2,combine/3,searchflag/1.
:- chr_constraint whiteblock/2,conblock/2,checkwhite/1,checkblack/1,cleaning/1,getresult/1.
:- chr_constraint sandwich/3,triple/3.
:- chr_option(debug, off). % on - off
:- chr_option(optimize, full). % full - off
:-op( 700,xfx,in).
:-op( 700,xfx,ins).
:-op( 600,xfx,'..').
:-[puzzles].

hitori(Difficulity,Size):-
statistics(cputime,T0),
puzzle(Difficulity,Size,P),
genblk(P,1,1),
generateR_C(P),
combineblk(1),
checkwhite(1),
searchflag(1),
getresult(Pnew),
statistics(cputime,T1),T is T1 - T0,write('Time spend is: '),write(T),writeln('seconds.'),
present(Pnew,Ppre,Size),
writeln(Ppre),write('All the numbers equal 0 are blacked out.').


present([],[],_).
present([[Head|[R|Rest]]|Taillist],[[Head2|[R2|Rest2]]|Taillist2],Size):-
(Head =< Size->Head2 = Head;Head2 = 0),
present([[R|Rest]|Taillist],[[R2|Rest2]|Taillist2],Size).
present([[Head]|Taillist],[[Head2]|Taillist2],Size):-
(Head =< Size->Head2 = Head;Head2 = 0),
present(Taillist,Taillist2,Size).

%=CHR==generate blocks
lastblock	@genblk([[This|[]]|[]],N,N)<=>block(N,N,This),puzzlesize(N).
lastinrow	@genblk([[This|[]]|Rest],R,C)<=>Rest\==[]|block(R,C,This),Rnew is R +1,genblk(Rest,Rnew,1).
normal		@genblk([[This|Tail]|Rest],R,C)<=>block(R,C,This),Cnew is C +1,genblk([Tail|Rest],R,Cnew).

%=CHR==addition constraint1 sandwich triple test at hitori(1,5)
findsandwich@puzzlesize(N),block(R,C1,Value),block(R,C2,Value),block(R,C3,Value)==>C3 is C1+2,C2 is C1+1|
			sandwich(R,C2,Value),Vblc1 is N*R+C1,sandwich(R,C1,Vblc1),Vblc2 is N*R+C3,sandwich(R,C3,Vblc2).
eatsandwich @newblock(R,C,Value2)\sandwich(R,C,Value)<=>Value2 = Value.
%=CHR==addition constraint2 triple coner test at hitori(4,5)
findtriple	@puzzlesize(N),block(1,1,V),block(1,2,V),block(2,1,V)==>
			Vblc is N*1+1,triple(1,1,Vblc),triple(1,2,V),triple(2,1,V).
findtriple	@puzzlesize(N),block(1,N,V),block(1,C2,V),block(2,N,V)==>C2 is N-1|
			Vblc is N*1+N,triple(1,N,Vblc),triple(1,C2,V),triple(2,N,V).
findtriple	@puzzlesize(N),block(N,1,V),block(N,2,V),block(R2,1,V)==>R2 is N-1|
			Vblc is N*N+1,triple(N,1,Vblc),triple(N,2,V),triple(R2,1,V).
findtriple	@puzzlesize(N),block(N,N,V),block(N,C2,V),block(R2,N,V)==>R2 is N-1,C2 is N-1|
			Vblc is N*N+N,triple(N,N,Vblc),triple(N,C2,V),triple(R2,N,V).
eattriple	@newblock(R,C,Value2)\triple(R,C,Value)<=>Value2 = Value.

%=CHR==new blocks, get a new range
canbeblack	@puzzlesize(N),row_org(Rlist,R),column_org(Clist,C)\block(R,C,V)<=>(select(V,Rlist,NewR),member(V,NewR);select(V,Clist,NewC),member(V,NewC))|
							Vblc is N*R+C,newblock(R,C,Vnew),Vnew in [V,Vblc].
number		@puzzlesize(_),row_org(_,R),column_org(_,C)\block(R,C,V)<=>newblock(R,C,V).
%=========important constraints start here
%=CHR==check all different. start from 1st till n.
checkallcl	@checkc(L,N,N,_)<=>length(L,N)|all_different(L).
checkcl1	@puzzlesize(N),newblock(1,C,V)==>checkc([V],1,N,C).
checkc+1	@puzzlesize(N),newblock(R,C,V)\checkc(L,Count,N,C)<=>R is Count+1|checkc([V|L],R,N,C).

checkrowall	@checkr(L,N,N,_)<=>length(L,N)|all_different(L).
checkrow1	@puzzlesize(N),newblock(R,1,V)==>checkr([V],1,N,R).
checkr+1	@puzzlesize(N),newblock(R,C,V)\checkr(L,Count,N,R)<=>C is Count+1|checkr([V|L],C,N,R).
%=CHR==check every black surround by white
check		@puzzlesize(N),newblock(R,C,V)==>nonvar(V),V > N|blackblock(R,C).

up			@blackblock(R,C),puzzlesize(N),newblock(R0,C,Vw)==>R0 is R-1|Vw in 1..N.
left		@blackblock(R,C),puzzlesize(N),newblock(R,C0,Vw)==>C0 is C-1|Vw in 1..N.
right		@blackblock(R,C),puzzlesize(N),newblock(R,C2,Vw)==>C2 is C+1|Vw in 1..N.
down		@blackblock(R,C),puzzlesize(N),newblock(R2,C,Vw)==>R2 is R+1|Vw in 1..N.




%=CHR==check white connection
generatewit @puzzlesize(N),newblock(R,C,V)==>nonvar(V),V =< N|whiteblock(R,C).
connecstart	@checkwhite(1),whiteblock(R1,C1)<=>conblock(R1,C1),checkwhite(2).
connecting	@checkwhite(2),conblock(R1,C1)\whiteblock(R2,C2)<=>Sum is abs(R2-R1)+abs(C2-C1),Sum=1|
										conblock(R2,C2).

check		@checkwhite(3),whiteblock(_,_)<=>false.
check		@checkwhite(3),checkwhite(2)<=>checkwhite(4).


%=CHR==Cleaning
cleanflg	@checkwhite(4)<=>cleaning(1).
cleanr		@cleaning(1)\row_org(_,_)<=>true.
cleanc		@cleaning(1)\column_org(_,_)<=>true.
cleanblack	@cleaning(1)\blackblock(_,_)<=>true.
cleanshits	@cleaning(1)\conblock(_,_)<=>true.
cleanblocks @cleaning(1)\newblock(_,_,_)<=>true.
cleansize	@cleaning(1)\puzzlesize(_)<=>true.
%=========important constraints end here

%=========important system CHR start here

%===
cleaning(1),getresult(Pnew1),combine(Pnew,1,1)<=>Pnew=Pnew1.


%===combine back to P
combineblk(1):-
combine([],0,0).


%=CHR==combine all new block to result.
combine(L,1,1)\searchflag(1)<=>searchvalue(L),checkwhite(3).
puzzlesize(N),newblock(N,N,V)\combine([],0,0)<=>combine([[V]],N,N).
puzzlesize(N),newblock(R1,N,V)\combine(T,R,1)<=>R1 is R-1|combine([[V]|T],R1,N).
newblock(R,C1,V)\combine([L|Tail],R,C)<=> C1 is C -1|combine([[V|L]|Tail],R,C1).



%===cut to column
cut_to_column([],[],[]).
cut_to_column([[Hof1r|Tof1r]|Tailofold],[Tof1r|Tailofnew],[Hof1r|Tailofc]):-
cut_to_column(Tailofold,Tailofnew,Tailofc).
%===cut to row
cut_to_row([R|Tail],Tail,R).

%===check every row, column
generateR_C(P):-
cut_r(P,1),cut_c(P,1).

%=CHR==cut to new colunm
cut_c([[]|_],_)<=>true.
cut_c(P,RN)<=>cut_to_column(P,Pnew,C)|RN_new is RN +1, column_org(C,RN),cut_c(Pnew,RN_new).


%=CHR==cut to new rows
cut_r([],_)<=>true.
cut_r(P,CN)<=>cut_to_row(P,Pnew,R)|CN_new is CN +1, row_org(R,CN),cut_r(Pnew,CN_new).











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

