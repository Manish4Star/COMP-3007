/*Muhammad Mustafa 100823576
 * COMP 3007 - A4
 * Solution
 *
 *Question 1*/
male(mustafa).
male(awais).
male(sabir).
male(idrees).
male(safee).

female(rabeeha).
female(rasheeda).
female(raiha).
female(saleha).

parent(sabir,mustafa).
parent(sabir,awais).
parent(rabeeha,mustafa).
parent(rabeeha,awais).

parent(saleha,rabeeha).
parent(saleha,rasheeda).
parent(saleha,safee).
parent(saleha,raiha).
parent(idrees,rabeeha).
parent(idrees,rasheeda).
parent(idrees,safee).
parent(idrees,raiha).

father(idrees,safee).
father(idrees,rabeeha).
father(idrees,rasheeda).
father(idrees,raiha).
father(sabir,mustafa).
father(sabir,awais).

mother(saleha,safee).
mother(saleha,rabeeha).
mother(saleha,rasheeda).
mother(saleha,raiha).
mother(rabeeha,mustafa).
mother(rabeeha,awais).

married(idrees,saleha).
married(sabir,rabeeha).

different(X,Y) :- X\=Y.
is_mother(X) :- female(X),parent(X,_).
is_father(X) :- male(X),parent(X,_).
aunt(X,Y) :- female(X),parent(W,Y),parent(Z,X),parent(Z,W),not(parent(X,Y)),!.
uncle(X,Y) :- male(X),parent(W,Y), parent(Z,X), parent(Z,W),not(parent(X,Y)),!.
sister(X,Y) :- female(X),parent(Z,X), parent(Z,Y),!.
brother(X,Y) :- male(X),parent(Z,X), parent(Z,Y),!.
grandfather(X,Y) :- male(X),parent(X,Z), parent(Z,Y),!.
grandmother(X,Y) :- female(X),parent(X,Z), parent(Z,Y),!.
ancestor(X,Y) :- parent(X,Y); parent(X,Z), ancestor(Z,Y),!.


/*Question 2*/
actor(jonny, depp, gender(male)).
actor(bruce, willis, gender(male)).
actor(glenn, close, gender(female)).
actor(orlando, bloom, gender(male)).
actor(jennifer, lawrence, gender(female)).
actor(sean, bean, gender(male)).
actor(angelina, jolie, gender(female)).
actor(keira, knightley, gender(female)).
actor(benedict, cumberbatch, gender(male)).

movie(year(2003), title([pirates,of,the,carribean]), cast([actor(jonny, depp), actor(keira, knightley), actor(orlando, bloom)])).
movie(year(2001), title([lord,of,the,rings]), cast([actor(orlando, bloom), actor(sean, bean)])).
movie(year(1988), title([die,hard]), cast([actor(bruce, willis)])).
movie(year(2014), title([the,imitation,game]), cast([actor(benedict, cumberbatch), actor(keira, knightley)])).
movie(year(2012), title([the,hunger,games]), cast([actor(jennifer,lawrence)])).
/* QUERIES FOR QUESTION 2:
 a) ?- movie(year(X),Title,_), X>2002.
 b) ?- movie(_,title(X),_),member(of,X).
 c) ?- movie(_,title(X1),_),movie(_,title(X2),_),member(Y,X1),member(Y,X2),X1\=X2.
 d) ?- actor(X,Y,gender(female)).
 e) ?- movie(_,Title,cast(X)),member(actor(Y,Z),X),actor(Y,Z,gender(female)).
 f) ?- movie(_,Title,cast(X)),member(actor(orlando, bloom),X).
 g) ?- movie(_,Title1,cast(L1)),!,movie(_,Title2,cast(L2)),member(actor(X,Y),L1),member(actor(X,Y),L2),Title1\=Title2.
 h) ?- actor(X,Y,_),movie(_,title([pirates,of,the,carribean]),cast(L1)),movie(_,title([lord,of,the,rings]),cast(L2)),movie(_,title([die,hard]),cast(L3)),movie(_,title([the,imitation,game]),cast(L4)),movie(_,title([the,hunger,games]),cast(L5)),not(member(actor(X,Y), L1)),not(member(actor(X,Y),L2)),not(member(actor(X,Y),L3)),not(member(actor(X,Y),L4)),not(member(actor(X,Y),L5)).
 */

/*Question 3*/
hanoi(1,L,R,_) :-
    write('Move upper most disk from '),
    write(L),
    write(' to '),
    write(R),
    nl.
hanoi(N,L,R,C) :-
    N>1,
    M is N-1,
    hanoi(M,L,C,R),
    hanoi(1,L,R,_),
    hanoi(M,C,R,L).


/*Question 4*/
mylast(A,[A]).
mylast(A,[_|Rest]) :- mylast(A,Rest).


after(A,[A|Rest],Rest).
after(A,[A|[Head|Tail]],R) :- Tail\=[], after(A,[Head|Tail],R).
after(A,[Head|Tail],R) :- A\=Head, after(A,Tail,R).


nextto(A,B,[A,B|_]).
nextto(A,B,[A,C|Tail]) :- C\=B, nextto(A,B,[C|Tail]).
nextto(A,B,[Head|Tail]) :- A\=Head, nextto(A,B,Tail).

occurs_at_position(A,[A|_],0).
occurs_at_position(A,[_|Tail],B) :- occurs_at_position(A,Tail,C), B is 1+C.


/*Question 5*/
myAppend([],X,X).
myAppend([X|Y],A,[X|B]) :- myAppend(Y,A,B).

myLast(X,L):-myAppend(_,X,L).

nextTo(X,Y,L):-myAppend(_,[X,Y|_],L).

myReverse([],[]).
myReverse([H|T],R1):- myReverse(T,R2), myAppend(R2,[H],R1).


/*Question 6 */
edge(a,b,2).
edge(b,c,2).
edge(c,d,2).
edge(a,d,2).
edge(e,a,2).

connected(X,Y):-edge(X,Y,_).
connected(X,Y):-edge(X,A,_),connected(A,Y).

pathLength(X,Y,R):-edge(X,Y,R).
pathLength(X,Y,R):-edge(X,A,D),pathLength(A,Y,T), R is D+T.

listForPath(X,Y,L):-append([edge(X,Y,T)],[],L).





































