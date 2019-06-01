:- dynamic(calculatescore/2).
:- use_module(library(jpl)).
play(Level,L):-
create(Board,L),

playing(Board,Level,max) .

playing(Board,_,_):-
member(Board,2048),
nl,write("WIN"),nl.

playing(B,_,_):-
move(up,B,B),move(down,B,B),move(left,B,B),move(right,B,B),nl,write("Game Over"),nl.

playing(Board,Level,Turn):-
move(up,Board,NewBoardU),
minmax(Board,NewBoardU,Level,SU),

move(down,Board,NewBoardD),
minmax(Board,NewBoardD,Level,SD),
move(left,Board,NewBoardL),
minmax(Board,NewBoardL,Level,SL),
move(right,Board,NewBoardR),
minmax(Board,NewBoardR,Level,SR),
checkturn(Turn,SU,SD,SL,SR,Result),
move(Result,Board,New),
add2_4(New,NewBoard),
write(Result),nl,
printboard(NewBoard),
changeturns(Turn,NewTurn),
playing(NewBoard,Level,NewTurn).
%%%%%%%%%%%%%%%%
minmax(Board,NewBoard,_,0):-
equal(Board,NewBoard).

minmax(_,Board,Level,Score):-
Level >= 0,
NewLevel is Level-1,
calculatescore(Board,Bscore),
asserta(calculatescore(Board,Bscore)),
 calculatenext(Board, 0, NewLevel, S0),
 calculatenext(Board, 1, NewLevel, S1),
 calculatenext(Board, 2, NewLevel, S2),
 calculatenext(Board, 3, NewLevel, S3),
 calculatenext(Board, 4, NewLevel, S4),
 calculatenext(Board, 5, NewLevel, S5),
 calculatenext(Board, 6, NewLevel, S6),
 calculatenext(Board, 7, NewLevel, S7),
 calculatenext(Board, 8, NewLevel, S8),
 calculatenext(Board, 9, NewLevel, S9),
 calculatenext(Board, 10, NewLevel, S10),
 calculatenext(Board, 11, NewLevel, S11),
 calculatenext(Board, 12, NewLevel, S12),
 calculatenext(Board, 13, NewLevel, S13),
 calculatenext(Board, 14, NewLevel, S14),
 calculatenext(Board, 15, NewLevel, S15),
 Score is Bscore+S0+S1+S2+S3+S4+S5+S6+S7+S8+S9+S10+S11+S12+S13+S14+S15.


%%%%%%%%%%%%%%%%
calculatenext([0,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], 0, Level, Score) :-
        Level >= 0,
        NewLevel is Level - 1,
        calculatemoves([2,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4],NewLevel,Score).
calculatenext([A1,0,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], 1, Level, Score) :-
        Level >= 0,
        NewLevel is Level - 1,
        calculatemoves([A1,2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4],NewLevel,Score).
calculatenext([A1,A2,0,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], 2, Level, Score) :-
        Level >= 0,
        NewLevel is Level - 1,
        calculatemoves([A1,A2,2,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4],NewLevel,Score).
calculatenext([A1,A2,A3,0,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], 3, Level, Score) :-
        Level >= 0,
        NewLevel is Level - 1,
        calculatemoves([A1,A2,A3,2,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4],NewLevel,Score).
calculatenext([A1,A2,A3,A4,0,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], 4, Level, Score) :-
        Level >= 0,
        NewLevel is Level - 1,
        calculatemoves([A1,A2,A3,A4,2,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4],NewLevel,Score).
calculatenext([A1,A2,A3,A4,B1,0,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], 5, Level, Score) :-
        Level >= 0,
        NewLevel is Level - 1,
        calculatemoves([A1,A2,A3,A4,B1,2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4],NewLevel,Score).
calculatenext([A1,A2,A3,A4,B1,B2,0,B4,C1,C2,C3,C4,D1,D2,D3,D4], 6, Level, Score) :-
        Level >= 0,
        NewLevel is Level - 1,
        calculatemoves([A1,A2,A3,A4,B1,B2,2,B4,C1,C2,C3,C4,D1,D2,D3,D4],NewLevel,Score).
calculatenext([A1,A2,A3,A4,B1,B2,B3,0,C1,C2,C3,C4,D1,D2,D3,D4], 7, Level, Score) :-
        Level >= 0,
        NewLevel is Level - 1,
        calculatemoves([A1,A2,A3,A4,B1,B2,B3,2,C1,C2,C3,C4,D1,D2,D3,D4],NewLevel,Score).
calculatenext([A1,A2,A3,A4,B1,B2,B3,B4,0,C2,C3,C4,D1,D2,D3,D4], 8, Level, Score) :-
        Level >= 0,
        NewLevel is Level - 1,
        calculatemoves([A1,A2,A3,A4,B1,B2,B3,B4,2,C2,C3,C4,D1,D2,D3,D4],NewLevel,Score).
calculatenext([A1,A2,A3,A4,B1,B2,B3,B4,C1,0,C3,C4,D1,D2,D3,D4], 9, Level, Score) :-
        Level >= 0,
        NewLevel is Level - 1,
        calculatemoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,2,C3,C4,D1,D2,D3,D4],NewLevel,Score).
calculatenext([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,0,C4,D1,D2,D3,D4], 10, Level, Score) :-
        Level >= 0,
        NewLevel is Level - 1,
        calculatemoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,2,C4,D1,D2,D3,D4],NewLevel,Score).
calculatenext([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,0,D1,D2,D3,D4], 11, Level, Score) :-
        Level >= 0,
        NewLevel is Level - 1,
        calculatemoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,2,D1,D2,D3,D4],NewLevel,Score).
calculatenext([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,0,D2,D3,D4], 12, Level, Score) :-
        Level >= 0,
        NewLevel is Level - 1,
        calculatemoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,2,D2,D3,D4],NewLevel,Score).
calculatenext([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,0,D3,D4], 13, Level, Score) :-
        Level >= 0,
        NewLevel is Level - 1,
        calculatemoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,2,D3,D4],NewLevel,Score).
calculatenext([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,0,D4], 14, Level, Score) :-
        Level >= 0,
        NewLevel is Level - 1,
        calculatemoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,2,D4],NewLevel,Score).
calculatenext([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,0], 15, Level, Score) :-
        Level >= 0,
        NewLevel is Level - 1,
        calculatemoves([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,2],NewLevel,Score).
calculatenext(_, _, _, 0).
%%%%%%%%%%%%%%%%
calculatemoves(Board,Level,Score):-
move(up,Board,NewBoardU),
minmax(Board,NewBoardU,Level,SU),
move(down,Board,NewBoardD),
minmax(Board,NewBoardD,Level,SD),
move(left,Board,NewBoardL),
minmax(Board,NewBoardL,Level,SL),
move(right,Board,NewBoardR),
minmax(Board,NewBoardR,Level,SR),
Score is SU+SD+SR+SL.
%%%%%%%%%%%%%%%%

checkturn(Turn,SU,SD,SL,SR,Result):-
Turn == max,
selectmaxmove(SU,SD,SL,SR,Result).

checkturn(Turn,SU,SD,SL,SR,Result):-
Turn == min,
selectminmove(SU,SD,SL,SR,Result).

%%%%%%%%%%%%%%%%
changeturns(max,min).
changeturns(min,max).
%%%%%%%%%%%%%%%%

equal([],[]).
equal([H1|T1],[H2|T2]) :-
        H1 == H2,
        equal(T1,T2).

%%%%%%%%%%%%%%%%
move(Move,Board,NewBoard):-
rotatematrix(Move,Board,Before),
maplist(tilesmix,Before,After),
rotatematrix(Move,NewBoard,After).



%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%
tilesmix([A,B,C,D], [A,B,C,D]) :- different(A,B), different(B,C), different(C,D).
tilesmix([A,A,B,B], [0,0,Ad,Bd]) :- square(A,Ad), square(B,Bd).
tilesmix([A,B,C,C], [0,A,B,Cd]) :- different(A,B), square(C,Cd).
tilesmix([A,B,B,C], [0,A,Bd,C]) :-	different(B,C), square(B,Bd).
tilesmix([A,A,B,C], [0,Ad,B,C]) :- different(A,B), different(B,C), square(A, Ad).
tilesmix([A,B,C,C], [0,A,B,Cd]) :- different(A,B), different(B,C), square(C,Cd).

%%%%%%%%%%%%%%%%
rotatematrix(up,[A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], [[D1,C1,B1,A1],[D2,C2,B2,A2],[D3,C3,B3,A3],[D4,C4,B4,A4]]).
rotatematrix(down, [A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], [[A1,B1,C1,D1],[A2,B2,C2,D2],[A3,B3,C3,D3],[A4,B4,C4,D4]]).
rotatematrix(left, [A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], [[A4,A3,A2,A1],[B4,B3,B2,B1],[C4,C3,C2,C1],[D4,D3,D2,D1]]).
rotatematrix(right, [A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4], [[A1,A2,A3,A4],[B1,B2,B3,B4],[C1,C2,C3,C4],[D1,D2,D3,D4]]).

%%%%%%%%%%%%%%%%

different(A,B):-
A \= B.

%%%%%%%%%%%%%%%%
% max score
selectmaxmove(SU,SD,SL,SR,up):-
SU >= SD,
SU >= SL,
SU >= SR.

selectmaxmove(SU,SD,SL,SR,down):-
SD >= SU,
SD >= SL,
SD >= SR.

selectmaxmove(SU,SD,SL,SR,left):-
SL >= SD,
SL >= SU,
SL >= SR.

selectmaxmove(SU,SD,SL,SR,right):-
SR >= SD,
SR >= SL,
SR >= SU.
%%%%%%%%%%%%%%%%%
% min score

selectminmove(SU,SD,SL,SR,up):-
SU < SD,
SU < SL,
SU < SR.

selectminmove(SU,SD,SL,SR,down):-
SD < SU,
SD < SL,
SD < SR.

selectminmove(SU,SD,SL,SR,left):-
SL < SD,
SL < SU,
SL < SR.

selectminmove(SU,SD,SL,SR,right):-
SR < SD,
SR < SL,
SR < SU.

%%%%%%%%%%%%%%%%
return(0,[2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).
return(1,[0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).
return(2,[0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0]).
return(3,[0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0]).
return(4,[0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0]).
return(5,[0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0]).
return(6,[0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0]).
return(7,[0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0]).
return(8,[0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0]).
return(9,[0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0]).
return(10,[0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0]).
return(11,[0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0]).
return(12,[0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0]).
return(13,[0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0]).
return(14,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0]).
return(15,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2]).


%%%%%%%%%%%%%%%%

create(Board,L):-
return(L,B),
add2_4(B,Board),
printboard(Board).
%%%%%%%%%%%%%%%%
add2_4(Board,NewBoard):-
numberofzeros(Board,N),
index(N,I),
changezero(Board,I,2,NewBoard).
%%%%%%%%%%%%%%%%
numberofzeros([],0).
numberofzeros([H|T],N):-
      H==0,
      numberofzeros(T,N1),
      N is N1+1.

numberofzeros([H|T],N):-
      H\=0,
      numberofzeros(T,N).
%%%%%%%%%%%%%%%%
index(Listofzeros,I):-
    I is random(Listofzeros).
%%%%%%%%%%%%%%%%
choose2_4(R):-
random(1,10,C),
C<5->R = 2;R = 4.
%%%%%%%%%%%%%%%%
changezero([],_,_,[]).
changezero([H|T],I,C,[NH|NT]):-
  H==0,
  I==0,
  NH is C,
  changezero(T,-1,C,NT).

changezero([H|T],I,C,[NH|NT]):-
  H==0,
  I >0,
  I1 is I-1,
  NH is H,
  changezero(T,I1,C,NT).

changezero([H|T],I,C,[NH|NT]):-
  H==0,
  I<0,
  NH is H,
  changezero(T,I,C,NT).

changezero([H|T],I,C,[NH|NT]):-
  H \= 0,
  NH is H,
  changezero(T,I,C,NT).
%%%%%%%%%%%%%%%%
calculatescore(Board,Score):-
  sum_list(Board,Score).

%%%%%%%%%%%%%%%%
square(N,NN):-
NN is N * 2.
%%%%%%%%%%%%%%%%
printboard([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4]) :-
        format(   '--------------------~n'),
        row([A1,A2,A3,A4]),
        row([B1,B2,B3,B4]),
        row([C1,C2,C3,C4]),
        maplist(print, [D1,D2,D3,D4]),
        format('~n--------------------~n').

row([A,B,C,D]) :- maplist(print, [A,B,C,D]),format(' ~n').
print(0) :- format(' __ ').

print(X) :- member(X,[0,2,4,8]), format('  ~d ', X).
print(X) :- member(X,[16,32,64]), format(' ~d ', X).
print(X) :- member(X,[128,256,512]), format('  ~d', X).
print(X) :- member(X,[1024,2048]), format(' ~d', X).
%%%%%%%%%%%%%%%%


interface3(P,W2) :-


atom_concat(P,W2,W3),
        jpl_new('javax.swing.JFrame', ['GUI'], F),
        jpl_new('javax.swing.JLabel',['--- 2048 Game ---'],LBL),
        jpl_new('javax.swing.JPanel',[],Pan),
        jpl_call(Pan,add,[LBL],_),
        jpl_call(F,add,[Pan],_),
        jpl_call(F, setLocation, [400,300], _),
        jpl_call(F, setSize, [400,300], _),
        jpl_call(F, setVisible, [@(true)], _),
        jpl_call(F, toFront, [], _),
jpl_call('javax.swing.JOptionPane', showMessageDialog, [F,W3], N),
jpl_call(F, dispose, [], _),
        /*write(N),nl,*/
        (	N == @(void)
                ->	write('')
                ;	write("")
        ).

%%%%%%%%%%%%%%%%
