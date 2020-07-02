(P,Q) :- P , Q.
(P;Q) :- P. (P;Q) :- Q.
true.
repeat.
X=:=Y:-Z is X,Z is Y.
X=/=Y:-Z is X,Z is Y,!,fail. X=/=Y.
X>Y:-Y<X. 
X=<Y:-X<Y,!. X=<Y:-Z is X,Z is Y.
X>=Y:-Y<X,!. X>=Y:-Z is X,Z is Y. 
X=X.
X/=X:-!,fail. X/=Y.   
length(L,_):-var(L),!,fail. length([_|L],N):-!,length(L,N1),N is N1+1.  
length([],0).
not P:-P,!,fail. not P.
X==Y:-X/==Y,!,fail. X==Y.
X/==Y:-'$same'(X,Y),!,fail. X/==Y.
atomic(X) :- atom(X),!. atomic(X) :- integer(X).
'$same'(X,Y):-var(X),!,var(Y),'$samev'(X,Y). '$same'(X,Y):-var(Y),!,fail.
'$same'(X,Y):-atomic(X),!,X=Y. '$same'(X,Y):-atomic(Y),!,fail.  
'$same'(X,Y):-X=.. [F|Ax],Y=.. [F|Ay],'$samea'(Ax,Ay).  
'$samea'([],[]):-!. '$samea'([X|Ax],[Y|Ay]):-'$same'(X,Y),'$samea'(Ax,Ay).
nonvar(X) :- var(X),!,fail. nonvar(X).
T=.. [F|A]:-nonvar(T),!,functor(T,F,_),'$alist'(1,T,A). 
T=.. [F|A]:-atom(F),!,length(A,N),functor(T,F,N),'$alist'(1,T,A).   
T=.. [T]:-integer(T).   
'$alist'(I,T,A):-arg(I,T,X),!,A=[X|A1],I1 is I+1,'$alist'(I1,T,A1). 
'$alist'(_,_,[]).
