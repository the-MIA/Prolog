woman (liz).
woman (sue).
woman (ann).
woman (jan).
man (russell).
man (tim).
mother (liz, sue).
mother (sue, ann).
mother (sue, jan).
man (jim).
man (bob).
man (bill).
mother (X) :- woman(X).
married (X,Y) :- man (X), woman (Y).
grandmother (X,Y) :- mother (X,Z), mother (Z,Y).
