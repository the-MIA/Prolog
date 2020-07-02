% 1st year

   subject (cs108).
      fred     (cs108, 'Programming Principles 1a');
      homepage (cs108, 'http://www.cs.rmit.edu.au/~dale/index.html').
      year     (cs108, 1).
      semester (cs108, 1).

   subject (cs109).
      fred     (cs109, 'Programming Principles 1b');
      homepage (cs109, 'http://www.cs.rmit.edu.au/~dale/cs109/index.html').
      year     (cs109, 1).
      semester (cs109, 2).
      prereq   (cs109, cs108).

% 2nd year

   subject (cs208).
      fred     (cs208, 'Programming Principles 2');
      year     (cs208, 2).
      semester (cs208, 1).
      prereq   (cs208, cs109).

   subject (cs209).
      fred     (cs209, 'Data Structures');
      year     (cs209, 2).
      semester (cs209, 2).
      prereq   (cs209, cs109).

   subject (cs280).
      fred     (cs280, 'Software Engineering 1');
      year     (cs280, 2).
      semester (cs280, 1).
      prereq   (cs280, cs109).

% 3rd year

   subject (cs381).
      fred     (cs381, 'Software Engineering 2');
      homepage (cs381, 'http://www.cs.rmit.edu.au/~dale/cs381/index.html').
      year     (cs381, 3).
      semester (cs381, 1).
      prereq   (cs381, cs280).

   subject (cs504).
      fred     (cs504, 'Concurrent Computing');
      homepage (cs504, 'http://www.cs.rmit.edu.au/~dale/cs504/index.html').
      year     (cs504, 3).
      semester (cs504, 2).
      prereq   (cs504, cs208).
      prereq   (cs504, cs209).



% rules
   prereq_ancestor(C, A) :- prereq(C,B),(A=B; prereq_ancestor(B,A)).

% the following doesn't work
% all_prereqs (C,P):- setof(A, prereq_ancestor (C,A),P).
