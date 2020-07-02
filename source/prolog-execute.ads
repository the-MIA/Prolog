
--  Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992 by the Program
--  Analysis and Verification Group, Leland Stanford Junior University.
--  All Rights Reserved.
--
--  This file is part of the Anna tools.  You may copy, modify, and
--  distribute the Anna tools under the conditions described in the Anna
--  General Public License.  A copy of this license should be in a file
--  named COPYING.
--
--  LELAND STANFORD JUNIOR UNIVERSITY ALLOWS FREE USE OF THIS SOFTWARE IN
--  ITS "AS IS" CONDITION.  LELAND STANFORD JUNIOR UNIVERSITY DISCLAIMS
--  ANY LIABILITY OF ANY KIND FOR ANY DAMAGES WHATSOEVER RESULTING FROM
--  THE USE OF THIS SOFTWARE.
----------------------------------------------------------------------

with Prolog.Term_Pkg; use Prolog.Term_Pkg;

package Prolog.Execute is

--  Execute is the finite state control of the abstract Prolog machine. It
--  executes the goal 'goalp' by manipulating the local and global stacks
--  (3) , and uses Unify  (9)  to match goals against clauses from the
--  database  (8) . CallEvalPred  (11)  handles evaluable predicates.

--  Unify implements the unification algorithm, which finds the most
--  general common instance of a pair of terms. It performs the matching
--  substitution by introducing variable bindings.  As tradition dictates,
--  no occurrence check is made before variable bindings are introduced.
--  Unify is used by Execute  (10)  to match goals against the heads of
--  clauses in the database.


    procedure First_Answer (
       Goalp   : in      Term;
       Envp    : in      Integer;
       Dbase   : in      Integer_List;
       Ans     :     out Solution;
       Success :     out Boolean);
    --  Execute a goal.
   
    procedure Next_Answer (
       Ans     :     out Solution;
       Success :     out Boolean);

    function Unify (
       X1, X2 : Term;
       E1, E2 : Integer;
       Depth  : Integer) return Boolean;
    --  Unify x1 and x2.  Perform the matching substitution by binding variables.

end Prolog.Execute;
