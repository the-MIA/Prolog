
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

with Prolog.Term_Pkg;        use Prolog.Term_Pkg;
with Prolog.Transformations; use Prolog.Transformations;

package Prolog.Evaluable_Predicates is

--  The following evaluable predicates are implemented directly:
--
--     call, !, read, write, get0, put, nl, op, abort, end, trace,
--     notrace, atom, integer, var, name, stats, is, <, asserta, assertz
--     (= assert), functor, arg, list, nolist.
--
--  Other evaluable predicates are defined in the initialization file in
--  terms of either these or the following 'secret' predicates:
--
--       '$clenv', '$getcl', '$advcl', '$zap', '$online', '$debug'.
--
--  To each of the directly implemented evaluable predicates corresponds a
--  value of the enumerated type 'evalpred'.

    use Local;
    use Atom_Pkg;

    function Call_Eval_Pred (
       Call    : Term;
       E       : Env;
       Routine : Evalpred;
       Arity   : Integer;
       Dbase   : Integer_List) return Boolean;
    --  Call an evaluable predicate.

end Prolog.Evaluable_Predicates;


