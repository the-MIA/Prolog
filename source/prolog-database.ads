
--  Copyright  (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992 by the Program
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

package Prolog.Database is

   --  The clauses which constitute the Prolog program are stored in skeletal
   --  Form, with each variable replaced either by an anonymous
   --  variable or by
   --  a skeletal reference containing an offset from the base of a frame on
   --  the local stack.  The body of a clause is represented by a collection
   --  of terms chained together by the 'brother' fields of their root nodes.

   use Prolog.Transformations.Local;
   --  For type Env.

   function Addclause (P      : Term;
                       E       : Env;
                       Dbase   : Integer;
                       Asserta : Boolean) return Clptr;

   procedure Addclause (C      : in out Clptr;
                        Dbase   : in     Integer;
                        Asserta : in     Boolean);

   --  Produce a skeleton for p and add it to the database.  The new clause
   --  is added at the front of the clause chain if asserta is true,
   --  otherwise at the end. Terms are always accompnied by their environments.


   function Makeclause (Head : Term; Tail : Term; E : Env) return Clptr;
   function Makeclause (P : Term; E : Env) return Clptr;
   --  Return a skeleton for P.

   procedure Findclause (X     : in     Term;
                         E     : in     Env;
                         Dbase : in out Integer_List;
                         Cl    : in out Clptr;
                         Value :    out Boolean);

   procedure Findclause (X    : in     Term;
                         E     : in     Env;
                         Dbase : in     Integer;
                         Cl    : in out Clptr;
                         Value :    out Boolean);
   --  Set cl to the first applicable clause. After this other 'procedure'
   --  entries are found by following the clptr chain on the 'proc'
   --  entry of the atom. Subsequent calls to FINDCLAUSE advance the pointer.

   procedure Zapclause (Cl : Clptr);
   --  Delete the clause entry pointed to by CL.

end Prolog.Database;
