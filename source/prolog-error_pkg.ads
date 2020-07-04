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

with Prolog.Errors; use Prolog.Errors;

package Prolog.Error_Pkg is

   subtype Error is Error_Type;

   type Moanaction is (Syntaxz, Abortz, Diez);
   --  Three types of action taken when an error occurs :
   --
   --      SyntaxZ : Syntax Error, store for reporting when the
   --                line is output.
   --      AbortZ  : Report Error and Abort Immediately.
   --      DieZ    : Report Error and DIE.

   Number_Of_Errors : Integer := 0;
   --  Has the number of syntax errors reported.

   procedure Moan (E : Error;
                   A : Moanaction);
   --  Rrror reporting

end Prolog.Error_Pkg;
