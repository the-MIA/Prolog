
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

package Prolog.Write_Out is

   --  WriteOut writes a term to the text file 'OUTPUT', using operator
   --  information in atom entries to select the best syntax.  The current
   --  version does not quote atoms, even if they contain spaces or wierd
   --  characters. Of course, this is sometimes just what is needed!

   use Prolog.Transformations.Local;

   Writelength : Natural := 100;
   --  Maximum list length allowed.

   Writedepth : Natural := 200;
   --  Depth of terms.

   Quoteflag : Boolean := True;
   Debugging : Boolean := False; --  Is the interpreter being debugged?

   type Tracemessage is (Goald, Provedd);

   procedure Writeout (X : Term; E : Env);
   --  Write a term.

   procedure Trace (M      : Tracemessage;
                    X      : Term;
                    E      : Env;
                    Indent : Integer      := 0);
   --  Output a trace message.

end Prolog.Write_Out;
