
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

package Prolog.Read_In is

   Readsize : Natural := 2000; -- Max depth of stack on Readin.

   Readdepth : Natural := 500; -- Max depth of recursion on Readin.


   --  ReadIn reads a Prolog sentence from the current input file and builds
   --  a term from it.  The sentence is parsed using a shift- reduce parsing
   --  algorithm which depends on operator information in atom entries.

   function Read_In return Term;
   --  Input and parse a Prolog sentence and build a term from it.

   procedure Reset_Lexan;

end Prolog.Read_In;
