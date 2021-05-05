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

--  This generic package implements a facility for variable length strings
--  in a table.
--  Each string is built up character by character in a special string
--  variable. When KeepVar is called, the information about the start
--  position and length of the variable is inserted into a table of
--  strings.
--  This implies that only one string can be built at a time, per
--  generic instantiation

--  One advantage of this scheme is its very fast access times, and
--  insertions. One disadvantage over using something like Unbounded_String
--  is the requirement to instantiate the pacakge/more general fiddling

--  This is similar to a storage pool for a give set of variables.


generic

    Var_Table_Size : Natural;
    --  Number of characters in all variables.

    MaxVars : Natural;
    --  Maximum number of variable length strings

package Prolog.Var_Table is

   type Varstring is private;

   Anon_String : constant Varstring;

   Null_String : constant Varstring;

   Varcount    : Natural range 0 .. MaxVars := 0;
   --  Number of variable length strings stored

   procedure Start_Var;
   --  Start a new variable.

   procedure Var_Char (C : Character);
   --  Add a character to the variable name.

   function Keep_Var return Varstring;

   function To_String (V : Varstring) return String;

private
   type Varstring is record
      Index : Integer range 0 .. Var_Table_Size;
      Length : Integer;
   end record;

   Anon_String : constant Varstring := (0, 1);
   Null_String : constant Varstring := (0, 0);

end Prolog.Var_Table;
