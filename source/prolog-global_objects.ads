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

package Prolog.Global_Objects is

   --  Global flags :

   Online : Boolean := False;
   --  Is the interpreter being used ONLINE?

   Haltflag : Boolean := False;
   --  Set by 'end' and tested by the Top Level.

   Tracing : Boolean;

   type Phase is (Userm, Progm, Sysm);
   Mode : Phase := Userm;
   --  User mode , program mode (reading program file) or
   --  System mode (reading system file). These are set by the
   --  appropriate packages.

   --  Global types :

   --  Io :

   Maxindepth  : constant := 7; --  Max number of files open for input.
   Maxoutdepth : constant := 7; --  Max number of files open for output.
   Rightmargin : constant := 78; --  The right margin on output.

   --  Constants for the Atom Table.

   Atom_Table_Size : constant := 10000;
   --  Maximum number of CHARACTERS in ALL the atoms.

   Hash_Table_Size : constant := 999;
   --  Number of entries in the atom hash table.

   --  Constants for Var Table.

   Var_Table_Size : constant := 10000;
   --  Number of characters in the var table.

   Maxvars : constant := 200;
   --  Maximum number of variables in a term.

   --  Readin and WriteOut :

   Maxdepth : constant := 200;

   Maxprec : constant := 1200;
   Subprec : constant := 999;

   type Chtype is (Smallc, Largec, Digitc, Specialc, Spacec, Wierdc);
   type Charray is array (Character) of Chtype;

   Charclass : Charray :=
     Charray'(
              'a' .. 'z'       => Smallc,

              'A' .. 'Z' | '_' => Largec,

              '0' .. '9'       => Digitc,

              '+' | '-' | '*' | '/' | '^' |
                '<' | '>' | '=' | '`' | '~' |
                '\' | ':' | '.' | '?' | '@' |
                '#' | '$' | '&' | '%' | '|'
                => Specialc,

              ' '
                | ASCII.HT
                | ASCII.LF       => Spacec,

              others           => Wierdc);

   --  Local stack :

   Maxframes  : constant := 10000;
   Maxlocsize : constant := 50000;

   Debug : Boolean := False;

end Prolog.Global_Objects;
