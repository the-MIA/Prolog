
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

--  with Prolog.Global_Objects; use Prolog.Global_Objects;

generic

    Atom_Table_Size : Natural;
    -- number of characters in the table.

    Hash_Table_Size : Natural;

    type Atom_Info is private;

    with function Default_Info return Atom_Info;

package Prolog.Atom_Table is

   --  Each atom is associated with operator and clause information which is
   --  stored in an 'atomentry'.  The identifiers for atoms in the input are
   --  mapped to the corresponding entry through a hash table.
   --  Collisions are handled by chaining together atom entries.

   --  In case of an ERROR a call is made to the error package made
   --  visible via GLOBAL_OBJECTS.

   type Atom is private;

   Null_Atom : constant Atom;

   procedure Startatom;
   --  Start an atom. All the characters passed to AtomChar
   --  are considered part of this atom till a Lookup is done.

   procedure Atomchar (C : Character);
   --  The next character of the atom started by StartAtom.

   function Lookup return Atom;
   function Lookup (S : String) return Atom;
   --  Enter the atom in the table and return its value.
   --  If atom is present, the old atom is returned, else
   --  a new entry is made. A series of calls to ATOMCHAR
   --  (following STARTATOM) followed by LOOKUP should be
   --  made. If this is not done, an error state results.

   function Lookup (Info : Atom_Info) return Atom;
   function Lookup (S : String; Info : Atom_Info) return Atom;
   --  Same as above except the INFO is also set for the atom.

   Acc_Count : Integer;
   --  Number of accesses to the table.

   Collision_Count : Integer;
   --  Number of collisions.

   Atom_Count : Integer;
   --  Number of atoms.

   Atom_Chars : Integer;
   --  Total number of characters used up by all the atoms.

   --  All the four integers above are changed by LOOKUP.

   function Get_Atomno (A : Atom) return Integer;
   --  Return the atom number.

   function Writeatom (A : Atom) return String;
   --  Return the atom string.

   procedure Set_Info (A : in out Atom; Info : Atom_Info);
   function Get_Info (A : Atom) return Atom_Info;
   --  Set and get atom information.

   function First_Atom return Atom;
   function Atom_Iterator return Atom;
   --  First_atom returns the first atom and successive calls
   --  to atom_iterator return the next atom.

private

   subtype Atom_Range is Integer range 0 .. Atom_Table_Size;

   type Atomentry;
   type Atom is access Atomentry;
   type Atomentry is record
      Index       : Atom_Range;
      Length      : Integer;
      Information : Atom_Info;
      Number      : Integer;
      Chain       : Atom := null;
   end record;

   Null_Atom : constant Atom := null;

end Prolog.Atom_Table;
