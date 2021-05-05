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

package body Prolog.Atom_Table is

   --   Collisions are handled by chaining together atom entries.
   --   Variable names are stored in another part of the same
   --   character array as atoms.

   Atombuf : String (1 .. Atom_Table_Size);
   Atomhwm : Atom_Range := 0;
   Atomindex : Atom_Range := 0;
   Atomptr : Atom;

   Newatom : Atom := new Atomentry;

   Hashtable : array (1 .. Hash_Table_Size) of Atom;

   ------------------
   --  Start_Atom  --
   ------------------

   procedure Startatom is
      --  Prepare to accept characters of an atom.
   begin
      Newatom := new Atomentry;
      Newatom.Index := Atomhwm;
      Newatom.Length := 0;
   end Startatom;

   -----------------
   --  Atom_Char  --
   -----------------

   procedure Atomchar (C : Character) is
      --  Store c as the next char of an atom.
   begin
      if Newatom.Index + Newatom.Length >= Atom_Table_Size then
         null; --  MOAN (atomspaceE, dieZ);
      end if;

      Newatom.Length := Newatom.Length + 1;
      Atombuf (Newatom.Index + Newatom.Length) := C;
   end Atomchar;

   -----------------
   --  Same_Atom  --
   -----------------

   function Sameatom (A1, A2 : Atom) return  Boolean is
      --  Test whether a1 and a2 are the same atom.
      J    : Integer;
      Same : Boolean;
   begin
      if A1.Length /= A2.Length then
         return False;
      else
         J := 0; Same := True;
         while (J /= A1.Length) and Same loop
            J := J + 1;
            Same := Atombuf (A1.Index + J) = Atombuf (A2.Index + J);
         end loop;
         return Same;
      end if;
   end Sameatom;

   --------------
   --  Lookup  --
   --------------

   function Lookup return Atom is
      --  Enter an atom and return its value.
      H     : Integer range 1 .. Hash_Table_Size;
      A     : Atom;
      Found : Boolean;
   begin
      --  Compute hash function:
      --  care needed to avoid overflow on some machines.
      --  the hash function is :
      --      if # of chars = 1 then 1
      --      else (8*1st char + last char + length) mod HASHSIZE + 1.
      if Newatom.Length >= 1 then
         H := (8 * Character'Pos (Atombuf (Newatom.Index + 1)) +
                 Character'Pos (Atombuf (Newatom.Index + Newatom.Length)) +
                 Newatom.Length) mod  Hash_Table_Size + 1;
      else
         H := 1;
      end if;

      A := Hashtable (H);
      Acc_Count := Acc_Count + 1;

      Found := False;
      while (A /= null) and not Found loop
         if Sameatom (A, Newatom) then
            Found := True;
         else
            A := A.all.Chain;
            Collision_Count := Collision_Count + 1;
         end if;

      end loop;

      if not Found then
         Atom_Count := Atom_Count + 1;
         A := Newatom;
         A.Number := Atom_Count;
         A.Information := Default_Info;
         A.Chain := Hashtable (H);
         Atomhwm := Atomhwm + Newatom.Length;
         Hashtable (H) := A;
      end if;

      return A;
   end Lookup;

   --------------
   --  Lookup  --
   --------------

   function Lookup (S : String) return Atom is
   begin
      Startatom;
      for I in S'Range loop
         Atomchar (S (I));
      end loop;
      return Lookup;
   end Lookup;

   --------------
   --  Lookup  --
   --------------

   function Lookup (Info : Atom_Info) return Atom is
      A : Atom;
   begin
      A := Lookup;
      A.Information := Info;
      return A;
   end Lookup;

   --------------
   --  Lookup  --
   --------------

   function Lookup (S : String; Info : Atom_Info) return Atom is
      A : Atom;
   begin
      Startatom;
      for I in S'Range loop
         Atomchar (S (I));
      end loop;

      A := Lookup;
      A.Information := Info;
      return A;
   end Lookup;

   ------------------
   --  Write_Atom  --
   ------------------

   function Write_Atom (A : Atom) return String is
      --  Write out an atom. Naive about quoting.
   begin
      return Atombuf (A.Index + 1 .. A.Index + A.Length);
   end Write_Atom;

   ----------------
   --  Set_Info  --
   ----------------

   procedure Set_Info (A : in out Atom; Info : Atom_Info) is
   begin
      A.Information := Info;
   end Set_Info;

   ----------------
   --  Get_Info  --
   ----------------

   function Get_Info (A : Atom) return Atom_Info is
   begin
      return A.Information;
   end Get_Info;

   -------------------
   --  Get_Atom_No  --
   -------------------

   function Get_Atom_No (A : Atom) return Integer is
   begin
      return A.Number;
   end Get_Atom_No;

   ------------------
   --  First_Atom  --
   ------------------

   function First_Atom return Atom is
   begin
      Atomindex := 1;
      Atomptr := Hashtable (Atomindex);
      return Atom_Iterator;
   end First_Atom;

   ---------------------
   --  Atom_Iterator  --
   ---------------------

   function Atom_Iterator return Atom is
      Temp : Atom;
   begin
      while (Atomindex <= Hash_Table_Size) and
        (Atomptr = null) loop
         Atomindex := Atomindex + 1;
         Atomptr   := Hashtable (Atomindex);
      end loop;
      Temp := Atomptr;
      if Atomptr /= null then
         Atomptr := Atomptr.Chain;
      end if;
      return Temp;
   end Atom_Iterator;


end Prolog.Atom_Table;
