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

with Prolog.Error_Pkg; use Prolog.Error_Pkg;
with Prolog.Errors;    use Prolog.Errors;

package body Prolog.Var_Table is

   --  INPUT_OUTPUT;

   Varbuf : String (1 .. Var_Table_Size);
   Varhwm : Integer range 0 .. Var_Table_Size;

   Newvar : Varstring;

   Vartable : array  (1 .. MaxVars) of Varstring;

   procedure Start_Var is
      --  Prepare to accept characters of a variable.
   begin
      Newvar.Index := Varhwm;
      Newvar.Length := 0;
   end Start_Var;

   procedure Varchar (C : Character) is
      --  Store c as the next character of a variable.
   begin
      if Newvar.Index + Newvar.Length >= Var_Table_Size then
         Moan (Var_Space_Error, Abortz);
      end if;

      Newvar.Length := Newvar.Length + 1;
      Varbuf (Newvar.Index + Newvar.Length) := C;
   end Varchar;

   function Samestring (V1, V2 : Varstring) return Boolean;
   function Samestring (V1, V2 : Varstring) return Boolean is
   begin
      if V1.Length /= V2.Length then
         return False;
      else
         for I in 1 .. V1.Length loop
            if Varbuf (V1.Index + I) /= Varbuf (V2.Index + I) then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end Samestring;

   ----------------
   --  Keep_Var  --
   ----------------

   function Keep_Var return Varstring is
      --  Mark the latest variable name permanent.
   begin
      for N in 1 .. Varcount loop
         if Samestring (Newvar, Vartable (N)) then
            return Vartable (N);
         end if;
      end loop;

      Varhwm := Varhwm + Newvar.Length;
      Varcount := Varcount + 1;
      Vartable (Varcount) := Newvar;
      return Newvar;
   end Keep_Var;

   function To_String (V : Varstring) return String is
   begin
      return Varbuf (V.Index + 1 .. V.Index + V.Length);
   end To_String;

begin
   Varbuf (1) := '_';
   Varhwm := 1;
end Prolog.Var_Table;
