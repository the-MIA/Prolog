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

with Prolog.Database;         use Prolog.Database;
with Prolog.Error_Pkg;        use Prolog.Error_Pkg;
with Prolog.Errors;           use Prolog.Errors;
with Prolog.Execute;          use Prolog.Execute;
with Prolog.Global_Objects;   use Prolog.Global_Objects;
with Prolog.Read_In;          use Prolog.Read_In;
with Prolog.Transformations;  use Prolog.Transformations;
with Prolog.Write_Out;        use Prolog.Write_Out;
with Prolog.Vars;             use Prolog.Vars;

with Prolog.Input_Output;     use Prolog.Input_Output;

with Implementation_Dependent_Routines;
with Anna_Filename_Utilities;
with Sequential_IO;

package body Prolog.Ada_Logic is

   --  use Read_In.Io;
   use Atom_Pkg;
   use Local;

   package Id renames Implementation_Dependent_Routines;
   package Fu renames Anna_Filename_Utilities;

   Max_Name_Length : constant := 80;

   type Speed_Node is record
      Tag    : Nodetag;
      Field  : Field_Type; --  field.
      Scope  : Integer;    --  various kinds of scope on various stacks.
      Name   : String (1 .. Max_Name_Length);
      Len    : Natural;
      Arity  : Integer; --  # of arguments
      Ival   : Integer; --  Integer value
      Offset : Integer; --  OFFSET on the stack.
      Anont  : Boolean; --  Anonymous var?
      Tail   : Natural := 0; --  if /= 0, this is a rule; len of tail
      Nvars  : Natural := 0;
   end record;

   package Term_Rec_Io is new Sequential_IO (Speed_Node);
   use Term_Rec_Io;


   type Clause_List_Record is record
       Value : Clause;
       Next : Clause_List;
   end record;

   type List_List_Record is record
       L : List_Of_Clauses;
       Next : List_List;
   end record;

   List_Count : Integer := 0; --  Present (unique) # of lists.

   Dummy    : Boolean;
   Clause_X : Clause;
   pragma Unreferenced (Clause_X);

   ---------------
   --  Is_Rule  --
   ---------------

   function Is_Rule (C : Clause) return Boolean is
   begin
      if C.The_Body /= null then
         return True;
      else
         return False;
      end if;
   end Is_Rule;

   ----------------------
   --  Attribute_Kind  --
   ----------------------

   function Attribute_Kind (C : Clause) return Attribute_Kind_Type is
   begin
      if C.The_Body /= null then
         raise Attr_Error;
      else
         case C.Head.Tag is
            when Funct => return Const;
            when Vart => return Var;
            when Intt => return Int;
            when Skelt =>
               if C.Head.Anont then
                  return Anon;
               else
                  return Var;
               end if;
         end case;
      end if;
   end Attribute_Kind;

   -------------------------
   --  Integer_Attribute  --
   -------------------------

   function Integer_Attribute (C : Clause) return Integer is
   begin
      if (C.The_Body /= null) or else (C.Head.Tag /= Intt) then
         raise Attr_Error;
      else
         return C.Head.Ival;
      end if;
   end Integer_Attribute;

   ----------------------------
   --  Identifier_Attribute  --
   ----------------------------

   function Identifier_Attribute (C : Clause) return Identifier is
   begin
      if False
        or else C.The_Body /= null
        or else C.Head.Tag  = Intt
        or else (C.Head.Tag = Skelt and then C.Head.Anont)
      then
         raise Attr_Error;

      elsif C.Head.Tag = Funct then
         return new String'(Write_Atom (C.Head.Name));

      elsif C.Head.Tag = Skelt then
         return new String'(To_String (C.Head.St));

      else
         return new String'(To_String (C.Head.Id));
      end if;

   end Identifier_Attribute;

   procedure Copy_Clause (C1 :     Clause;
                          C2 : out Clause;
                          N  :     Integer;
                          F  :     Field_Type);

   procedure Copy_Term (C1 :     Term;
                        C2 : out Term;
                        F  :     Field_Type);

   -----------------
   --  Copy_Term  --
   -----------------

   procedure Copy_Term (C1 : Term; C2 : out Term; F : Field_Type) is
      Value, C3, C4 : Term;
   begin
      if C1 = null then
         C2 := null;
      else
         case C1.Tag is

            when Funct =>
               Func_Garb.Get (Value);
               Value.all := Node'(Funct, null, F, 0, null,
                                  C1.Name, C1.Arity, null);
               if C1.Arity > 0 then
                  Copy_Term (C1.Son, Value.Son, F);
                  C3 := C1.Son.Brother;
                  C4 := Value.Son;
                  for I in 1 .. (C1.Arity - 1) loop
                     Copy_Term (C3, C4.Brother, F);
                     C3 := C3.Brother;
                     C4 := C4.Brother;
                  end loop;
               end if;

            when Intt =>
               Int_Garb.Get (Value);
               Value.all := Node'(Intt, null, F, 0, null, C1.Ival);

            when Vart =>
               Var_Garb.Get (Value);
               Value.all := Node'(Vart, null, F, 0, null, null, C1.Id);

            when Skelt =>
               Skel_Garb.Get (Value);
               Value.all := Node'(Skelt, null, F, 0, null,
                                  C1.Offset, C1.St, C1.Anont);

         end case;

         C2 := Value;
         Value := null;
      end if;
   end Copy_Term;

   ----------------------
   --  Destroy_Clause  --
   ----------------------

   procedure Destroy_Clause (C : in out Clause) is
   begin
      if C.Head /= null then
         case C.Head.Tag is
            when Funct =>  Func_Garb.Free (C.Head);
            when Vart  =>  Var_Garb.Free  (C.Head);
            when Intt  =>  Int_Garb.Free  (C.Head);
            when Skelt =>  Skel_Garb.Free (C.Head);
         end case;
      end if;

      if C.The_Body /= null then
         case C.The_Body.Tag is
            when Funct =>  Func_Garb.Free (C.The_Body);
            when Vart  =>  Var_Garb.Free  (C.The_Body);
            when Intt  =>  Int_Garb.Free  (C.The_Body);
            when Skelt =>  Skel_Garb.Free (C.The_Body);
         end case;
      end if;

      Clause_Garb.Free (Clptr (C));
   end Destroy_Clause;

   --------------
   --  Length  --
   --------------

   function Length (L : List_Of_Clauses) return Natural is
   begin
      return (L.Arity);
   end Length;

   function Length (L : List_Of_Sons) return Natural is
   begin
      return (L.Arity);
   end Length;

   function Length (L : List_Of_Lists) return Natural is
   begin
      return (L.Arity);
   end Length;

   --------------
   --  Create  --
   --------------

   function Create return List_Of_Clauses is
   begin
      List_Count := List_Count + 1;
      return new  List_Of_Clauses_Record'(List_Count, null, 0);
   end Create;

   function Create return List_Of_Lists is
   begin
      return (null, null, 0);
   end Create;

   function Create return List_Of_Sons is
   begin
      return (0, null);
   end Create;

   ----------------
   --  Get_Item  --
   ----------------

   function Get_Item (L : List_Of_Clauses; N : Positive) return Clause is
      Ptr  : Clause_List := L.List;
      I    : Integer     := 1;
      Temp : Clause;
   begin
      if N > L.Arity then
         raise Out_Of_Range;
      else
         while I <= N loop
            if I = N then
               Copy_Clause (Ptr.Value, Temp, -1, Heapf);
               return Temp;
            else
               Ptr := Ptr.Next;
               I := I + 1;
            end if;
         end loop;
      end if;

      pragma Assert (False, "Stop Gnat from complaining");
      return Temp;
   end Get_Item;

   ----------------
   --  Get_Item  --
   ----------------

   function Get_Item (L : List_Of_Sons; N : Positive) return Clause is
      Ptr : Term := L.List;
      I : Integer := 1;
      Temp : Clptr;
      X : Term;
   begin
      if N > L.Arity then
         raise Out_Of_Range;
      else
         while I <= N loop
            if I = N then
               Clause_Garb.Get (Temp);
               if (Ptr.Tag = Skelt) and then not (Ptr.Anont) then
                  Var_Garb.Get (X);
                  X.Brother := null;
                  X.Field := Heapf;
                  X.Scope := 0;
                  X.Chain := null;
                  X.Val := null;
                  X.Id := Ptr.St;
                  Temp.all :=  Cls'(Head => X, The_Body => null,
                                    Typ => Madec, Nvars => 0,
                                    Keyval => 0, Dbase => -1,
                                    Previous => null, Next => null);
               else
                  Temp.all :=  Cls'(Head => Ptr, The_Body => null,
                                    Typ => Madec, Nvars => 0,
                                    Keyval => 0, Dbase => -1,
                                    Previous => null, Next => null);
               end if;
               return Clause (Temp);
            else
               Ptr := Ptr.Brother;
               I := I + 1;
            end if;
         end loop;
      end if;
      pragma Assert (False);
      raise Program_Error;

   end Get_Item;

   ----------------
   --  Get_Item  --
   ----------------

   function Get_Item (L : List_Of_Lists;
                      N : Positive) return List_Of_Clauses
   is
      Ptr : List_List := L.List;
      I   : Integer   := 1;
   begin
      if N > L.Arity then
         raise Out_Of_Range;
      else
         while I <= N loop
            if I = N then
               return Ptr.L;
            else
               Ptr := Ptr.Next;
               I := I + 1;
            end if;
         end loop;
      end if;
      pragma Assert (False);
      raise Program_Error;
   end Get_Item;

   ------------
   --  Copy  --
   ------------

   procedure Copy (L1     :     List_Of_Clauses;
                   L2     : out List_Of_Clauses;
                   Number :     Integer);

   procedure Copy (L1     :     List_Of_Clauses;
                   L2     : out List_Of_Clauses;
                   Number :     Integer)
   is
      Ptr1 : Clause_List := L1.List;
      Ptr2 : Clause_List;
      Cl : Clause;
      Ltemp : List_Of_Clauses;
   begin
      if L1.List = null then
         Ltemp := Create;
      else
         Ltemp := new List_Of_Clauses_Record;
         Copy_Clause (L1.List.Value, Cl, Number, Heapf);
         Add_Clause (Clptr (Cl), Number, False);
         Ptr2 := new Clause_List_Record;
         Ltemp.all.List := Ptr2;
         Ptr2.Value := Cl;
         Ptr1 := Ptr1.Next;
         while Ptr1 /= null loop
            Copy_Clause (Ptr1.Value, Cl, Number, Heapf);
            Add_Clause (Clptr (Cl), Number, False);
            Ptr2.Next  := new Clause_List_Record;
            Ptr2       := Ptr2.Next;
            Ptr2.Value := Cl;
            Ptr1       := Ptr1.Next;
         end loop;
         Ptr2.Next := null;
         Ltemp.all.Arity  := L1.all.Arity;
         Ltemp.all.Number := Number;
      end if;
      L2 := Ltemp;
   end Copy;

   ------------
   --  Copy  --
   ------------

   procedure Copy (L1 :     List_Of_Clauses;
                   L2 : out List_Of_Clauses)
   is
      Value : List_Of_Clauses;
   begin
      Value := Create;
      Copy (L1, Value, Value.Number);
      L2 := Value;
   end Copy;

   ------------
   --  Copy  --
   ------------

   procedure Copy (L1 :     List_Of_Sons;
                   L2 : out List_Of_Sons) is
      Ptr1 : Term := L1.List;
      Ptr2 : Term;
      Value : List_Of_Sons;
   begin
      if L1.List = null then
         L2 := Create;
      else
         Value := Create;
         Copy_Term (L1.List, Value.List, Heapf);
         Ptr2 := Value.List;
         Ptr1 := Ptr1.Brother;
         while Ptr1 /= null loop
            Copy_Term (Ptr1, Ptr2.Brother, Heapf);
            Ptr1 := Ptr1.Brother;
            Ptr2 := Ptr2.Brother;
         end loop;
         Ptr2.Brother := null;
         Value.Arity := L1.Arity;
         L2 := Value;
         Value := (0, null);
      end if;
   end Copy;

   ------------
   --  Copy  --
   ------------

   procedure Copy (L1 :     List_Of_Lists;
                   L2 : out List_Of_Lists) is
   begin
      L2 := L1; --  To be changed.
   end Copy;

   --------------
   --  Append  --
   --------------

   procedure Append (C :        Clause;
                     L : in out List_Of_Clauses)
   is
      Ptr : Clause_List;
   begin
      Ptr := new Clause_List_Record;
      case C.Typ is
         when Commac =>
            Ptr.Value := Clause (Make_Clause (C.Head, 0));
         when Chainc =>
            Ptr.Value := Clause (Make_Clause (C.Head, C.The_Body, 0));
         when Madec =>
            Ptr.Value := C;
      end case;
      Add_Clause (Clptr (Ptr.Value), L.Number, True);
      Ptr.Next := L.List;
      L.List   := Ptr;
      L.Arity := L.Arity + 1;
   end Append;

   --------------
   --  Append  --
   --------------

   procedure Append (C :        List_Of_Clauses;
                     L : in out List_Of_Lists)
   is
      Ptr1 : Integer_List;
      Ptr2 : List_List;
   begin
      Ptr1 := new Integer_Record'(C.Number, L.Int_List);
      Ptr2 := new List_List_Record'(C, L.List);
      L.Int_List := Ptr1;
      L.List     := Ptr2;
      L.Arity    := L.Arity + 1;
   end Append;

   --------------
   --  Append  --
   --------------

   procedure Append (C :        Clause;
                     L : in out List_Of_Sons) is
   begin
      C.Dbase := -1;
      C.Head.Brother := L.List;
      L.List := C.Head;
      L.Arity := L.Arity + 1;
   end Append;

   --------------
   --  Append  --
   --------------

   procedure Append (L : in out List_Of_Clauses; C : Clause) is
      Ptr : Clause_List;
   begin
      if L.List = null then
         Append (C, L);
      else
         Ptr := L.List;
         while Ptr.Next /= null loop
            Ptr := Ptr.Next;
         end loop;
         case C.Typ is

            when Commac =>
               Ptr.Next :=
                 new Clause_List_Record'(Clause (Make_Clause (C.Head, 0)),
                                         null);
            when Chainc =>
               Ptr.Next :=
                 new Clause_List_Record'(Clause (Make_Clause (C.Head,
                                                             C.The_Body, 0)),
                                         null);
            when Madec =>
               Ptr.Next := new Clause_List_Record'(C, null);
         end case;
         Add_Clause (Clptr (Ptr.Next.Value), L.Number, False);
         L.Arity := L.Arity + 1;
      end if;
   end Append;

   --------------
   --  Append  --
   --------------

   procedure Append (L : in out List_Of_Sons; C : Clause) is
      Ptr, T : Term;
   begin
      C.Dbase := -1;
      if L.List = null then
         Copy_Term (C.Head, T, C.Head.Field);
         L.List := T;
         L.Arity := 1;
      else
         Ptr := L.List;
         while Ptr.Brother /= null loop
            Ptr := Ptr.Brother;
         end loop;
         Copy_Term (C.Head, T, C.Head.Field);
         Ptr.Brother := T;
         T.Brother := null;
         L.Arity := L.Arity + 1;
      end if;
   end Append;

   --------------
   --  Append  --
   --------------

   procedure Append (L : in out List_Of_Lists; C : List_Of_Clauses) is
      Ptr1 : Integer_List;
      Ptr2 : List_List;
   begin
      if L.List = null then
         L.List     := new List_List_Record'(C, null);
         L.Int_List := new Integer_Record'(C.Number, null);
         L.Arity := 1;
      else
         Ptr1 := L.Int_List;
         Ptr2 := L.List;
         while Ptr1.Next /= null loop
            Ptr1 := Ptr1.Next;
            Ptr2 := Ptr2.Next;
         end loop;
         Ptr2.Next := new List_List_Record'(C, null);
         Ptr1.Next := new Integer_Record'(C.Number, null);
         L.Arity := L.Arity + 1;
      end if;
   end Append;

   --------------
   --  Append  --
   --------------

   procedure Append (L1 : in out List_Of_Clauses;
                     L2 :        List_Of_Clauses)
   is
      Ptr : Clause_List;
      Ptr2 : List_Of_Clauses;
   begin
      if L1.Number = 0 then
         L1 := Create;
      end if;
      if L1.List = null then
         Copy (L2, L1, L1.Number);
      else
         Ptr := L1.List;
         while Ptr.Next /= null loop
            Ptr := Ptr.Next;
         end loop;
         Copy (L2, Ptr2, L1.Number);
         Ptr.Next := Ptr2.List;
         L1.Arity := L1.Arity + L2.Arity;
      end if;
   end Append;

   --------------
   --  Append  --
   --------------

   procedure Append (L1 : in out List_Of_Sons;
                     L2 :        List_Of_Sons)
   is
      Ptr : Term;
   begin
      if L1.List = null then
         L1 := L2;
      else
         Ptr := L1.List;
         while Ptr.Brother /= null loop
            Ptr := Ptr.Brother;
         end loop;
         Ptr.Brother := L2.List;
         L1.Arity := L1.Arity + L2.Arity;
      end if;
   end Append;

   --------------
   --  Append  --
   --------------

   procedure Append (L1 : in out List_Of_Lists;
                     L2 :        List_Of_Lists)
   is
      Ptr1 : Integer_List;
      Ptr2 : List_List;
   begin
      if L1.List = null then
         L1 := L2;
      else
         Ptr1 := L1.Int_List;
         Ptr2 := L1.List;
         while Ptr1.Next /= null loop
            Ptr1 := Ptr1.Next;
            Ptr2 := Ptr2.Next;
         end loop;
         Ptr2.Next := L2.List;
         Ptr1.Next := L2.Int_List;
         L1.Arity  := L1.Arity + L2.Arity;
      end if;
   end Append;

   -------------------
   --  Delete_Item  --
   -------------------

   procedure Delete_Item (L : in out List_Of_Clauses; N : Positive) is
      Ptr : Clause_List;
      I   : Integer := 2;
   begin
      if L.List = null then
         raise Out_Of_Range;
      elsif L.List.Next = null then
         if N = 1 then
            Zap_Clause (Clptr (L.List.Value));
            L.List := null;
            L.Arity := 0;
         else
            raise Out_Of_Range;
         end if;
      elsif N = 1 then
         Zap_Clause (Clptr (L.List.Value));
         L.List  := L.List.Next;
         L.Arity := L.Arity - 1;
      else
         Ptr := L.List;
         while (Ptr.Next.Next /= null) and (I /= N) loop
            I := I + 1;
            Ptr := Ptr.Next;
         end loop;
         if I = N then
            Zap_Clause (Clptr (Ptr.Next.Value));
            Ptr.Next := Ptr.Next.Next;
            L.Arity  := L.Arity - 1;
         else
            raise Out_Of_Range;
         end if;
      end if;
   end Delete_Item;

   -------------------
   --  Delete_Item  --
   -------------------

   procedure Delete_Item (L : in out List_Of_Sons;
                          N :        Positive)
   is
      Ptr : Term;
      I : Integer := 2;
   begin
      if L.List = null then
         raise Out_Of_Range;
      elsif L.List.Brother = null then
         if N = 1 then
            L.List  := null;
            L.Arity := 0;
         else
            raise Out_Of_Range;
         end if;
      elsif N = 1 then
         L.List  := L.List.Brother;
         L.Arity := L.Arity - 1;
      else
         Ptr := L.List;
         while (Ptr.Brother.Brother /= null) and (I /= N) loop
            I   := I + 1;
            Ptr := Ptr.Brother;
         end loop;
         if I = N then
            Ptr.Brother := Ptr.Brother.Brother;
            L.Arity := L.Arity - 1;
         else
            raise Out_Of_Range;
         end if;
      end if;
   end Delete_Item;

   -------------------
   --  Delete_Item  --
   -------------------

   procedure Delete_Item (L : in out List_Of_Lists; N : Positive) is
      Ptr1 : Integer_List;
      Ptr2 : List_List;
      I : Integer := 2;
   begin
      if L.List = null then
         raise Out_Of_Range;
      elsif L.List.Next = null then
         if N = 1 then
            L.List := null;
            L.Int_List := null;
            L.Arity := 0;
         else
            raise Out_Of_Range;
         end if;
      elsif N = 1 then
         L.List := L.List.Next;
         L.Int_List := L.Int_List.Next;
         L.Arity := L.Arity - 1;
      else
         Ptr1 := L.Int_List;
         Ptr2 := L.List;
         while (Ptr1.Next.Next /= null) and (I /= N) loop
            I := I + 1;
            Ptr1 := Ptr1.Next;
            Ptr2 := Ptr2.Next;
         end loop;
         if I = N then
            Ptr1.Next := Ptr1.Next.Next;
            Ptr2.Next := Ptr2.Next.Next;
            L.Arity := L.Arity - 1;
         else
            raise Out_Of_Range;
         end if;
      end if;
   end Delete_Item;

   ---------------
   --  Destroy  --
   ---------------

   procedure Destroy (L : in out List_Of_Clauses) is
   begin
      while L.List /= null loop
         Zap_Clause (Clptr (L.List.Value));
         L.List := L.List.Next;
      end loop;
      L.Arity := 0;
   end Destroy;

   ---------------
   --  Destroy  --
   ---------------

   procedure Destroy (L : in out List_Of_Sons) is
   begin
      L := (0, null);
      --  while L.LIST /= null loop
      --  PTR := L.LIST;
      --  L.LIST := L.LIST.BROTHER;
      --  DISPOSE(PTR);
      --  end loop;
   end Destroy;

   ---------------
   --  Destroy  --
   ---------------

   procedure Destroy (L : in out List_Of_Lists) is
   begin
      L := (null, null, 0);
   end Destroy;

   -------------
   --  Arity  --
   -------------

   function Arity (C : Clause) return Natural is
   begin
      if C.The_Body /= null then
         raise Attr_Error;
      elsif C.Head.Tag = Funct then
         return (C.Head.Arity);
      else
         return 0;
      end if;
   end Arity;

   --------------------
   --  Get_Son_List  --
   --------------------

   function Get_Son_List (C : Clause) return List_Of_Sons is
   begin
      if C.The_Body /= null then
         raise Attr_Error;
      elsif C.Head.Tag = Funct then
         return (C.Head.Arity, C.Head.Son);
      else
         raise No_Sons;
      end if;
   end Get_Son_List;

   ----------------
   --  Get_Head  --
   ----------------

   function Get_Head (C : Clause) return Clause is
      Temp : Clptr;
   begin
      Clause_Garb.Get (Temp);
      Temp.all :=  (Cls'(Head     => C.Head,
                         The_Body => null,
                         Typ      => C.Typ,
                         Nvars    => 0, Keyval => 0,
                         Dbase    => -1, Previous => null,
                         Next     => null));
      return Clause (Temp);
   end Get_Head;

   ----------------
   --  Get_Tail  --
   ----------------

   function Get_Tail (C : Clause) return List_Of_Sons is
      Ptr : Term;
      Count : Integer := 0;
   begin
      if C.The_Body /= null then
         Ptr := C.The_Body;
         while Ptr /= null loop
            Count := Count + 1;
            Ptr := Ptr.Brother;
         end loop;
         return (Count, C.The_Body);
      else
         raise Attr_Error;
      end if;
   end Get_Tail;

   ------------------
   --  Build_Fact  --
   ------------------

   function Build_Fact (Attr : Integer) return Clause is
      Temp : Clptr;
      T    : Term;
   begin
      Clause_Garb.Get (Temp);
      Int_Garb.Get (T);
      T.all := Node'(Intt, null, Heapf, 0, null, Attr);
      Temp.all :=  (Cls'(Head => T, The_Body => null, Typ => Madec,
                         Nvars => 0, Keyval => 0,
                         Dbase => -1, Previous => null,
                         Next => null));
      return Clause (Temp);
   end Build_Fact;

   ------------------
   --  Build_Fact  --
   ------------------

   function Build_Fact (Attr : Identifier) return Clause is
      Temp : Clptr; T : Term;
   begin
      if Attr (Attr'First) in 'A' .. 'Z' then
         Start_Var;
         for I in Attr'Range loop
            Var_Char (Attr (I));
         end loop;
         Clause_Garb.Get (Temp);
         Var_Garb.Get (T);
         T.all := Node'(Vart, null, Heapf, 0, null, null, Keep_Var);
         Temp.all :=  (Cls'(Head => T,
                            The_Body => null, Typ => Chainc,
                            Nvars => 0, Keyval => 0,
                            Dbase => -1, Previous => null,
                            Next => null));
         return Clause (Temp);
      else
         Startatom;
         for I in Attr'Range loop
            Atomchar (Attr (I));
         end loop;
         Clause_Garb.Get (Temp);
         Func_Garb.Get (T);
         T.all := Node'(Funct, null, Heapf, 0, null, Lookup, 0, null);
         Temp.all := (Cls'(Head => T,
                           The_Body => null, Typ => Chainc,
                           Nvars => 0, Keyval => 0,
                           Dbase => -1, Previous => null,
                           Next => null));
         return Clause (Temp);
      end if;
   end Build_Fact;

   ------------------
   --  Build_Anon  --
   ------------------

   function Build_Anon return Clause is
      Temp : Clptr; T : Term;
   begin
      Clause_Garb.Get (Temp);
      Skel_Garb.Get (T);
      T.all := Node'(Skelt, null, Heapf, 0, null, 0, Anon_String, True);
      Temp.all :=  (Cls'(Head => T,
                         The_Body => null, Typ => Madec,
                         Nvars => 0, Keyval => 0,
                         Dbase => -1, Previous => null,
                         Next => null));
      return Clause (Temp);
   end Build_Anon;

   --------------------
   --  Set_Son_List  --
   --------------------

   procedure Set_Son_List (C : in out Clause; Sons : List_Of_Sons) is
   begin
      if C.The_Body /= null then
         raise Attr_Error;
      elsif C.Head.Tag /= Funct then
         raise No_Sons;
      else
         C.Head.Son := Sons.List;
         C.Head.Arity := Sons.Arity;
      end if;
   end Set_Son_List;

   ------------------
   --  Build_Rule  --
   ------------------

   function Build_Rule (Head_Of_Clause : Clause;
                        Tail : List_Of_Sons) return Clause is
      Temp : Clptr;
   begin
      Clause_Garb.Get (Temp);
      Temp.all := (Cls'(Head => Head_Of_Clause.all.Head,
                        The_Body => Tail.List, Typ => Chainc,
                        Nvars => 0, Keyval => 0,
                        Dbase => -1, Previous => null,
                        Next => null));
      return Clause (Temp);
   end Build_Rule;

   -------------------
   --  Copy_Clause  --
   -------------------

   procedure Copy_Clause (C1 :     Clause;
                          C2 : out Clause;
                          N  :     Integer;
                          F  :     Field_Type)
   is
      Ptr1, Ptr2, Ptr3, Ptr4 : Term;
      Temp : Clptr;
   begin
      Copy_Term (C1.Head, Ptr1, F);
      Copy_Term (C1.The_Body, Ptr2, F);
      Ptr4 := Ptr2;
      if C1.The_Body /= null then
         Ptr3 := C1.The_Body.Brother;
         while Ptr3 /= null loop
            Copy_Term (Ptr3, Ptr4.Brother, F);
            Ptr3 := Ptr3.Brother;
            Ptr4 := Ptr4.Brother;
         end loop;
         Ptr4.Brother := null;
      end if;
      Clause_Garb.Get (Temp);
      Temp.all :=  Cls'(Ptr1, Ptr2,
                        Nvars => C1.Nvars, Keyval => C1.Keyval, Typ => C1.Typ,
                        Dbase => N, Previous => null,
                        Next => null);
      C2 := Clause (Temp);
      Temp := null;
   end Copy_Clause;

   -------------------
   --  Copy_Clause  --
   -------------------

   procedure Copy_Clause (C1 : Clause; C2 : out Clause) is
   begin
      Copy_Clause (C1, C2, C1.Dbase, Heapf);
   end Copy_Clause;

   -------------------
   --  Read_Clause  --
   -------------------

   function Read_Clause (Text : String) return Clause is
      T  : Term;
      T1 : Term;
   begin
      Set_String (Text);
      T1 := Glotop;
      T := Read_In.Read_In;

      while Glotop /= T1 loop
         Glotop := Glotop.Chain;
         Glosize := Glosize - 1;
      end loop;

      if (T.Tag = Funct) and then (T.Name = Arrowa) then
         return (Clause (Make_Clause (T, 0)));
      else
         return new Cls'(Head => T, The_Body => null,
                         Nvars => 0, Keyval => 0, Typ => Chainc,
                         Dbase => -1, Previous => null, Next => null);
      end if;

   end Read_Clause;

   -------------------
   --  Read_Clause  --
   -------------------

   function Read_Clause (Text : Identifier) return Clause is
   begin
      return Read_Clause (Text.all);
   end Read_Clause;

   -------------------
   --  Dump_Clause  --
   -------------------

   function Dump_Clause (Tree : Clause) return Identifier is
      Idd : Identifier;
      Ptr : Term;
      function Dump_Term (T : Term) return Identifier;
      function Dump_Term (T : Term) return Identifier is
      begin
         Init_String;
         Writeout (T, 0);
         Wr_Ln;
         return new String'(Get_String);
      end Dump_Term;
   begin
      if Tree.The_Body = null then
         return new String'(Dump_Term (Tree.Head).all);
      else
         Idd := new String'(Dump_Term (Tree.Head).all & " :- " &
                              Dump_Term (Tree.The_Body).all);
         Ptr := Tree.The_Body.Brother;
         while Ptr /= null loop
            Idd := new String'(Idd.all & ", " & Dump_Term (Ptr).all);
            Ptr := Ptr.Brother;
         end loop;
         return Idd;
      end if;
   end Dump_Clause;

   -----------------
   --  Read_File  --
   -----------------

   function Read_File (File : String) return List_Of_Clauses is
      Value : List_Of_Clauses;
      C     : Term;
      Dummy : Boolean;
   begin
      Dummy := See_File (Fu.Readable_Or_Moan (File));
      Value := Create;

      while not (File_Ended) loop
         C := Read_In.Read_In;
         exit when C.all.Tag = Funct and then
           Get_Info (C.all.Name).Pclass = Evalp and then
           Get_Info (C.all.Name).Routine = Endr;

         Append (Value, new Cls'(Head     => C,
                                 The_Body => null,
                                 Typ      => Commac,
                                 Nvars    => 0,
                                 Keyval   => 0,
                                 Dbase    => -1,
                                 Previous => null,
                                 Next     => null));
      end loop;

      Dummy := Seen_File;
      return Value;

   end Read_File;

   ----------------------
   --  Read_Fast_File  --
   ----------------------

   function Read_Fast_File (File : String) return List_Of_Clauses is
      F : Term_Rec_Io.File_Type;
      L : List_Of_Clauses;

      procedure Get_Term (T : out Term; Tail, Nvars : out Natural);
      procedure Get_Term (T : out Term; Tail, Nvars : out Natural) is
         S : Speed_Node;
         T1 : Term;
      begin
         Read (F, S);
         Tail := S.Tail;
         Nvars := S.Nvars;

         case S.Tag is

            when Funct =>
               declare
                  Name : Atom;
                  Son, Prev : Term;
                  Dummy1, Dummy2 : Natural;
               begin
                  Startatom;
                  for I in 1 .. S.Len loop
                     Atomchar (S.Name (I));
                  end loop;
                  Name := Lookup;
                  if S.Arity > 0 then
                     Get_Term (Son, Dummy1, Dummy2);
                     Prev := Son;
                     for I in 2 .. S.Arity loop
                        Get_Term (Prev.Brother, Dummy1, Dummy2);
                        Prev := Prev.Brother;
                     end loop;
                  else
                     Son := null;
                  end if;
                  T1 := new Node'(Funct, null, S.Field, S.Scope, null,
                                  Name, S.Arity, Son);
               end;
            when Intt =>
               T1 := new Node'(Intt, null, S.Field, S.Scope, null, S.Ival);
            when Vart =>
               declare
                  Id : Varstring;
                  Val : Term;
                  Dummy1, Dummy2 : Natural;
               begin
                  Start_Var;
                  for I in 1 .. S.Len loop
                     Var_Char (S.Name (I));
                  end loop;

                  Id := Keep_Var;
                  Get_Term (Val, Dummy1, Dummy2);

                  T1 := new Node'(Vart, null, S.Field, S.Scope, null,
                                  Val, Id);
               end;

            when Skelt =>
               Start_Var;
               for I in 1 .. S.Len loop
                  Var_Char (S.Name (I));
               end loop;
               T1 := new Node'(Skelt, null, S.Field, S.Scope, null,
                               S.Offset, Keep_Var, S.Anont);
         end case;
         T := T1;
      end Get_Term;

      function Get_Rule return Clause;
      function Get_Rule return Clause is
         C : Clause;
         T : Term;
         Tail, Dummy1, Dummy2 : Natural;

      begin
         C := new Cls;
         Get_Term (C.Head, Tail, C.Nvars);
         if Tail /= 0 then
            Get_Term (C.The_Body, Dummy1, Dummy2);
            T := C.The_Body;
            for J in 2 .. Tail loop
               Get_Term (T.Brother, Dummy1, Dummy2);
               T := T.Brother;
            end loop;
         else
            C.The_Body := null;
         end if;
         C.Previous := null;
         C.Next := null;
         return C;
      end Get_Rule;

   begin
      Term_Rec_Io.Open (F, In_File, Fu.Readable_Or_Moan (File));
      L := Create;
      while not Term_Rec_Io.End_Of_File (F) loop
         Append (L, Get_Rule);
      end loop;
      Term_Rec_Io.Close (F);
      return L;
   end Read_Fast_File;

   ------------------
   --  Write_File  --
   ------------------

   procedure Write_File (File : String; List : List_Of_Clauses) is
      Ptr : Clause_List;
      Dummy : Boolean;

      procedure Write_Tail (T : Term);
      procedure Write_Tail (T : Term) is
      begin
         if T /= null then
            Writeout (T, 0);
            if T.Brother /= null then
               Wr_String (", ");
               Write_Tail (T.Brother);
            end if;
         end if;
      end Write_Tail;

   begin
      Dummy := Tell_File (Fu.Createable_Or_Moan (File));
      Ptr := List.List;
      for I in 1 .. List.Arity loop
         Writeout (Ptr.Value.Head, 0);
         if Ptr.Value.The_Body /= null then
            Wr_String (" :- ");
            --  Walt: I'm trying to get the tail successfully printed.
            --                WRITEOUT(PTR.VALUE.THE_BODY,0);
            Write_Tail (Ptr.Value.The_Body);
         end if;
         Wr ('.');
         Wr_Ln;
         Ptr := Ptr.Next;
      end loop;
      Dummy := Told_File;
   end Write_File;

   -----------------------
   --  Write_Fast_File  --
   -----------------------

   procedure Write_Fast_File (File : String; List : List_Of_Clauses) is
      F : Term_Rec_Io.File_Type;
--      S : Speed_Node;
      L : Clause_List;
      T : Term;

      function Term_To_Speed_Node (T : Term;
                                   Tail, Nvars : Natural) return Speed_Node;

      function Term_To_Speed_Node (T : Term;
                                   Tail, Nvars : Natural) return Speed_Node
      is
         S : Speed_Node;

         procedure Set_Name (Sn : in out Speed_Node; St : String);
         procedure Set_Name (Sn : in out Speed_Node; St : String) is
         begin
            Sn.Len := St'Length;
            Sn.Name (1 .. St'Length) := St;
         end Set_Name;

      begin
         S.Tag := T.Tag;
         S.Field := T.Field;
         S.Scope := T.Scope;
         S.Tail := Tail;
         S.Nvars := Nvars;
         case T.Tag is

            when Funct =>
               Set_Name (S, Write_Atom (T.Name));
               S.Arity := T.Arity;

            when Intt =>
               S.Ival := T.Ival;

            when Vart =>
               Set_Name (S, To_String (T.Id));

            when Skelt =>
               S.Offset := T.Offset;
               Set_Name (S, To_String (T.St));
               S.Anont := T.Anont;
         end case;
         return S;
      end Term_To_Speed_Node;

      procedure Write_Term (T : Term;
                            Tail, Nvars : Natural := 0);
      procedure Write_Term (T : Term;
                            Tail, Nvars : Natural := 0) is
         T1 : Term;
      begin
         Write (F, Term_To_Speed_Node (T, Tail, Nvars));
         case T.Tag is
            when Funct =>
               T1 := T.Son;
               for I in 1 .. T.Arity loop
                  Write_Term (T1);
                  T1 := T1.Brother;
               end loop;
            when others => null;
         end case;
      end Write_Term;

   begin
      Term_Rec_Io.Create (F, Out_File, Fu.Createable_Or_Moan (File));
      L := List.List;
      for I in 1 .. List.Arity loop
         if L.Value.The_Body /= null then
            Write_Term (L.Value.Head, Get_Tail (L.Value).Arity, L.Value.Nvars);
         else
            Write_Term (L.Value.Head, 0, L.Value.Nvars);
         end if;
         if L.Value.The_Body /= null then
            T := Get_Tail (L.Value).List;
            for J in 1 .. Get_Tail (L.Value).Arity loop
               Write_Term (T);
               T := T.Brother;
            end loop;
         end if;
         L := L.Next;
      end loop;
      Term_Rec_Io.Close (F);
   end Write_Fast_File;

   --------------
   --  Length  --
   --------------

   function Length (Ans : Answer) return Integer is
      Ptr : Answer := Ans;
      I : Integer := 0;
   begin
      while Ptr /= null loop
         I := I + 1;
         Ptr := Answer (Ptr.Next);
      end loop;
      return I;
   end Length;

   -------------------
   --  Get_Binding  --
   -------------------

   function Get_Binding (Ans : Answer; Var : String) return Clause is
      Ptr : Answer := Ans;
   begin
      while Ptr /= null loop
         if Var = Ptr.Value.Id.all then
            return (new Cls'(Head     => Ptr.Value.C,
                             The_Body => null,
                             Typ      => Chainc,
                             Nvars    => 0,
                             Keyval   => 0,
                             Dbase    => -1,
                             Previous => null,
                             Next     => null));
         else
            Ptr := Answer (Ptr.Next);
         end if;
      end loop;

      raise Not_Bound;
   end Get_Binding;

   -------------------
   --  Get_Binding  --
   -------------------

   function Get_Binding (Ans : Answer; Var : Identifier) return Clause is
   begin
      return Get_Binding (Ans, Var.all);
   end Get_Binding;

   --------------
   --  Length  --
   --------------

   function Length (L : List_Of_Answers) return Integer is
      Ptr : List_Of_Answers := L;
      I : Integer := 0;
   begin
      while Ptr /= null loop
         I := I + 1;
         Ptr := Ptr.Next;
      end loop;
      return I;
   end Length;

   ------------------
   --  Get_Answer  --
   ------------------

   function Get_Answer (L : List_Of_Answers; N : Integer)
                       return Answer
   is
      Ptr : List_Of_Answers := L;
      I : Integer := 1;
   begin
      while Ptr /= null loop
         if I = N then
            return Ptr.Value;
         else
            I := I + 1;
            Ptr := Ptr.Next;
         end if;
      end loop;
      raise Out_Of_Range;
   end Get_Answer;

   -------------
   --  Query  --
   -------------

   procedure Query (Question  : Clause;
                    Based_On  : List_Of_Clauses;
                    Success   : out Boolean;
                    Solutions : out List_Of_Answers)
   is
      Succ : Boolean;
      L : List_Of_Answers;
      A : Solution;
      Q : Term;
   begin
      Copy_Term (Question.Head, Q, Globalf);
      First_Answer (Q, 0, new Integer_Record'(Based_On.Number, null), A, Succ);
      Success := Succ;
      if Succ then
         L := new Answer_List_Record'(Answer (A), null);
         Solutions := L;
         while Succ loop
            Next_Answer (A, Succ);
            if Succ then
               L.Next := new Answer_List_Record'(Answer (A), null);
               L := L.Next;
            end if;
         end loop;
      else
         Solutions := null;
      end if;
   end Query;

   -------------
   --  Query  --
   -------------

   procedure Query (Question  :     Clause;
                    Based_On  :     List_Of_Lists;
                    Success   : out Boolean;
                    Solutions : out List_Of_Answers)
   is
      Succ : Boolean;
      L : List_Of_Answers;
      A : Solution;
      Q : Term;
   begin
      Copy_Term (Question.Head, Q, Globalf);
      First_Answer (Q, 0, Based_On.Int_List, A, Succ);
      Success := Succ;
      if Succ then
         L := new Answer_List_Record'(Answer (A), null);
         Solutions := L;
         while Succ loop
            Next_Answer (A, Succ);
            if Succ then
               L.Next := new Answer_List_Record'(Answer (A), null);
               L := L.Next;
            end if;
         end loop;
      else
         Solutions := null;
      end if;
   end Query;

   -------------
   --  Query  --
   -------------

   procedure Query (Question :     Clause;
                    Based_On :     List_Of_Clauses;
                    Success  : out Boolean;
                    Bindings : out Answer)
   is      --  One single answer.
      S : Solution;
      Q : Term;
   begin
      Copy_Term (Question.Head, Q, Globalf);
      First_Answer (Q, 0, new Integer_Record'(Based_On.Number, null),
                    S, Success);
      Bindings := Answer (S);
   end Query;

   -------------
   --  Query  --
   -------------

   procedure Query (Question :     Clause;
                    Based_On :     List_Of_Lists;
                    Success  : out Boolean;
                    Bindings : out Answer)
   is  --  One single answer.
      S : Solution;
      Q : Term;
   begin
      Copy_Term (Question.Head, Q, Globalf);
      First_Answer (Q, 0, Based_On.Int_List,
                    S, Success);
      Bindings := Answer (S);
   end Query;

   -------------------
   --  Next_Answer  --
   -------------------

   procedure Next_Answer (Success : out Boolean; Bindings : out Answer) is
      S : Solution;
   begin
      Next_Answer (S, Success);
      Bindings := Answer (S);
   end Next_Answer;

   ----------------------------------------

   --------------------
   --  Init_Package  --
   --------------------

   procedure Init_Package;
   procedure Init_Package is
   begin
      Global_Objects.Mode := Sysm;

      --  The following used to be "SeeSystemFile"
      Dummy := See_File (Fu.Readable_Or_Moan (Id.Ada_Logic_System_File));

      if not Dummy then
         Moan (Init_Error, Diez);
      end if;

      while not File_Ended loop
         Clause_X := Clause (Add_Clause (Read_In.Read_In, 0, 0, False));

      end loop;

      Dummy := Seen_File;

      if not (Dummy) then
         Moan (Init_Error, Diez);
      end if;

      Global_Objects.Mode := Userm;

   exception
      when others =>
         Kill_Local (0);
         Kill_Global (null);
         Init_Input;
         raise;
   end Init_Package;

begin
   Init_Package;
end Prolog.Ada_Logic;
