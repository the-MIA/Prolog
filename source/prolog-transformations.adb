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

with Prolog.Error_Pkg; use Prolog.Error_Pkg;
with Prolog.Errors;    use Prolog.Errors;

package body Prolog.Transformations is

   --  This module contains an assortment of useful functions and procedures
   --  for handling terms and stacks.

   -----------------
   --  Make_Func  --
   -----------------

   function Make_Func (A : Atom; M : Integer; S : Term) return Term is
      --  Construct a functor node on the global stack.
      X : Term;
   begin
      Func_Garb.Get (X);
      X.all := Node'(Funct, null, Globalf, Glosize, Glotop, A, M, S);
      Glotop  := X;
      Glosize := Glosize + 1;
      return X;
   end Make_Func;

   ----------------
   --  Make_Int  --
   ----------------

   function Make_Int (I : Integer) return Term is
      --  Construct an integer node on the global stack.
      X : Term;
   begin
      Int_Garb.Get (X);
      X.all := Node'(Intt, null, Globalf, Glosize, Glotop, I);
      Glotop  := X;
      Glosize := Glosize + 1;
      return X;
   end Make_Int;

   ----------------
   --  Make_Var  --
   ----------------

   function Make_Var (V : Term; S : Varstring) return Term is
      --  Construct a variable node on the global stack.
      X : Term;
   begin
      Var_Garb.Get (X);
      X.all := Node'(Vart, null, Globalf, Glosize, Glotop, V, S);
      Glotop  := X;
      Glosize := Glosize + 1;
      return X;
   end Make_Var;

   -------------------
   --  Kill_Global  --
   -------------------

   procedure Kill_Global (Newptr : Term) is
      Temp : Term;
   begin
      while Glotop /= Newptr loop
         Temp := Glotop.Chain;
         case Glotop.Tag is
            when Funct =>  Func_Garb.Free (Glotop);
            when Vart  =>  Var_Garb.Free (Glotop);
            when Intt  =>  Int_Garb.Free (Glotop);
            when Skelt =>  Skel_Garb.Free (Glotop);
         end case;
         Glotop  := Temp;
         Glosize := Glosize - 1;
      end loop;
   end Kill_Global;

   ---------------
   --  Is_Func  --
   ---------------

   function Is_Func (X : Term; A : Atom; M : Integer) return Boolean is
      --  True if x is a functor node with name a and arity m.
   begin
      if X.Tag /= Funct then
         return False;
      else
         return (X.Name = A) and (X.Arity = M);
      end if;
   end Is_Func;

   ---------------
   --  Is_Atom  --
   ---------------

   function Isatom (X : Term) return Boolean is
      --  True if x is an atom.
   begin
      if X.Tag /= Funct then
         return False;
      else
         return X.Arity = 0;
      end if;
   end Isatom;

   -------------
   --  Deref  --
   -------------

   function Deref (X : Term; E : Env) return Term is

      --  Dereference x as far as possible.  The result y is reached from x by
      --  a possible environment reference followed by a (possibly empty)
      --  chain of variable references.  The result cannot be subjected to
      --  further dereferencing, so y satisfies
      --
      --    (Y.TAG in (funcT, intT, varT, anonT) ) and
      --       ((Y.TAG = varT) => (Y.VAL = NIL)).
      --
      --  This function is used heavily by all parts of the interpreter.  The
      --  body of EnvRef  (3)  is inserted directly where indicated.
      Y, Z : Term;
      S    : Varstring;
   begin
      Y := X;
      if (Y.Tag = Skelt) and then (not (Y.Anont)) and then (E /= 0) then
         S := Y.St;
         Y := Envref (Y.Offset, E);
         if Y /= null and then Y.Tag = Vart then Y.Id := S; end if;
      end if;
      while Y.Tag = Vart loop
         Z := Y.Val;
         exit when Z = null;
         Y := Z;
      end loop;
      return Y;
   end Deref;

   -----------------
   --  Bind_Vars  --
   -----------------

   procedure Bindvars (V1, V2 : Term) is

      --  Bind variables v1 and v2 by assigning to one of them.  The following
      --  rules must be obeyed when variable bindings are introduced:
      --  (1) No Variable On The Global Stack May Be Bound To A
      --  variable on the local stack.  On success, the local stack may
      --  contract, and this must not affect the global stack.
      --  (2) for Much The Same Reason, No Variable On The Local Stack
      --  may be bound to a more recently created variable on the local
      --  stack.
      --  In addition, it helps to reduce the size of the trail and to make
      --  global stack reclamation more fruitful (should it ever get
      --  implemented!) if rule (2) is observed for the global stack too.
   begin
      if V1 /= V2 then
         if
           (V1.Field > V2.Field) or
             ((V1.Field = V2.Field) and (V1.Scope > V2.Scope))
         then
            V1.Val := V2;
            Trailvar (V1);
         else
            V2.Val := V1;
            Trailvar (V2);
         end if;
      end if;
   end Bindvars;

   ------------
   --  Bind  --
   ------------

   procedure Bind (V : in out Term; X : Term; E : Env; Depth : Integer) is

      --  Bind v to the value of x.  Usually it suffices to copy the 'info'
      --  field of the value, but if x is a functor in a clause, its arguments
      --  must be copied onto the global stack to make them independent of the
      --  environment.

      Y : Term;

      function Copyargs (S : Term) return Term;

      ------------
      --  Copy  --
      ------------

      function Copy (X : Term) return Term is
         --  Copy x onto the heap.
         Y, Z : Term;
      begin
         Y := Deref (X, E);
         case Y.Tag is
            when Funct =>
               if Y.Field = Heapf then
                  Z := Make_Func (Y.Name, Y.Arity, Copyargs (Y.Son));
               else
                  Z := Make_Func (Y.Name, Y.Arity, Y.Son);
               end if;
            when Intt =>
               Z := Make_Int (Y.Ival);
            when Vart =>
               Z := Make_Var (null, Y.Id);
               Bindvars (Y, Z);
            when Skelt =>
               if Y.Anont then
                  Z := Make_Var (null, Anon_String);
               else
                  null;
               end if;
         end case;
         return Z;
      end Copy;

      -----------------
      --  Copy_Args  --
      -----------------

      function Copyargs (S : Term) return Term is
         --  Copy the arguments of a functor node.
         Value, T, U, V : Term;
      begin
         if S = null then
            Value := null;
         else
            U := Copy (S);
            T := S.Brother;
            V := U;
            while T /= null loop
               V.Brother := Copy (T);
               T := T.Brother;
               V := V.Brother;
            end loop;
            Value := U;
         end if;
         return Value;
      end Copyargs;

   begin -- Bind
      if Depth > Maxdepth then
         Moan (Depth_Error, Abortz);
      end if;
      Y := Deref (X, E);
      if V.Tag /= Vart then
         Moan (Fault_Error, Diez);
      end if;
      if (Y.Tag = Funct) and (Y.Field = Heapf) then
         Func_Garb.Get (V.Val);
         V.Val.all := Node'(Funct, null, Globalf, Glosize, null, Y.Name,
                            Y.Arity, Copyargs (Y.Son));
      else
         V.Val := Y;
      end if;
   end Bind;

   ----------------
   --  Get_Body  --
   ----------------

   procedure Getbody (V : in out Term; B : Term; E : Env) is
      --  Bind v to a term representing the clause body b.
      --  b must not be the empty body.
      L, R : Term;
   begin
      if B.Brother = null then
         Bind (V, B, E, 0);
      else
         L := Make_Func (Null_Atom, 0, null);
         R := Make_Func (Null_Atom, 0, null);
         L.Brother := R;
         V := Make_Func (Commaa, 2, L);
         Bind (L, B, E, 0);
         Getbody (R, B.Brother, E);
      end if;
   end Getbody;

   ----------------
   --  List_Rep  --
   ----------------

   function Listrep (S : String) return Term is
      --  A Prolog list of the characters of s: cf. 'atom'.
      X, Y : Term;
   begin
      X := Make_Func (Nila, 0, null);
      for N in reverse S'Range loop
         Y := Make_Int (Character'Pos (S (N)));
         Y.Brother := X;
         X := Make_Func (Consa, 2, Y);
      end loop;
      return X;
   end Listrep;

end Prolog.Transformations;
