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

with Ada.Text_IO;           use Ada.Text_IO;

with Prolog.Database;       use Prolog.Database;
with Prolog.Error_Pkg;      use Prolog.Error_Pkg;
with Prolog.Global_Objects; use Prolog.Global_Objects;
with Prolog.Input_Output;   use Prolog.Input_Output;

with Prolog.Execute;        use Prolog.Execute;
with Prolog.Errors;         use Prolog.Errors;

with Prolog.Read_In;        use Prolog.Read_In;
with Prolog.Vars;           use Prolog.Vars;
with Prolog.Write_Out;      use Prolog.Write_Out;


package body Prolog.Evaluable_Predicates is

   ----------------
   --  Evaluate  --
   ----------------

   function Evaluate (X : Term; E : Env; Depth : Integer) return Integer is
      --  Evaluate x as an arithmetic expression.
      Y     : Term;
      A, B  : Integer;
      Value : Integer;
   begin
      if Depth > Maxdepth then
         Moan (Depth_Error, Abortz);
      end if;
      Y := Deref (X, E);
      case Y.Tag is
         when Skelt =>
            if Y.Anont then
               Moan (Bad_Exp_Error, Abortz);
            else
               null;
            end if;
         when Funct =>
            if Y.Arity = 2 then
               if Y.Name = Consa then
                  Value := Evaluate (Y.Son, E, Depth + 1);
               else
                  A := Evaluate (Y.Son, E, Depth + 1);
                  B := Evaluate (Y.Son.Brother, E, Depth + 1);
                  if Y.Name = Plusa then
                     Value := A + B;
                  elsif Y.Name = Minusa then
                     Value := A - B;
                  elsif Y.Name = Timesa then
                     Value := A * B;
                  elsif Y.Name = Dividea then
                     if B = 0 then
                        Moan (Divide_Error, Abortz);
                     end if;
                     Value := A / B;
                  elsif Y.Name = Moda then
                     if B = 0 then
                        Moan (Divide_Error, Abortz);
                     end if;
                     Value := A mod B;
                  else
                     Moan (Bad_Exp_Error, Abortz);
                  end if;
               end if;
            else
               if  (Y.Name = Nega) and (Y.Arity = 1) then
                  Value := -Evaluate (Y.Son, E, Depth + 1);
               else
                  Moan (Bad_Exp_Error, Abortz);
               end if;
            end if;
         when Intt =>
            Value := Y.Ival;
         when Vart =>
            Moan (Bad_Exp_Error, Abortz);
      end case;
      return Value;
   end Evaluate;

   ------------------
   --  Int_Result  --
   ------------------

   function Intresult (X : Term; E : Env; I : Integer) return Boolean is
      --  Specialized unification algorithm for returning integer results.
      --  IntResult (x, e, i) is equivalent to Unify(x, MakeInt(i), e, 0, 0)
      --  but avoids allocating a global node.

      Y     : Term;
      Value : Boolean;
   begin
      Y := Deref (X, E);
      case Y.Tag is
         when Funct =>
            Value := False;
         when Intt =>
            Value := Y.Ival = I;
         when Vart =>
            Value := Unify (X, Make_Int (I), E, 0, 0);
         when Skelt =>
            if Y.Anont then
               Value := True;
            end if;
      end case;
      return Value;
   end Intresult;

   ----------------------
   --  Call_Eval_Pred  --
   ----------------------

   function Call_Eval_Pred
     (Call    : Term;
      E       : Env;
      Routine : Evalpred;
      Arity   : Integer;
      Dbase   : Integer_List) return Boolean
   is
      --  Call an evaluable predicate.

      Result : Boolean;
      Argval : array  (1 .. Maxevalarity) of Term;
      Ans    : Solution;
      Dummy  : Clptr;

      ----------------
      --  Get_Args  --
      ----------------

      procedure Getargs is
         --  Fill in argval with the tfm.dereferenced arguments.
         X, A : Term;
      begin
         X := Deref (Call, E);
         if Arity /= X.Arity then
            Moan (Arity_Error, Abortz);
         end if;
         A := X.Son;
         for I in 1 .. Arity loop
            Argval (I) := Deref (A, E);
            A := A.Brother;
         end loop;
      end Getargs;

      ---------------
      --  Do_Call  --
      ---------------

      procedure Docall is
         --  Evaluable predicate 'call'. This code is tricky.
         X  : Term;
         E1 : Env;
      begin
         if Argval (1).Tag /= Funct then
            Moan (Call_Error, Abortz);
         end if;

         E1 := Newenv (Call, E, null, Dbase, 1, Glotop, Present_Trail);
         X  := Envref (1, E1);
         Bind (X, Argval (1), E, 0);
         First_Answer (X, E1, Dbase, Ans, Result);

         if E1 > Choicepoint then
            Disposeenv;
         end if;

      end Docall;

      ---------------
      --  Do_Get0  --
      ---------------

      procedure Doget0 is
         --  Evaluable predicate 'get0'.
         Ch : Character;
      begin
         if In_File_Depth = 1 then
            Get (Ch);
         else
            if File_Ended then
               Moan (Eof_Error, Diez);
            end if;
            Ch := Getchar;
         end if;
         Result := Intresult (Argval (1), E, Character'Pos (Ch));
      end Doget0;

      --------------
      --  Do_Put  --
      --------------

      procedure Doput is
         --  Evaluable predicate 'put'.
         Ch : Integer;
      begin
         Ch := Evaluate (Argval (1), E, 0);
         if Out_File_Depth = 1 then
            Put (Character'Val (Ch));
         else
            Wr (Character'Val (Ch));
         end if;
      end Doput;

      -------------
      --  Do_Op  --
      -------------

      procedure Doop is
         --  Evaluable predicate 'op'.
         P    : Integer;
         A    : Atom;
         F    : Optype;
         Info : Atom_Info;
      begin
         if (Argval (1).Tag /= Intt) or
           not Isatom (Argval (2)) or
           not Isatom (Argval (3))
         then
            Moan (Op_Error, Abortz);
         end if;
         P := Argval (1).Ival;
         A := Argval (2).Name;
         if  (P < 1) or (P > Maxprec) then
            Moan (Op_Error, Abortz);
         end if;
         if A = Fxa then
            F := Fxo;
         elsif A = Fya then
            F := Fyo;
         elsif A = Xfa then
            F := Xfo;
         elsif A = Yfa then
            F := Yfo;
         elsif A = Xfxa then
            F := Xfxo;
         elsif A = Xfya then
            F := Xfyo;
         elsif A = Yfxa then
            F := Yfxo;
         else
            Moan (Op_Error, Abortz);
         end if;
         Info := Get_Info (Argval (3).Name);
         Info.Oclass := F;
         Info.Oprec  := P;
         Set_Info (Argval (3).Name, Info);
      end Doop;

      ---------------
      --  Do_Name  --
      ---------------

      procedure Doname is
         --  Evaluable predicate 'name'.
         X, Y : Term;
         Ch   : Integer;
      begin --  doname
         if Isatom (Argval (1)) then
            Result := Unify (Argval (2),
                             Listrep (Writeatom (Argval (1).Name)), E, 0, 0);
         else
            Startatom;
            X := Argval (2);
            while Is_Func (X, Consa, 2) loop
               Y := Deref (X.Son, E);
               if Y.Tag /= Intt then
                  Moan (Name_Arg_Error, Abortz);
               end if;
               Ch := Y.Ival;
               if  (Ch < Character'Pos (Character'First)) or
                 (Ch > Character'Pos (Character'Last))
               then
                  Moan (Bad_Char_Error, Abortz);
               end if;
               Atomchar (Character'Val (Ch));
               X := Deref (X.Son.Brother, E);
            end loop;
            if not Is_Func (X, Nila, 0) then
               Moan (Name_Arg_Error, Abortz);
            end if;
            Result := Unify (Argval (1),
                             Make_Func (Lookup, 0, null), E, 0, 0);
         end if;
      end Doname;

      --------------
      --  Aabort  --
      --------------

      procedure Aabort is
      begin
         Moan (Fault_Error, Abortz);
      end Aabort;

      ------------------
      --  Do_Retract  --
      ------------------

      procedure Doretract is
         Cl    : Clptr;
         Found : Boolean;
         Trl   : Trail;
         H, T  : Term;
         Ptr   : Term;
      begin
         if Argval (1).Tag /= Funct then
            Moan (Clause_Error, Abortz);
         end if;

         if Argval (1).Name = Arrowa then
            H := Argval (1).Son;
            T := Argval (1).Son.Brother;
         else
            H := Argval (1);
            T := null;
         end if;

         Trl := Present_Trail;
         Result := False;
         Cl := Get_Info (Argval (1).Name).Proc;

         loop
            Find_Clause (H, E, 0, Cl, Found);
            if not Found then
               exit when True;

            elsif Unify (H, Cl.Head, E, E, 0) then
               if  (T = null) and (Cl.The_Body = null) then
                  null;
               elsif  (T = null) or (Cl.The_Body = null) then
                  Found := False;
               else
                  Ptr := Cl.The_Body;
                  while Ptr /= null loop
                     if not Unify (T, Ptr, E, E, 0) then
                        exit when True;
                     else
                        Ptr := Ptr.Brother;
                        T := T.Brother;
                     end if;
                  end loop;
                  if Ptr /= null then
                     Found := False;
                  end if;
               end if;
               if Found then
                  Zap_Clause (Cl);
                  Result := True;
                  exit when True;
               elsif Cl.Next /= Get_Info (Cl.Head.Name).Proc then
                  Cl := Cl.Next;
               else
                  exit when True;
               end if;
            elsif Cl.Next /= Get_Info (Cl.Head.Name).Proc then
               Cl := Cl.Next;
            else
               exit when True;
            end if;
         end loop;
         Trimtrail (Trl);
      end Doretract;

      ------------------
      --  Do_Functor  --
      ------------------

      procedure Dofunctor is
         --  Evaluable predicate 'functor'.
         --  1st arg var => 1st arg :=  (2nd arg)(..skeleton :arity N..).
         --  1st arg functor => 2nd arg := name, 3rd arg := arity.
         --  1st arg integer => 2nd arg = 3rd arg.
         X, Y : Term;
         M    : Integer;
      begin
         case Argval (1).Tag is
            when Funct =>
               if not Intresult (Argval (3), E, Argval (1).Arity) then
                  Result := False;
               else
                  Result := Unify (Argval (2), Make_Func (Argval (1).Name,
                                                          0, null), E, 0, 0);
               end if;

            when Intt =>
               if not Intresult (Argval (3), E, 0) then
                  Result := False;
               else
                  Result := Intresult (Argval (2), E, Argval (1).Ival);
               end if;

            when Vart | Skelt =>
               if (Argval (1).Tag = Skelt) and then not Argval (1).Anont then
                  null;
               else
                  if Argval (3).Tag /= Intt then
                     Moan (Functor_Error, Abortz);
                  end if;
                  M := Argval (3).Ival;
                  if Isatom (Argval (2)) and (M >= 0) then
                     X := null;
                     for Ix in reverse 1 .. M loop
                        Y := Make_Var (null, Null_String);
                        Y.Brother := X;
                        X := Y;
                     end loop;
                     Result := Unify (Argval (1), Make_Func (Argval (2).Name,
                                                             M, X),
                                      E, 0, 0);
                  elsif  (Argval (2).Tag = Intt) and (M = 0) then
                     Result := Intresult (Argval (1), E,
                                          Argval (2).Ival);
                  else
                     Moan (Functor_Error, Abortz);
                  end if;
               end if;
         end case;

      end Dofunctor;

      --------------
      --  Do_Arg  --
      --------------

      procedure Doarg is
         --  Evaluable predicate 'arg'.
         --  First argument has to be an Integer, 2nd a function.
         X : Term;
         N : Integer;
      begin
         if  (Argval (1).Tag /= Intt) or
           (Argval (2).Tag /= Funct)
         then
            Result := False;
         else
            N := Argval (1).Ival;
            if  (N < 1) or (N > Argval (2).Arity) then
               Result := False;
            else
               X := Argval (2).Son;
               for I in 2 .. N loop
                  X := X.Brother;
               end loop;
               Result := Unify (Argval (3), X, E, E, 0);
            end if;
         end if;
      end Doarg;

      -------------------
      --  Do_Same_Var  --
      -------------------

      procedure Dosamevar is
      begin
         if  (Argval (1).Tag /= Vart) or (Argval (2).Tag /= Vart) then
            Moan (Fault_Error, Diez);
         else
            Result :=  (Argval (1).Id = Argval (2).Id);
         end if;
      end Dosamevar;

   begin --  CallEvalPred
      Getargs;
      Result := True; --  Default value.
      case Routine is
         when Callr => Docall; --  Right here

         when Cutr => Cut (E); --  In LOCAL_STACK

         when Readr => Result := Unify (Read_In.Read_In, Argval (1), 0, E, 0);
                                 --  In WRITE_OUT

         when Writer => Writeout (Argval (1), E);

         when Get0r => Doget0; --  Right here

         when Putr => Doput; --  Right Here

         when Nlr => Wrln; --  In IO

         when Eolnr => Result := Line_Ended; --  In IO

         when Eofr => Result := File_Ended; --  In IO

         when Namer => Doname; --  Right here

         when Opr => Doop; --  Right here

         when Abortr => Aabort; --  Right here

         when Tracer => Tracing := True; --  In GLOBAL_OBJECTS

         when Notracer => Tracing := False; --  In GLOBAL_OBJECTS

         when Atomr => Result := Isatom (Argval (1)); --  In TRANSFORM

         when Integerr => Result := Argval (1).Tag = Intt; --  Right here.

         when Varr => Result :=  ((Argval (1).Tag = Vart) or
                                    (Argval (1).Tag = Skelt)); --  Right here.

         when Isr =>
            Result := Intresult (Argval (1), E, Evaluate (Argval (2), E, 0));
            --  Right here

         when Ltr => Result :=  (Evaluate (Argval (1), E, 0) <
                                   Evaluate (Argval (2), E, 0)); --  Right here

         when Assertar | Assertzr =>
            Dummy := Add_Clause (Argval (1), E, 0,
                                 Routine = Assertar);
            --  In DATABASE

         when Retractr => Doretract; --  Right here

         when Functorr => Dofunctor; --  Right here.

         when Argr => Doarg; --  Right here.

         when Listr => Listing := True; --  In IO

         when Nolistr => Listing := False; --  In IO

         when Debugr => Debugging := True; --  In WRITE_OUT

         when Seer =>
            Result := See_File (Writeatom (Argval (1).Name)); --  In IO

         when Seenr => Result := Seenfile; --  In IO

         when Tellr =>
            Result := Tell_File (Writeatom (Argval (1).Name)); --  In IO

         when Toldr => Result := Toldfile; --  In IO

         when Quoter => Quoteflag := True; --  In WRITE_OUT

         when Noquoter => Quoteflag := False; --  In WRITE_OUT

         when Samevarr => Dosamevar;

         when Endr => Haltflag := True;

      end case;
      return Result;
   end Call_Eval_Pred;

end Prolog.Evaluable_Predicates;
