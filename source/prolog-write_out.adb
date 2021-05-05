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

with Ada.Text_IO;           use Ada.Text_IO;
with Prolog.Input_Output;   use Prolog.Input_Output;
with Prolog.Global_Objects; use Prolog.Global_Objects;
--  with Prolog.Read_In;        use Prolog.Read_In;
with Prolog.Vars;           use Prolog.Vars;

package body Prolog.Write_Out is

   use Prolog.Term_Pkg.Atom_Pkg;

    --  WriteOut writes a term to the text file 'OUTPUT', using operator
    --  information in atom entries to select the best syntax.  The current
    --  version does not quote atoms, even if they contain spaces or wierd
    --  characters. Of course, this is sometimes just what is needed!

   -------------
   --  Lprec  --
   -------------

   function Lprec (A : Atom) return Prec is
      --  The precedence for a left operand of a.
      Tf : Boolean;
   begin
      Tf := Get_Info (A).Oclass = Xfo or else
        Get_Info (A).Oclass = Xfxo or else
        Get_Info (A).Oclass = Xfyo;

      return Get_Info (A).Oprec - Boolean'Pos (Tf);
   end Lprec;

   -------------
   --  Rprec  --
   -------------

   function Rprec (A : Atom) return Prec is
      --  The precedence for a right operand of a.
      Tf : Boolean;
   begin
      Tf := Get_Info (A).all.Oclass = Fxo  or else
        Get_Info (A).Oclass = Xfxo or else
        Get_Info (A).Oclass = Yfxo;

      return Get_Info (A).Oprec - Boolean'Pos (Tf);
   end Rprec;

   ----------------
   --  Writeout  --
   ----------------

   procedure Writeout (X : Term; E : Env) is
      --  Write a term.

      ------------------
      --  Make_Quote  --
      ------------------

      function Makequote (S : String) return String is
      begin
         if
           (Charclass (S (S'First)) = Largec) or
             (Charclass (S (S'First)) = Digitc)
         then
            return ("'" & S & "'");
         end if;
         for I in S'Range loop
            if
              (Charclass (S (I)) /= Smallc) and
                (Charclass (S (I)) /= Largec) and
                (Charclass (S (I)) /= Digitc)
            then
               return ("'" & S & "'");
            end if;
         end loop;
         return S;
      end Makequote;

      ------------------
      --  Write_Term  --
      ------------------

      procedure Writeterm (X : Term; P : Prec; Depth : Integer) is
         --  Write a term with maximum precedence p.
         Y : Term;

         -------------------
         --  Write_Stand  --
         -------------------

         procedure Writestand is
            --  Write a complex term in standard notation.
            S : Term;
         begin
            if Quoteflag and not (Get_Info (Y.Name).Sys) then
               Wr_String (Makequote (Writeatom (Y.Name)));
            else
               Wr_String (Writeatom (Y.Name));
            end if;
            Wr ('(');
            Writeterm (Y.Son, Subprec, Depth + 1);
            S := Y.Son.Brother;
            while S /= null loop
               Wr (',');
               Wr_Check;
               Wr (' ');
               Writeterm (S, Subprec, Depth + 1);
               S := S.Brother;
            end loop;
            Wr (')');
            Wr_Check;
         end Writestand;

         ----------------
         --  Write_Op  --
         ----------------

         procedure Writeop is
            --  Write an operator expression.
         begin
            case Get_Info (Y.Name).Oclass is
               when Fxo | Fyo =>
                  if Quoteflag and not (Get_Info (Y.Name).Sys) then
                     Wr_String (Makequote (Writeatom (Y.Name)));
                  else
                     Wr_String (Writeatom (Y.Name));
                  end if;
                  Wr_Check;
                  Wr (' ');
                  Writeterm (Y.Son, Rprec (Y.Name), Depth + 1);
               when Xfo | Yfo =>
                  Writeterm (Y.Son, Lprec (Y.Name), Depth + 1);
                  Wr_Check;
                  Wr (' ');
                  if Quoteflag and not (Get_Info (Y.Name).Sys) then
                     Wr_String (Makequote (Writeatom (Y.Name)));
                  else
                     Wr_String (Writeatom (Y.Name));
                  end if;
               when Xfxo | Xfyo | Yfxo =>
                  Writeterm (Y.Son, Lprec (Y.Name), Depth + 1);
                  if
                    (Y.Name /= Commaa) and
                      (Y.Name /= Semia)
                  then
                     Wr (' ');
                  end if;
                  if Quoteflag and not (Get_Info (Y.Name).Sys) then
                     Wr_String (Makequote (Writeatom (Y.Name)));
                  else
                     Wr_String (Writeatom (Y.Name));
                  end if;
                  Wr_Check;
                  Wr (' ');
                  Writeterm (Y.Son.Brother, Rprec (Y.Name), Depth + 1);
               when Nono => null;
            end case;
         end Writeop;

         -----------------
         --  Write_Exp  --
         -----------------

         procedure Writeexp is
            --  Write an operator expression, using parentheses if higher
            --  precedence is needed.
         begin
            if P < Get_Info (Y.Name).Oprec then
               Wr ('(');
               Writeop;
               Wr (')');
            else
               Writeop;
            end if;
            Wr_Check;
         end Writeexp;

         ---------------------
         --  Writelist_Old  --
         ---------------------

         procedure Writelist_Old is
            --  Write a list in square bracket notation.
            N : Integer;
            Z : Term;
         begin
            Wr ('[');
            Writeterm (Y.Son, Subprec, Depth + 1);
            N := 1;
            Z := Deref (Y.Son.Brother, E);
            while (N /= Writelength) and Is_Func (Z, Consa, 2) loop
               Wr (',');
               Wr_Check;
               Wr (' ');
               Writeterm (Z.Son, Subprec, Depth + 1);
               Z := Deref (Z.Son.Brother, E);
               N := N + 1;
            end loop;
            if not Is_Func (Z, Nila, 0) then
               if N < Writelength then
                  Wr (' ');
                  Wr ('|');
                  Wr_Check;
                  Wr (' ');
                  Writeterm (Z, Subprec, Depth + 1);
               else
                  Wr (' ');
                  Wr ('.');
                  Wr ('.');
                  Wr ('.');
               end if;
            end if;
            Wr (']');
            Wr_Check;
         end Writelist_Old;
         pragma Unreferenced (Writelist_Old);

         -----------------
         --  Writelist  --
         -----------------

         procedure Writelist is
            --  Write a list in pointed notation.
            N : Integer;
            Z : Term;
         begin
            Wr ('(');
            Writeterm (Y.Son, Subprec, Depth + 1);
            N := 1;
            Z := Deref (Y.Son.Brother, E);
            while (N /= Writelength) and Is_Func (Z, Consa, 2) loop
               Wr_Check; Wr ('.');
               Writeterm (Z.Son, Subprec, Depth + 1);
               Z := Deref (Z.Son.Brother, E);
               N := N + 1;
            end loop;
            if N < Writelength then
               Wr_Check; Wr ('.');
               Writeterm (Z, Subprec, Depth + 1);
               Wr_Check;
            else
               Wr (' '); Wr ('.'); Wr ('.'); Wr ('.');
            end if;
            Wr (')'); Wr_Check;
         end Writelist;

         ------------------
         --  Write_Func  --
         ------------------

         procedure Writefunc is
            --  Write a complex term.
         begin
            if Y.Arity > 2 then
               Writestand;
            else
               case Y.Arity is
                  when 0 =>
                     if Quoteflag and not (Get_Info (Y.Name).Sys) then
                        Wr_String (Makequote (Writeatom (Y.Name)));
                     else
                        Wr_String (Writeatom (Y.Name));
                     end if;
                     Wr_Check;
                  when 1 =>
                     if Y.Name = Curlya then
                        Wr ('}');
                        Writeterm (Y.Son, Maxprec, Depth + 1);
                        Wr ('K');
                        Wr_Check;
                     else
                        if Get_Info (Y.Name).Oclass in Fxo .. Yfo then
                           Writeexp;
                        else
                           Writestand;
                        end if;
                     end if;
                  when 2 =>
                     if Y.Name = Consa then
                        Writelist;
                     else
                        if Get_Info (Y.Name).Oclass in Xfxo .. Yfxo then
                           Writeexp;
                        else
                           Writestand;
                        end if;
                     end if;
                  when others => null;
               end case;
            end if;
         end Writefunc;

         -----------------
         --  Write_Var  --
         -----------------

         procedure Writevar is
         begin
            if Y.Tag = Skelt then
               Wr_String (To_String (Y.St));
               Wr_Check;
            else
               Wr_String (To_String (Y.Id));
               Wr_Check;
            end if;
         end Writevar;

      begin --  WriteTerm
         if Depth = Writedepth then
            Wr ('.');
            Wr ('.');
            Wr ('.');
         else
            Y := Deref (X, E);
            case Y.Tag is
               when Skelt =>
                  if Y.Anont then
                     Writevar;
                  else
                     Writevar;
                  end if;
               when Funct =>
                  Writefunc;
               when Intt =>
                  if Y.Ival >= 0 then
                     Wr_Int (Y.Ival);
                  elsif Y.Ival = Integer'First then
                     declare
                        type Acc_String is access String;
                        Last_Int : constant Acc_String :=
                          new String'(Integer'Image (Integer'First));
                     begin
                        Wr ('^');
                        Wr_String (Last_Int.all (Last_Int'First + 1 ..
                                                   Last_Int'Last));
                     end;
                  else
                     Wr ('^');
                     Wr_Int (-Y.Ival);
                  end if;
               when Vart =>
                  Writevar;
            end case;
         end if;
         Wr_Check;
      end Writeterm;

   begin --  WriteOut
      Varcount := 0;
      Writeterm (X, Maxprec, 0);
   end Writeout;

   -------------
   --  Trace  --
   -------------

   procedure Trace (M      : Tracemessage;
                    X      : Term;
                    E      : Env;
                    Indent : Integer := 0)
   is
      --  Output a trace message.
      Y : Term;

      procedure Write_Term (X : Term; P : Prec; Depth : Integer)
      is
         pragma Unreferenced (P);

         Y : Term;

         procedure Write_Func is
            --  Write a complex term in standard notation.
            S : Term;
         begin
            Put (Writeatom (Y.Name));
            if Y.Arity /= 0 then
               Put ("(");
               Write_Term (Y.Son, Subprec, Depth + 1);
               S := Y.Son.Brother;
               while S /= null loop
                  Put (",");
                  Put (" ");
                  Write_Term (S, Subprec, Depth + 1);
                  S := S.Brother;
               end loop;
               Put (")");
            end if;
         end Write_Func;

      begin
         Y := Deref (X, E);
         case Y.Tag is
            when Skelt =>
               Put (To_String (Y.St));
            when Funct =>
               Write_Func;
            when Intt =>
               Put (Integer'Image (Y.Ival));
            when Vart =>
               Put (To_String (Y.Id));
         end case;
      end Write_Term;

   begin
      Y := Deref (X, E);
      --  Don't trace evaluable predicates unless debugging interpreter.
      --     if DEBUGGING or not GET_INFO(Y.NAME).SYS then
      for I in 1 .. Indent loop Put (" "); end loop;
      case M is
         when Goald   => Put ("GOAL:     ");
         when Provedd => Put ("PROVED:   ");
      end case;
      Varcount := 0;
      Write_Term (Y, Maxprec, 0);
      New_Line;
      --     end if;
   end Trace;

end Prolog.Write_Out;
