
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

with Ada.Text_IO;            use Ada.Text_IO;

with Prolog.Error_Pkg;       use Prolog.Error_Pkg;
with Prolog.Errors;          use Prolog.Errors;
with Prolog.Global_Objects;  use Prolog.Global_Objects;
with Prolog.Input_Output;    use Prolog.Input_Output;
with Prolog.Transformations; use Prolog.Transformations;
with Prolog.Vars;            use Prolog.Vars;

package body Prolog.Read_In is

    use Prolog.Term_Pkg.Atom_Pkg;

    --  Read_In reads a Prolog sentence from the current input file and builds
    --  a term from it.  The sentence is parsed using a shift- reduce parsing
    --  algorithm which depends on operator information in atom entries.

    type Token is (Atom_Token,  Int_Token,  Var_Token, Leftpar,
                   Rightpar,    Leftsq,     Rightsq,   Lcurly,
                   Rcurly,      Comma,      Vbar,      Fullstop,
                   List_Token,  End_Token);

    Ch, Lastch : Character := ' ';
    Tkn       : Token;
    This_Int  : Integer;
    This_Atom : Atom;
    This_List : Term;
    This_Var  : Term;

    type Var_Table_Type is record
        Ident : Varstring;
        Rootvar : Term;
    end record;

    Vartable : array (1 .. Maxvars) of Var_Table_Type;
    Varcount : Integer range 0 .. Maxvars;

   --------------
   --  Lookup  --
   --------------

   function Lookup (Newvar : Varstring) return Term is
   --  Mark the latest variable name permanent.
   begin
       for N in 1 .. Varcount loop
           if Newvar = Vartable (N).Ident then
               return Makevar (Vartable (N).Rootvar,
                               Vartable (N).Rootvar.Id);
           end if;
       end loop;

       Varcount := Varcount + 1;
       Vartable (Varcount).Rootvar := Makevar (null, Newvar);
       Vartable (Varcount).Ident := Newvar;
       return Vartable (Varcount).Rootvar;
   end Lookup;

   ----------------
   --  Initvars  --
   ----------------

   procedure Initvars is
   begin
       Varcount := 0;
   end Initvars;

   -----------
   -- Lprec --
   -----------

   function Lprec(A: Atom) return Integer is
       --  The precedence for a left operand of a.
       Tf:Boolean;
   begin
      Tf := Get_Info(A).Oclass =Xfo or else
            Get_Info(A).Oclass=Xfxo or else
            Get_Info(A).Oclass=Xfyo;
      return Get_Info(A).Oprec - Boolean'Pos(Tf);
   
   end Lprec;

   -----------
   -- Rprec --
   -----------

   function Rprec(A: Atom) return Integer is
       --  The precedence for a right operand of a.
       Tf : Boolean;
   begin
      Tf := Get_Info(A).all.Oclass = Fxo  or else
            Get_Info(A).Oclass = Xfxo     or else
            Get_Info(A).Oclass = Yfxo;

      return Get_Info(A).Oprec - Boolean'Pos(Tf);
   end Rprec;

   ------------------------
   --  Skip_White_Space  --
   ------------------------

   procedure Skip_White_Space is
   begin
      loop
         exit when Ch = Ascii.Nul or Charclass (Ch) /= Spacec;
         Ch := Getchar;

      end loop;

   end Skip_White_Space;
         
   ------------------
   --  Line_Ended  --
   ------------------

   function Line_Ended return Boolean is
   begin
      return Ch = Ascii.LF or Ch = Ascii.Nul;
   end;

   --------------------
   --  Skip_Comment  --
   --------------------

   procedure Skip_Comment is
   begin
      --  Comment, ignore till end of line
      --  Skip whitespace till end of line
      loop
         exit when Ch = Ascii.LF or Ch = Ascii.Nul;
         Ch := Getchar;
      end loop;

      if debug then
         put("just skipped all comments, ch is now " & ch);
         new_line;
      end if;

   end;
      
   --------------------------
   --  Skip_To_Next_Token  --
   --------------------------

   procedure Skip_To_Next_Token is
   begin
      loop
         Skip_White_Space;
         if ch = '%' then
            Skip_Comment;
         end if;
         exit when Charclass (Ch) /= Spacec;
      end loop;
   end;
      
   -------------------
   --  Reset_Lexan  --
   -------------------

   procedure Reset_Lexan is
   begin
      Ch := ' ';
   end;

   -----------
   -- Lexan --
   -----------

   function Lexan return Token is
   --  The lexical analyser.
   --  Because other routines (notably elaboration code for ada_logic)
   --  presume to check end_of_file rather than leaving it up to the
   --  the analyzer, we have to ensure that we always leave the input
   --  in a state where it is either
   --  * ready to parse another real sentence
   --  * at end of the input
   --  This means skipping trailing blanks & comments.

   begin
      loop
         Skip_White_Space;

   --      if File_Ended then
   --         if debug then 
   --            put ("file ended");
   --            new_line;
   --         end if;

   --         return End_Token;
   --      else

         if True then
            case Ch is
               when Ascii.Nul  =>
                  return End_Token;

               when 'a' .. 'z' =>
                  --  Pick up an identifier
                  Startatom;

                  loop
                      Atomchar(Ch);
                      Ch := Getchar;

                      exit when not (Charclass(Ch) = Smallc or else
                                     Charclass(Ch) = Largec or else
                                     Charclass(Ch) = Digitc);
                  end loop;


                  This_Atom := Lookup;

                  --  Some useful debugging statements
                  --  Put ("Atom is '");
                  --  Put ( WriteAtom (This_Atom));
                  --  Put ("'");
                  --  New_Line;

                  return Atom_Token;

                when ''' =>
                    --  Quoted string 'hello'
                    Startatom;
                    loop
                        if Line_Ended then
                            Moan (Need_Quote_Error, Syntaxz);
                        end if;

                        Ch := Getchar;

                        if Ch = ''' then
                            Ch := Getchar;
                            exit when Ch /= ''';
                            Atomchar(Ch);
                        else
                            Atomchar(Ch);
                        end if;

                    end loop;

                    This_Atom := Lookup;
                    return Atom_Token;

               when '"' =>
                    Startatom;
                    loop
                        if Line_Ended then
                            Moan (Need_Quote_Error, Syntaxz);
                        end if;

                        Ch := Getchar;
                        if Ch = '"' then
                            Ch := Getchar;
                            exit when Ch /= '"';
                            Atomchar(Ch);
                        else
                            Atomchar(Ch);
                        end if;

                    end loop;

                    This_List := Listrep(Writeatom(Lookup));
                    return List_Token;

               when '_' =>
                    Ch := Getchar;
                    if Charclass(Ch) /= Smallc or else
                       Charclass(Ch) /= Largec or else
                       Charclass(Ch) /= Digitc then

                       This_Var := Makevar (null, Anon_String);
                       return Var_Token;
                    else
                        Startvar;
                        Varchar ('_');
                        while Charclass (Ch) = Smallc or else
                              Charclass (Ch) = Largec or else
                              Charclass (Ch) = Digitc loop
                            Varchar (Ch);
                            Ch := Getchar;
                        end loop;

                        This_Var := Lookup (Keepvar);
                        return Var_Token;
                    end if;

               when 'A' .. 'Z' =>
                    Startvar;
                        while Charclass(Ch) = Smallc or else
                              Charclass(Ch) = Largec or else
                              Charclass(Ch) = Digitc loop
                        Varchar (Ch);
                        Ch := Getchar;
                    end loop;

                    This_Var := Lookup (Keepvar);
                    return Var_Token;

               when '0' .. '9' =>
                    This_Int := 0;
                    loop
                        This_Int := 10 * This_Int + (Character'Pos(Ch) -
                                                   Character'Pos('0'));
                        Ch := Getchar;
                        exit when Charclass (Ch) /= Digitc;
                    end loop;

                    return Int_Token;

               when '(' =>
                    Ch := Getchar;
                    return Leftpar;

               when ')' =>
                    Ch := Getchar;
                    return Rightpar;

               when '[' =>
                    Ch := Getchar;
                    if Ch = ']' then
                        Ch := Getchar;
                        This_Atom := Nila;
                        return Atom_Token;

                    else
                        return Leftsq;
                    end if;

               when ']' =>
                    Ch := Getchar;
                    return Rightsq;

               when '{' => 
                    Ch := Getchar;
                    if Ch = 'K' then
                        Ch := Getchar;
                        This_Atom := Curlya;
                        return Atom_Token;
                    else
                        Ch := Getchar;
                        return Lcurly;
                    end if;

               when '}' =>
                    Ch := Getchar;
                    return Rcurly;

               when ',' =>
                  Ch := Getchar;
                  if Ch = '.' then
                      Ch := Getchar;
                      if Ch = '.' then
                          Ch := Getchar;
                          return Vbar;
                      else
                          Moan (Bad_Cdd_Error, Syntaxz);
                      end if;
                  else
                      return Comma;
                  end if;

               when '!' =>
                  Ch := Getchar;
                  This_Atom := Cuta;
                  return Atom_Token;

               when ';' =>
                  Ch := Getchar;
                  This_Atom := Semia;
                  return Atom_Token;

               when '|' =>
                  Ch := Getchar;
                  return Vbar;

               when '%' =>
                  Skip_Comment;

               when others =>

                  if Charclass (Ch) = Specialc then
                      Lastch := Ch;
                      Ch := Getchar;

                      if (Lastch = '/') and (Ch = '*') then
                         Ch := Getchar;

                         loop
                            Lastch := Ch; Ch := Getchar;
                            exit when (Lastch = '*') and (Ch = '/');
                         end loop;

                         Ch := Getchar;
                         return Lexan;

                      elsif (Lastch = '^') and (Charclass(Ch) = Digitc) then
                         This_Int := 0;
                         loop
                            This_Int := 10*This_Int + (Character'Pos(Ch) -
                                                      Character'Pos('0'));
                            Ch := Getchar;
                            exit when (Charclass(Ch) /= Digitc);
                         end loop;

                         This_Int := - This_Int;
                         return Int_Token;

                      elsif (Lastch = '.') and (Charclass(Ch) = Spacec) then
                         return Fullstop;

                      else
                         Startatom;
                         Atomchar(Lastch);

                         while Charclass(Ch) = Specialc loop
                            Atomchar(Ch);
                            Ch := Getchar;
                         end loop;

                         This_Atom := Lookup;
                         return Atom_Token;

                      end if;

                   elsif Charclass(Ch) = Spacec then
                          
                      Ch := Getchar;

                   else
                      Moan (Wierd_Ch_Error, Syntaxz);

                   end if;
               end case;
            end if;
         end loop;
    end Lexan;

    function Read_In return Term is
        --  Input and parse a Prolog sentence and build a term from it.  The
        --  finite state part of the parser is characterized by the variables
        --  'context' and 'expected'.

        --  'context' indicates the construct being parsed:

        --     outerK       The outermost level of a sentence.
        --     innerK       An expression in parentheses.
        --     funcK        The arguments of a functor.
        --     listK        The elements of a list.
        --     endlistK     A list continuation (between '|' or ',..'
        --                                       and ') ' in a list).
        --     curlyK       An expression in curly brackets.
        --     finalK       None. The expression is complete.

        --  'expected' indicates whether the next symbol is to be an operator
        --  (opX) or An Operand (Randx).

        --  Two stacks are used: one, represented by the array 'stack', to hold
        --  parts ot incompletely parsed terms, the other, represented by the
        --  array 'statestack', to hold contextual information during parsing
        --  of nested constructs. In fact, the parsing algorithm corresponds
        --  to a stack machine with a single stack, but two stacks are used
        --  only as a matter of convenience.

        type Readstate is
              (Outer_K, Inner_K, Funck, Listk, Endlistk, Curlyk, Finalk);
        --  state of the readin.

        --  The term Stack :

        type Elemtag is (Terml, Opl, Funcl, Markl);
        --  types of elements on stack.

        subtype Atomtag is Elemtag range  Opl .. Funcl;
        --  Kind of tag used for atoms.

        type Stack_Type is record
            Tag : Elemtag;
            Tval: Term; --  Used only if tag is termL
            Aval : Atom; --  Used only if tag is opL, funcL
        end record;
        --  This workaround necessary because there is NO way
        --  in Ada to dynamically change the TAG on an array
        --  of variant records.

        Stack: array  (1 .. Readsize)  of  Stack_Type;
        Top: Integer range 0 .. Readsize; --  Top of term stack.

        --  the state Stack :

        type Statestack_Type is record
            Scontext: Readstate;
            Shiprec: Prec;
        end record;

        Statetop: Integer range 0 .. Readdepth;
        Statestack: array (1 .. Readdepth) of Statestack_Type;

        type Expected_Type is (Opx, Randx);
        Expected: Expected_Type;
        --  Expecting a operator or an operand (randX) .

        type Stateset is array(Readstate)of Boolean;
        S :Stateset; --  A state set modeled by an array.

        Context: Readstate; --  The current context.
        Hiprec, Loprec: Prec; --  Current High and Low Precedences.
        Value : Term; --  Value returned by readin.

        ------------
        --  Push  --
        ------------

        procedure Push(T: Elemtag) is
            --  Push a new element onto the stack.
        begin
            if Top >= Readsize then
                Moan (Read_Stack_Error, Syntaxz);
            end if;
            Top := Top + 1;
            Stack(Top).Tag := T;
            case T is
                when Terml => Stack(Top).Tval :=  null;
                when Opl | Funcl => Stack(Top).Aval :=  Null_Atom;
                when Markl => null;
            end case;
        end Push;

        -----------------
        --  Shiftterm  --
        -----------------

        procedure Shiftterm(X: Term) is
            --  Push x onto the stack.
        begin
            Push(Terml);
            Stack(Top).Tval := X;
        end Shiftterm;

       -------------
       --  Shift  --
       -------------

        procedure Shift(T: Atomtag;A: Atom) is
            --  Push a onto the stack, either as an operator OR as a functor.
        begin
            Push(T);
            Stack (Top).Aval := A;
        end Shift;

       -----------
       --  Pop  --
       -----------

        function Pop return Term is
            --  Pop a term off the stack.
        begin
            Top := Top - 1;
            return Stack(Top+1).Tval;
        end Pop;

       --------------
       --  Reduce  --
       --------------

       procedure Reduce(P, Lp: Prec) is
           --  Collapse items on the stack. Before each reduction step, the
           --  operator a on top of the stack is 'balanced' against the
           --  precedences p = b@.oprec and lp = Lprec(b) of a new operator b,
           --  to see if a could be a left operand of b, or b a right operand of
           --  a. If neither is possible or both are possible, a precedence
           --  conflict is reported.  If only the first is possible, a reduction
           --  step is taken. If only the second is possible, reduction is
           --  complete.

           X, Y: Term;
           A: Atom;
           Reduced: Boolean;
       begin
           X := Pop;
           Reduced := False;
           while (Stack(Top).Tag = Opl) and not Reduced loop
               A := Stack(Top).Aval;
               case Integer(Boolean'Pos(Rprec(A) >= P)) -
                    Integer(Boolean'Pos(Get_Info(A).Oprec <= Lp)) is
                   when 1 =>
                       Reduced := True;
                   when 0 =>
                       Moan (Conflict_Error, Syntaxz);
                   when -1 =>
                       Top := Top - 1;
                       case Get_Info(A).Oclass is
                           when Fxo | Fyo =>
                               X := Makefunc(A, 1, X);
                           when Xfxo | Xfyo | Yfxo =>
                               Y := Pop;
                               Y.Brother := X;
                               X := Makefunc(A, 2, Y);
                           when others => null;
                       end case;
                   when others => null;
               end case;
           end loop;
           Shiftterm(X);

       end Reduce;

       ------------------
       --  Checkdelim  --
       ------------------

       procedure Checkdelim (S :in out Stateset) is
           --  Attempt to force the state required for a delimiter.
           --  This state must satisfy the predicate
           --      (expected = opX) and (context in s).
           --  If initially (expected = randX) and the top item on the stack
           --  is a prefix operator, this operator is converted to an atom.
           --  This allows for constructions such as (?-) in which a prefix
           --  operator occurs as an atom.

           A: Atom;
       begin
           if Expected = Randx then
               if Stack (Top).Tag /= Opl then
                   Moan (Need_Rand_Error, Syntaxz);
               end if;

               A := Stack (Top).Aval;
               if not (Get_Info(A).Oclass in  Fxo .. Fyo) then
                   Moan (Need_Rand_Error, Syntaxz);
               end if;

               Top := Top - 1;
               Shiftterm (Makefunc (A, 0, null));
           end if;

           if not S (Context) then
               Moan (Bad_Delim_Error, Syntaxz);
           end if;

           Reduce (Maxprec, Maxprec);

           for I in Readstate loop
               S(I) := False;
           end loop;

       end Checkdelim;

       --------------------
       --  Entercontext  --
       --------------------

       procedure Entercontext (K: Readstate; H: Prec) is
           --  Save the current context and set up a new one.
       begin
           if Statetop >= Readdepth then
               Moan (Read_Nest_Error, Syntaxz);
           end if;
           Statetop := Statetop + 1;
           Statestack (Statetop).Scontext := Context;
           Statestack (Statetop).Shiprec  := Hiprec;
           Context := K;
           Expected := Randx;
           Hiprec := H;

       end Entercontext;


       -------------------
       --  Exitcontext  --
       -------------------

       procedure Exitcontext (X: Term) is
           --  Return from an inner context, leaving its value x on the stack.
       begin
           Top := Top - 1;
           Shiftterm(X);
           Context := Statestack(Statetop).Scontext;
           Hiprec := Statestack(Statetop).Shiprec;
           Statetop := Statetop - 1;
           Expected := Opx; Loprec := 0;

       end Exitcontext;

       --------------
       --  Getfunc --
       --------------

       function Getfunc return Term is
           --  Assemble a functor and arguments. On entry, the stack
           --  holds a funcL element, followed by one termL element for
           --  each argument.

           X, Y: Term;
           N: Integer;
       begin
           X := Pop;
           N := 1;
           while Stack(Top).Tag = Terml loop
               Y := Pop;
               Y.Brother := X;
               X := Y;
               N := N + 1;
           end loop;
           return Makefunc(Stack(Top).Aval, N, X);
       end Getfunc;

       --------------
       --  Getlist --
       --------------

       function Getlist return Term is
           --  Assemble a list. On entry, the stack holds a markL
           --  element, followed by a termL element for each list
           --  element, then a termL element for the list continuation.

           X, Y: Term;
       begin
           X := Pop;

           loop
               Y := Pop;
               Y.Brother := X;
               X := Makefunc(Consa, 2, Y);
               exit when Stack(Top).Tag /= Terml;
           end loop;

           return X;
       end Getlist;

       -----------------
       --  Stow_Atom  --
       -----------------

       procedure Stow_Atom(A : Atom) is

           procedure Squashrand(A : Atom) is
               --  Check precedence and reduce a left operand.
               P, Lp: Prec;
           begin
               P := Get_Info(A).Oprec; Lp := Lprec(A);
               if ((Lp < Loprec) or (P > Subprec)) and
                   (not((Context = Outer_K) or 
                        (Context = Inner_K) or (Context = Curlyk))) then

                   Moan (Prec_Error, Syntaxz);
               end if;
               Reduce(P, Lp);
           end Squashrand;

       begin
           case Expected is
               when Randx =>
                   Tkn := Lexan;
                   if Tkn = Leftpar then
                       Shift(Funcl,A);
                       Entercontext(Funck,Subprec);
                       Tkn := Lexan;

                   elsif Get_Info(A).Oclass = Fxo or else
                         Get_Info(A).Oclass = Fyo then

                       if Get_Info(A).Oprec > Hiprec then
                           Moan (Prec_Error, Syntaxz);
                       end if;

                       Shift(Opl,A);
                       Expected := Randx;
                       Hiprec := Rprec(A);
                   else
                       Shiftterm(Makefunc(A,0,null));
                       Expected := Opx;
                       Loprec := 0;
                   end if;

               when Opx =>
                   case Get_Info(A).Oclass is
                       when Xfo | Yfo =>
                           Squashrand(A);
                           Shiftterm(Makefunc(A, 1, Pop));
                           Expected := Opx;
                           Loprec := Get_Info(A).Oprec;
                       when Xfxo | Xfyo | Yfxo =>
                           Squashrand(A);
                           Shift(Opl,A);
                           Expected := Randx;
                           Hiprec := Rprec(A);
                       when Fxo | Fyo | Nono =>
                           Moan (Need_Op_Error, Syntaxz);
                   end case;
                   Tkn := Lexan;
           end case;
       end Stow_Atom;

    begin --  Read_In
        --  Reset_Lexan must be called for each new file to be parsed

        Top := 0;
        Statetop := 0;
        Initvars;
        Push(Markl);
        Context  := Outer_K;
        Expected := Randx;
        Hiprec   := Maxprec;

        Tkn := Lexan;

        loop

            if Tkn = End_Token then
               if Top = 1 then
                   --  End of file - represented by "?- end".

                   Value := Makefunc (Enda, 0, null);
                   Context := Finalk;

               else
                   --  The input has finished, but the parse did
                   --  not manage to produce a complete term.
                   --  We'll call it 'unexpected eof'

                   Moan (Eof_Error, Syntaxz);
               end if;

            else
                case Tkn is
                    when Atom_Token => 
                        Stow_Atom (This_Atom);

                    when Var_Token =>
                        Shiftterm (This_Var);
                        Tkn := Lexan;
                        Expected := Opx;
                        Loprec := 0;

                    when Int_Token =>
                        Shiftterm (Makeint (This_Int));
                        Tkn := Lexan;
                        Expected := Opx;
                        Loprec := 0;

                    when Leftpar =>
                        if Expected = Opx then
                            Moan (Need_Op_Error, Syntaxz);
                        end if;

                        Push (Markl);
                        Entercontext (Inner_K, Maxprec);
                        Tkn := Lexan;

                    when Rightpar =>
                        S := Stateset'(Inner_K | Funck => True,
                              others => False);
                        Checkdelim (S);
                        if Context = Inner_K then
                            Exitcontext (Pop);
                        else
                            Exitcontext (Getfunc);
                        end if;
                        Tkn := Lexan;

                    when Leftsq =>
                        if Expected = Opx then
                            Moan (Need_Op_Error, Syntaxz);
                        end if;
                        Tkn := Lexan;
                        Push(Markl);
                        Entercontext(Listk, Subprec);

                    when Rightsq =>
                        S := Stateset'(Listk | Endlistk => True,
                              others => False);
                        Checkdelim(S);
                        if Context = Listk then
                            Shiftterm(Makefunc(Nila, 0, null));
                        end if;
                        Exitcontext(Getlist);
                        Tkn := Lexan;

                    when Lcurly =>
                        if Expected = Opx then
                            Moan (Need_Op_Error, Syntaxz);
                        end if;
                        Push(Markl);
                        Entercontext(Curlyk,Maxprec);
                        Tkn := Lexan;

                    when Rcurly =>
                        S := Stateset'(Curlyk => True, others => False);
                        Checkdelim(S);
                        Exitcontext(Makefunc(Curlya,1,Pop));
                        Tkn := Lexan;

                    when Comma =>
                        if (Context = Outer_K) or
                           (Context = Inner_K) or
                           (Context = Curlyk) then
                            Stow_Atom(Commaa);
                        else
                            S := Stateset'(Funck | Listk => True, others => False);
                            Checkdelim(S);
                            Expected := Randx;
                            Hiprec := Subprec;
                            Tkn := Lexan;
                        end if;

                    when Vbar =>
                        S := Stateset'(Listk => True, others => False);
                        Checkdelim(S);
                        Context := Endlistk;
                        Expected := Randx;
                        Hiprec := Subprec;
                        Tkn := Lexan;

                    when Fullstop =>
                        S := Stateset'(Outer_K => True, others => False);
                        Checkdelim(S);
                        Context := Finalk;
                        Value := Pop;

                    when List_Token =>
                        if Expected = Opx then
                            Moan (Need_Op_Error,Syntaxz);
                        end if;

                        Shiftterm(This_List);
                        Expected := Opx;
                        Loprec := 0;
                        Tkn := Lexan;

                    when End_Token =>
                        null;
                end case;
            end if;
            exit when Context = Finalk;
        end loop;

        --  ensure we leave the input stream in a consistent state
        Skip_To_Next_Token;
        return Value;
    end Read_In;

end Prolog.Read_In;

--  The grammar is :
--
--      <CLAUSE> ::= <FUNCTOR> | <INTEGER> | <LIST> | <VAR> '.' ;
--
--      <FUNCTOR>::= <ATOM> | <ATOM> ( <CLAUSE> )+ ;
--
--      <ATOM>   ::= ( <SMALL> | ( <SMALL> | <LARGE> | <DIGIT> )* ) |
--                   ( ''' ( <CHARACTER> )* ''' ) |
--                   '[]' | '{K' | ',' | '!' | ';' | 
--                   <ALL SPECIAL CHAR SEQUENCES EXCEPT RESERVED ONES> ;
--
--      <VAR>    ::= ( '_' ( <SMALL> | <LARGE> | <DIGIT> )* ) |
--                   ( <LARGE> ( <SMALL> | <LARGE> | <DIGIT> )* ) |
--                   '_' ;
--
--      <INTEGER>::= ( <DIGIT>* ) | ( '^' <DIGIT>* ) ;
--
--      <LIST>   ::= ( '"' ( <CHARACTER> )* '"' ) | 
--                   '[' ( <CLAUSE> ',')* ']' |
--                   '[' ( <CLAUSE> ( <CLAUSE> )* ) ']' |
--                   <THE 'cons' FORM OF THE LIST> ;
--      
--      <SMALL> ::= 'a' .. 'z' ;
--      <LARGE> ::= 'A' .. 'Z' ;
--      <DIGIT> ::= '0' .. '9' ;
--      <SPACE> ::= '<CRLF>' | ' ' | '<TAB>' ;
--      <SPECIAL> ::= '+' | '-' | '*' | '/' | '^' | 
--                    '<' | '>' | '=' | '`' | '~' |
--                    '\' | ':' | '.' | '?' | '@' |
--                    '#' | '$' | '&' | '%' | '!' ;
--      <CHARACTER> ::= 'All ASCII characters' ;
