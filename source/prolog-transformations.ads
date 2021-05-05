
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

with Prolog.Global_Objects; use Prolog.Global_Objects;
with Prolog.Term_Pkg;       use Prolog.Term_Pkg;
with Prolog.Vars;           use Prolog.Vars;

with Prolog.Local_Stack;

package Prolog.Transformations is

   package Local is new Prolog.Local_Stack (Maxframes, Maxlocsize);
   use Local, Atom_Pkg;

   --  This module contains an assortment of useful functions and procedures
   --  for handling terms and stacks.

   --  Things the interface "inherently" understands.
   --  These (at present) have no associated procedure entries.
   Commaa : constant Atom;
   Consa  : constant Atom;
   Nila   : constant Atom;
   Semia  : constant Atom;
   Arrowa : constant Atom;


   --  Arithmatic predicates. Not evaluable predicates but understood
   --  by evaluable predicates.
   Plusa   : constant Atom;
   Minusa  : constant Atom;
   Timesa  : constant Atom;
   Dividea : constant Atom;
   Moda    : constant Atom;

   Nega    : constant Atom;
   Fxa     : constant Atom;
   Fya     : constant Atom;
   Xfa     : constant Atom;
   Yfa     : constant Atom;

   Xfxa    : constant Atom;
   Xfya    : constant Atom;
   Yfxa    : constant Atom;

   --  The interface has to be told about these by proc entries.
   --  Not evaluable predicates.
   Truea   : constant Atom;
   Faila   : constant Atom;
   Repeata : constant Atom;
   Nota    : constant Atom;
   Equala  : constant Atom;

   Neqa    : constant Atom;
   Eqeqa   : constant Atom;
   Neqeqa  : constant Atom;
   Coleqa  : constant Atom;
   Eqneqa  : constant Atom;

   Gta     : constant Atom;
   Leqa    : constant Atom;
   Geqa    : constant Atom;
   Eqdotdota : constant Atom;

   --  Evaluable predicates.
   Calla    : constant Atom;
   Cuta     : constant Atom;
   Reada    : constant Atom;
   Writea   : constant Atom;
   Get0a    : constant Atom;

   Puta     : constant Atom;
   Nla      : constant Atom;
   Eolna    : constant Atom;
   Eofa     : constant Atom;
   Namea    : constant Atom;

   Opa      : constant Atom;
   Aborta   : constant Atom;
   Tracea   : constant Atom;
   Notracea : constant Atom;
   Atoma    : constant Atom;

   Integera : constant Atom;
   Vara     : constant Atom;
   Isa      : constant Atom;
   Lta      : constant Atom;
   Assertaa : constant Atom;

   Asserta  : constant Atom;
   Retracta : constant Atom;
   Functora : constant Atom;
   Arga     : constant Atom;
   Lista    : constant Atom;

   Nolista  : constant Atom;
   Debuga   : constant Atom;
   Seea     : constant Atom;
   Seena    : constant Atom;
   Tella    : constant Atom;

   Tolda    : constant Atom;
   Quotea   : constant Atom;
   Noquotea : constant Atom;
   Samevara : constant Atom;
   Enda     : constant Atom;

   --  Others (?)
   Tildea   : constant Atom;
   Vbara    : constant Atom;
   Curlya   : constant Atom;


   Glotop   : Term := null;
   Glosize  : Natural := 0;

   function Make_Func (A : Atom; M : Integer; S : Term) return Term;
   function Make_Int (I : Integer) return Term;
   function Make_Var (V : Term; S : Varstring) return Term;

   procedure Killglobal (Newptr : Term);

   function Is_Func (X : Term; A : Atom; M : Integer) return Boolean;
   function Isatom (X : Term) return Boolean;

   function Deref (X : Term; E : Env) return Term;

   procedure Bindvars (V1, V2 : Term);
   procedure Bind (V     : in out Term;
                   X     :        Term;
                   E     :        Env;
                   Depth :        Integer);

   function Listrep (S : String) return Term;

   procedure Getbody (V : in out Term;
                      B :        Term;
                      E :        Env);

private
   Comma_Atom  : aliased Atom_Record := (Normp, Xfyo, 1000, True, null);
   Cons_Atom   : aliased Atom_Record := (Normp, Xfyo, 650, True, null);
   Nil_Atom    : aliased Atom_Record := (Normp, Nono, 0, True, null);
   Semi_Atom   : aliased Atom_Record := (Normp, Xfyo, 1100, True, null);
   Arrow_Atom  : aliased Atom_Record := (Normp, Xfxo, 1200, True, null);

   Plus_Atom   : aliased Atom_Record := (Normp, Yfxo, 500, True, null);
   Minus_Atom  : aliased Atom_Record := (Normp, Yfxo, 500, True, null);
   Times_Atom  : aliased Atom_Record := (Normp, Yfxo, 400, True, null);
   Divide_Atom : aliased Atom_Record := (Normp, Yfxo, 400, True, null);
   Mod_Atom    : aliased Atom_Record := (Normp, Xfxo, 400, True, null);

   Neg_Atom    : aliased Atom_Record := (Normp, Fxo,  0, True, null);
   Fx_Atom     : aliased Atom_Record := (Normp, Nono, 0, True, null);
   Fy_Atom     : aliased Atom_Record := (Normp, Nono, 0, True, null);
   Xf_Atom     : aliased Atom_Record := (Normp, Nono, 0, True, null);
   Yf_Atom     : aliased Atom_Record := (Normp, Nono, 0, True, null);

   Xfx_Atom    : aliased Atom_Record := (Normp, Nono, 0, True, null);
   Xfy_Atom    : aliased Atom_Record := (Normp, Nono, 0, True, null);
   Yfx_Atom    : aliased Atom_Record := (Normp, Nono, 0, True, null);

   True_Atom   : aliased Atom_Record := (Normp, Nono, 0, True, null);
   Fail_Atom   : aliased Atom_Record := (Normp, Nono, 0, True, null);
   Repeat_Atom : aliased Atom_Record := (Normp, Nono, 0, True, null);
   Not_Atom    : aliased Atom_Record := (Normp, Fxo, 800, True, null);
   Equal_Atom  : aliased Atom_Record := (Normp, Xfxo, 700, True, null);

   Neq_Atom    : aliased Atom_Record := (Normp, Xfxo, 700, True, null);
   Eqeq_Atom   : aliased Atom_Record := (Normp, Xfxo, 700, True, null);
   Neqeq_Atom  : aliased Atom_Record := (Normp, Xfxo, 700, True, null);
   Coleq_Atom  : aliased Atom_Record := (Normp, Xfxo, 700, True, null);
   Eqneq_Atom  : aliased Atom_Record := (Normp, Xfxo, 700, True, null);

   Gt_Atom       : aliased Atom_Record := (Normp, Xfxo, 700, True, null);
   Leq_Atom      : aliased Atom_Record := (Normp, Xfxo, 700, True, null);
   Geq_Atom      : aliased Atom_Record := (Normp, Xfxo, 700, True, null);
   Eqdotdot_Atom : aliased Atom_Record := (Normp, Xfxo, 700, True, null);

   Call_Atom    : aliased Atom_Record := (Evalp, Nono, 0, True, Callr, 1);
   Cut_Atom     : aliased Atom_Record := (Evalp, Nono, 0, True, Cutr, 0);
   Read_Atom    : aliased Atom_Record := (Evalp, Nono, 0, True, Readr, 1);
   Write_Atom   : aliased Atom_Record := (Evalp, Nono, 0, True, Writer, 1);
   Get0_Atom    : aliased Atom_Record := (Evalp, Nono, 0, True, Get0r, 1);

   Put_Atom     : aliased Atom_Record := (Evalp, Nono, 0, True, Putr, 1);
   Nl_Atom      : aliased Atom_Record := (Evalp, Nono, 0, True, Nlr, 0);
   Eoln_Atom    : aliased Atom_Record := (Evalp, Nono, 0, True, Eolnr, 0);
   Eof_Atom     : aliased Atom_Record := (Evalp, Nono, 0, True, Eofr, 0);
   Name_Atom    : aliased Atom_Record := (Evalp, Nono, 0, True, Namer, 2);

   Op_Atom      : aliased Atom_Record := (Evalp, Nono, 0, True, Opr, 3);
   Abort_Atom   : aliased Atom_Record := (Evalp, Nono, 0, True, Abortr, 0);
   Trace_Atom   : aliased Atom_Record := (Evalp, Nono, 0, True, Tracer, 0);
   Notrace_Atom : aliased Atom_Record := (Evalp, Nono, 0, True, Notracer, 0);
   Atom_Atom    : aliased Atom_Record := (Evalp, Nono, 0, True, Atomr, 1);

   Integer_Atom : aliased Atom_Record := (Evalp, Nono, 0, True, Integerr, 1);
   Var_Atom     : aliased Atom_Record := (Evalp, Nono, 0, True, Varr, 1);
   Is_Atom      : aliased Atom_Record := (Evalp, Xfxo, 700, True, Isr, 2);
   Lt_Atom      : aliased Atom_Record := (Evalp, Xfxo, 700, True, Ltr, 2);
   Asserta_Atom : aliased Atom_Record := (Evalp, Nono, 0, True, Assertar, 1);

   Assert_Atom  : aliased Atom_Record := (Evalp, Nono, 0, True, Assertzr, 1);
   Retract_Atom : aliased Atom_Record := (Evalp, Nono, 0, True, Retractr, 1);
   Functor_Atom : aliased Atom_Record := (Evalp, Nono, 0, True, Functorr, 3);
   Arg_Atom     : aliased Atom_Record := (Evalp, Nono, 0, True, Argr, 3);
   List_Atom    : aliased Atom_Record := (Evalp, Nono, 0, True, Listr, 0);

   Nolist_Atom  : aliased Atom_Record := (Evalp, Nono, 0, True, Nolistr, 0);
   Debug_Atom   : aliased Atom_Record := (Evalp, Nono, 0, True, Debugr, 0);
   See_Atom     : aliased Atom_Record := (Evalp, Nono, 0, True, Seer, 1);
   Seen_Atom    : aliased Atom_Record := (Evalp, Nono, 0, True, Seenr, 0);
   Tell_Atom    : aliased Atom_Record := (Evalp, Nono, 0, True, Tellr, 1);

   Told_Atom    : aliased Atom_Record := (Evalp, Nono, 0, True, Toldr, 0);
   Quote_Atom   : aliased Atom_Record := (Evalp, Nono, 0, True, Quoter, 0);
   Noquote_Atom : aliased Atom_Record := (Evalp, Nono, 0, True, Noquoter, 0);
   Samevar_Atom : aliased Atom_Record := (Evalp, Nono, 0, True, Samevarr, 0);
   End_Atom     : aliased Atom_Record := (Evalp, Nono, 0, True, Endr, 0);

   --  Others (?)
   Tilde_Atom   : aliased Atom_Record := (Normp, Fyo, 300, True, null);
   Vbar_Atom    : aliased Atom_Record := (Normp, Fyo, 300, True, null);
   Curly_Atom   : aliased Atom_Record := (Normp, Nono, 0, True, null);


   --  Things the interface "inherently" understands.
   --  These (at present) have no associated procedure entries.

   Commaa  : constant Atom := Lookup (", ",       Comma_Atom'Access);
   Consa   : constant Atom := Lookup (".",        Cons_Atom'Access);
   Nila    : constant Atom := Lookup ("nil",      Nil_Atom'Access);
   Semia   : constant Atom := Lookup (";",        Semi_Atom'Access);
   Arrowa  : constant Atom := Lookup (":-",       Arrow_Atom'Access);

   Plusa   : constant Atom := Lookup ("+",        Plus_Atom'Access);
   Minusa  : constant Atom := Lookup ("-",        Minus_Atom'Access);
   Timesa  : constant Atom := Lookup ("*",        Times_Atom'Access);
   Dividea : constant Atom := Lookup ("/",        Divide_Atom'Access);
   Moda    : constant Atom := Lookup ("mod",      Mod_Atom'Access);

   Nega    : constant Atom := Lookup ("^",        Neg_Atom'Access);
   Fxa     : constant Atom := Lookup ("fx",       Fx_Atom'Access);
   Fya     : constant Atom := Lookup ("fy",       Fy_Atom'Access);
   Xfa     : constant Atom := Lookup ("xf",       Xf_Atom'Access);
   Yfa     : constant Atom := Lookup ("yf",       Yf_Atom'Access);

   Xfxa    : constant Atom := Lookup ("xfx",      Xfx_Atom'Access);
   Xfya    : constant Atom := Lookup ("xfy",      Xfy_Atom'Access);
   Yfxa    : constant Atom := Lookup ("yfx",      Yfx_Atom'Access);


   --  the interface has to be told about these by proc entries.
   --  Not evaluable predicates.
   Truea   : constant Atom := Lookup ("true",     True_Atom'Access);
   Faila   : constant Atom := Lookup ("fail",     Fail_Atom'Access);
   Repeata : constant Atom := Lookup ("repeat",   Repeat_Atom'Access);
   Nota    : constant Atom := Lookup ("not",      Not_Atom'Access);
   Equala  : constant Atom := Lookup ("=",        Equal_Atom'Access);

   Neqa    : constant Atom := Lookup ("/=",       Neq_Atom'Access);
   Eqeqa   : constant Atom := Lookup ("==",       Eqeq_Atom'Access);
   Neqeqa  : constant Atom := Lookup ("/==",      Neqeq_Atom'Access);
   Coleqa  : constant Atom := Lookup ("=:=",      Coleq_Atom'Access);
   Eqneqa  : constant Atom := Lookup ("=/=",      Eqneq_Atom'Access);

   Gta     : constant Atom   := Lookup (">",      Gt_Atom'Access);
   Leqa    : constant Atom   := Lookup ("=<",     Leq_Atom'Access);
   Geqa    : constant Atom   := Lookup (">=",     Geq_Atom'Access);
   Eqdotdota : constant Atom := Lookup ("=..",    Eqdotdot_Atom'Access);

   --  Evaluable predicates.
   Calla    : constant Atom := Lookup ("call",    Call_Atom'Access);
   Cuta     : constant Atom := Lookup ("!",       Cut_Atom'Access);
   Reada    : constant Atom := Lookup ("read",    Read_Atom'Access);
   Writea   : constant Atom := Lookup ("write",   Write_Atom'Access);
   Get0a    : constant Atom := Lookup ("get0",    Get0_Atom'Access);

   Puta     : constant Atom := Lookup ("put",     Put_Atom'Access);
   Nla      : constant Atom := Lookup ("nl",      Nl_Atom'Access);
   Eolna    : constant Atom := Lookup ("eoln",    Eoln_Atom'Access);
   Eofa     : constant Atom := Lookup ("eof",     Eof_Atom'Access);
   Namea    : constant Atom := Lookup ("name",    Name_Atom'Access);

   Opa      : constant Atom := Lookup ("op",      Op_Atom'Access);
   Aborta   : constant Atom := Lookup ("abort",   Abort_Atom'Access);
   Tracea   : constant Atom := Lookup ("trace",   Trace_Atom'Access);
   Notracea : constant Atom := Lookup ("notrace", Notrace_Atom'Access);
   Atoma    : constant Atom := Lookup ("atom",    Atom_Atom'Access);

   Integera : constant Atom := Lookup ("integer", Integer_Atom'Access);
   Vara     : constant Atom := Lookup ("var",     Var_Atom'Access);
   Isa      : constant Atom := Lookup ("is",      Is_Atom'Access);
   Lta      : constant Atom := Lookup ("<",       Lt_Atom'Access);
   Assertaa : constant Atom := Lookup ("asserta", Asserta_Atom'Access);

   Asserta  : constant Atom := Lookup ("assert",  Assert_Atom'Access);
   Retracta : constant Atom := Lookup ("retract", Retract_Atom'Access);
   Functora : constant Atom := Lookup ("functor", Functor_Atom'Access);
   Arga     : constant Atom := Lookup ("arg",     Arg_Atom'Access);
   Lista    : constant Atom := Lookup ("list",    List_Atom'Access);

   Nolista  : constant Atom := Lookup ("nolist",  Nolist_Atom'Access);
   Debuga   : constant Atom := Lookup ("$debug",  Debug_Atom'Access);
   Seea     : constant Atom := Lookup ("see",     See_Atom'Access);
   Seena    : constant Atom := Lookup ("seen",    Seen_Atom'Access);
   Tella    : constant Atom := Lookup ("tell",    Tell_Atom'Access);

   Tolda    : constant Atom := Lookup ("told",    Told_Atom'Access);
   Quotea   : constant Atom := Lookup ("quote",   Quote_Atom'Access);
   Noquotea : constant Atom := Lookup ("noquote", Noquote_Atom'Access);
   Samevara : constant Atom := Lookup ("$samev",  Samevar_Atom'Access);
   Enda     : constant Atom := Lookup ("end",     End_Atom'Access);

   --  Others (?)
   Tildea   : constant Atom := Lookup ("~",       Tilde_Atom'Access);
   Vbara    : constant Atom := Lookup ("|",       Vbar_Atom'Access);
   Curlya   : constant Atom := Lookup ("{K",      Curly_Atom'Access);

end Prolog.Transformations;
