-- Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992 by the Program
-- Analysis and Verification Group, Leland Stanford Junior University.
-- All Rights Reserved.
--
-- This file is part of the Anna tools.  You may copy, modify, and
-- distribute the Anna tools under the conditions described in the Anna
-- General Public License.  A copy of this license should be in a file
-- named COPYING.
--
-- LELAND STANFORD JUNIOR UNIVERSITY ALLOWS FREE USE OF THIS SOFTWARE IN
-- ITS "AS IS" CONDITION.  LELAND STANFORD JUNIOR UNIVERSITY DISCLAIMS
-- ANY LIABILITY OF ANY KIND FOR ANY DAMAGES WHATSOEVER RESULTING FROM
-- THE USE OF THIS SOFTWARE.
----------------------------------------------------------------------
with Prolog.Atom_Table;
with Prolog.Global_Objects;     use Prolog.Global_Objects;
with Prolog.Vars;               use Prolog.Vars;
with Prolog.Garbage_Collection;

package Prolog.Term_Pkg is

    type Predtype is (Normp, Evalp);
    -- Predicate type of atom : normal or evaluable predicate.

    type Cls;
    type Clptr is access Cls;
    -- Incomplete clause definition for use in atom.

    type Optype is (Fxo, Fyo, Xfo, Yfo, Xfxo, Xfyo, Yfxo, Nono);
    -- operator type : ?f?O => infix/postfix/prefix.
    -- X/Y => associativity.

    type Evalpred is
         (Callr,    Cutr,     Readr,    Writer,   Get0r,    Putr,
          Nlr,      Eolnr,    Eofr,     Namer,    Opr,      Abortr,
          Tracer,   Notracer, Atomr,    Integerr, Varr,     Isr,
          Ltr,      Assertar, Assertzr, Retractr, Functorr, Argr,
          Listr,    Nolistr,  Debugr,   Seer,     Seenr,    Tellr,
          Toldr,    Quoter,   Noquoter, Samevarr, Endr);

    -- Evaluable predicates

    Maxevalarity : Integer := 4;
    subtype Evalarity is Integer range 0 .. Maxevalarity;

    -- Arity type of evaluable predicates
    subtype Prec is Integer range 0 .. Maxprec;

    type Atom_Record(Pclass : Predtype) is record
        Oclass : Optype;  -- Operator class
        Oprec  : Prec;    -- Precedence
        Sys    : Boolean; -- System predicate ?

        case Pclass is
            when Normp => Proc : Clptr; -- Pointer to program clause.
            when Evalp =>
                Routine : Evalpred; -- Pointer to Routine
                Arity   : Evalarity; -- and arity.
        end case;
    end record;

    type Atom_Info is access all Atom_Record;
    function Default_Info return Atom_Info;

    package Atom_Pkg is new Atom_Table(Atom_Table_Size,
                                       Hash_Table_Size,
                                       Atom_Info,
                                       Default_Info);
    use Atom_Pkg;

    type Nodetag is (Funct, Intt, Vart, Skelt);
    -- type of node.
    --     funcT : function.
    --     intT  : integer.
    --     varT  : variable.
    --     anonT : anonymous variable.
    --     skelT : a skeleton database term.

    type Field_Type is (Globalf, Localf, Heapf);
    -- Where the node is stored : global stack, local stack or the heap.
    -- globalF > localF > heapF in order of premanence.

    type Node(Tag : Nodetag);
    type Term is access Node;

    type Node(Tag : Nodetag) is record
        Brother : Term;       -- next argument of parent functor, other purposes.
        Field   : Field_Type; -- field.
        Scope   : Integer;    -- various kinds of scope on various stacks.
        Chain   : Term;       -- Next term on global stack.
        case Tag is
            when Funct =>
                Name : Atom;     -- the atom f in f(...).
                Arity : Integer; -- # of arguments
                Son : Term;      -- pointer to the list of argumrnts

            when Intt => Ival : Integer; -- Integer value

            when Vart =>
                Val : Term;      -- value.
                Id  : Varstring; -- the string representing the variable.

            when Skelt => Offset : Integer; -- OFFSET on the stack.
                          St     : Varstring;
                          Anont  : Boolean; -- Anonymous var?
        end case;
    end record;

    type Cls_Type is (Madec, Chainc, Commac);

    type Cls is record
        Head : Term; -- the head of the clause.
        The_Body : Term; -- the tail of the term.
        Typ : Cls_Type;
        Nvars : Integer; -- Number of local vars excluding anons.
        Keyval : Integer; -- key : set by the database.
        Dbase  : Integer; -- The database this clause belongs to.
        Previous : Clptr; -- ptr to previous clause in program.
        Next : Clptr; -- ptr to next clause for program.
    end record;

    subtype Skelt_Node is Node(Skelt);
    subtype Skelt_Term is Term(Skelt);
    package Skel_Garb is new Garbage_Collection(Skelt_Node,Skelt_Term);

    subtype Vart_Node is Node(Vart);
    subtype Vart_Term is Term(Vart);
    package Var_Garb is new Garbage_Collection(Vart_Node,Vart_Term);

    subtype Intt_Node is Node(Intt);
    subtype Intt_Term is Term(Intt);
    package Int_Garb is new Garbage_Collection(Intt_Node,Intt_Term);

    subtype Funct_Node is Node(Funct);
    subtype Funct_Term is Term(Funct);
    package Func_Garb is new Garbage_Collection(Funct_Node,Funct_Term);

    package Clause_Garb is new Garbage_Collection(Cls,Clptr);

    -- Clauses transparent to cut.
    Andg, Or1g, Or2g : Clptr;

    type Integer_Record;
    type Integer_List is access Integer_Record;

    type Integer_Record is record
        I : Integer;
        Next : Integer_List;
    end record;


    type Acc_String is access String;

    type Binding_Type is
        record
            Id : Acc_String;
            C : Term;
        end record;


   type Answer_Record;
   type Solution is access Answer_Record;

   type Answer_Record is
       record
           Value : Binding_Type;
           Next : Solution;
       end record;


end Prolog.Term_Pkg;
