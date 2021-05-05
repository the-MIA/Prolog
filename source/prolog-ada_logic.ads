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

with Prolog.Term_Pkg;      use Prolog.Term_Pkg;

package Prolog.Ada_Logic is


   type Attribute_Kind_Type is  (Int, Const, Var, Anon);

   type Identifier is access all String;


   type Clause is private;
   type List_Of_Clauses is private;
   type List_Of_Sons is private;
   type List_Of_Lists is private;

   No_Sons      : exception;
   Attr_Error   : exception;
   Not_Bound    : exception;
   Out_Of_Range : exception;

   function Attribute_Kind (C : Clause) return Attribute_Kind_Type;

   function Integer_Attribute (C : Clause) return Integer;
   function Identifier_Attribute (C : Clause) return Identifier;

   function Build_Fact (Attr : Integer) return Clause;
   function Build_Fact (Attr : Identifier) return Clause;
   function Build_Anon return Clause;

   procedure Destroy_Clause (C : in out Clause);

   function Arity         (C : Clause) return Natural;
   function Get_Son_List  (C : Clause) return List_Of_Sons;
   procedure Set_Son_List (C : in out Clause; Sons : List_Of_Sons);

   function Get_Head (C : Clause) return Clause;
   function Get_Tail (C : Clause) return List_Of_Sons;
   function Is_Rule  (C : Clause) return Boolean;
   function Build_Rule (
      Head_Of_Clause : Clause;
      Tail           : List_Of_Sons) return Clause;

   procedure Copy_Clause (C1 : Clause; C2 : out Clause);

   function Length (L : List_Of_Clauses) return Natural;
   function Length (L : List_Of_Sons) return Natural;
   function Length (L : List_Of_Lists) return Natural;

   function Create return List_Of_Clauses;
   function Create return List_Of_Sons;
   function Create return List_Of_Lists;

   function Get_Item (L : List_Of_Clauses; N : Positive) return Clause;
   function Get_Item (L : List_Of_Sons;    N : Positive) return Clause;
   function Get_Item (L : List_Of_Lists;
                      N : Positive) return List_Of_Clauses;

   procedure Copy (L1 : List_Of_Clauses; L2 : out List_Of_Clauses);
   procedure Copy (L1 : List_Of_Sons;    L2 : out List_Of_Sons);
   procedure Copy (L1 : List_Of_Lists;   L2 : out List_Of_Lists);

   procedure Append (C : Clause;          L : in out List_Of_Clauses);
   procedure Append (C : Clause;          L : in out List_Of_Sons);
   procedure Append (C : List_Of_Clauses; L : in out List_Of_Lists);

   procedure Append (L : in out List_Of_Clauses; C : Clause);
   procedure Append (L : in out List_Of_Sons;    C : Clause);
   procedure Append (L : in out List_Of_Lists;   C : List_Of_Clauses);

   procedure Append (L1 : in out List_Of_Clauses; L2 : List_Of_Clauses);
   procedure Append (L1 : in out List_Of_Sons;    L2 : List_Of_Sons);
   procedure Append (L1 : in out List_Of_Lists;   L2 : List_Of_Lists);

   procedure Delete_Item (L : in out List_Of_Clauses; N : Positive);
   procedure Delete_Item (L : in out List_Of_Sons;    N : Positive);
   procedure Delete_Item (L : in out List_Of_Lists;   N : Positive);

   procedure Destroy (L : in out List_Of_Clauses);
   procedure Destroy (L : in out List_Of_Sons);
   procedure Destroy (L : in out List_Of_Lists);

   function Read_Clause (Text : Identifier) return Clause;
   function Read_Clause (Text : String) return Clause;
   function Dump_Clause (Tree : Clause) return Identifier;

   function Read_File (File : String) return List_Of_Clauses;
   function Read_Fast_File (File : String) return List_Of_Clauses;

   procedure Write_File      (File : String; List : List_Of_Clauses);
   procedure Write_Fast_File (File : String; List : List_Of_Clauses);



   type Answer is private;

   function Length      (Ans : Answer) return Integer;
   function Get_Binding (Ans : Answer; Var : String) return Clause;
   function Get_Binding (Ans : Answer; Var : Identifier) return Clause;

   type List_Of_Answers is private;

   function Length     (L : List_Of_Answers) return Integer;
   function Get_Answer (L : List_Of_Answers; N : Integer) return Answer;

   procedure Query (Question  : in     Clause;
                    Based_On  : in     List_Of_Clauses;
                    Success   :    out Boolean;
                    Solutions :    out List_Of_Answers);

   procedure Query (Question  : in     Clause;
                    Based_On  : in     List_Of_Clauses;
                    Success   :    out Boolean;
                    Bindings  :    out Answer); --  One single answer.

   procedure Next_Answer (Success : out Boolean; Bindings : out Answer);

   procedure Query (Question  : in     Clause;
                    Based_On  : in     List_Of_Lists;
                    Success   :    out Boolean;
                    Solutions :    out List_Of_Answers);

   procedure Query (Question  : in     Clause;
                    Based_On  : in     List_Of_Lists;
                    Success   :    out Boolean;
                    Bindings  :    out Answer); --  One single answer.

   --  procedure Next_Answer (Success : out Boolean; Bindings : out Answer);

   --  Interrupts: ^C

   User_Interrupt : exception;

private
   type Clause_List_Record;
   type Clause_List is access Clause_List_Record;
   type List_Of_Clauses_Record;
   type List_Of_Clauses is access List_Of_Clauses_Record;

   type List_Of_Clauses_Record is record
       Number : Integer;
       List   : Clause_List;
       Arity  : Integer := 0;
   end record;

   type List_Of_Sons is record
       Arity : Integer;
       List  : Term;
   end record;

   type List_List_Record;
   type List_List is access List_List_Record;

   type List_Of_Lists is record
       Int_List : Integer_List;
       List     : List_List;
       Arity    : Integer := 0;
   end record;

   type Clause is new Clptr;
   type Answer is new Solution;
   type Answer_List_Record;
   type List_Of_Answers is access Answer_List_Record;

   type Answer_List_Record is
       record
           Value : Answer;
           Next  : List_Of_Answers;
       end record;

end Prolog.Ada_Logic;
