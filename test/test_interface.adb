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

--  Tidied up by Dale Stanbrough, but still a rather strange bit of
--  code. You have to understand the packages fairly well before you
--  can use this procedure to drive the software.
--  Not at all suitable for learning how to use the packages.

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Prolog.Ada_Logic;    use Prolog.Ada_Logic;

procedure Test_Interface is

   Nclauses   : constant := 200;
   Nlists     : constant := 200;
   Nsonlists  : constant := 200;
   Nlistlists : constant := 200;

   type Clause_Array    is array  (1 .. Nclauses)   of Clause;
   type List_Array      is array  (1 .. Nlists)     of List_Of_Clauses;
   type Son_Array       is array  (1 .. Nsonlists)  of List_Of_Sons;
   type List_List_Array is array  (1 .. Nlistlists) of List_Of_Lists;

   C   : Clause_Array;
   L   : List_Array;
   S   : Son_Array;
   Lol : List_List_Array;

   Op, Op1, Op2 : Integer;
   Success : Boolean;

   This_Answer : Answer;
   All_Answers : List_Of_Answers;

   ----------------------------------------
   function Get_List (Msg : String := "") return Integer is
      L : Integer range 1 .. Nlists;
   begin
      Put (Msg & " List # (1 .."); Put (Nlists); Put (") :");
      Get (L);
      return L;
   end Get_List;

   ----------------------------------------
   function Get_Son_List (Msg : String := "") return Integer is
      S : Integer range 1 .. Nsonlists;
   begin
      Put (Msg & " SonList # (1 ..");
      Put (Nsonlists);
      Put (") :");
      Get (S);
      return S;
   end Get_Son_List;

   ----------------------------------------
   function Get_List_List (Msg : String := "") return Integer is
      S : Integer range 1 .. Nlistlists;
   begin
      Put (Msg & " ListofLists # (1 ..");
      Put (Nlistlists);
      Put (") :");
      Get (S);
      return S;
   end Get_List_List;

   ----------------------------------------
   function Getclause (Msg : String := "") return Integer is
      C : Integer range 1 .. Nclauses;
   begin
      Put (Msg & " Clause # (1 .."); Put (Nclauses); Put (") :");
      Get (C);
      return C;
   end Getclause;

   ----------------------------------------
   function Getinteger (Msg : String := "") return Integer is
      I : Integer;
   begin
      Put (Msg & " Integer : ");
      Get (I);
      return I;
   end Getinteger;

   ----------------------------------------
   function Getid (Msg : String := "") return Identifier is
      S : String (1 .. 100);
      Len : Integer := 1;
   begin
      Put (Msg & " Identifier : ");
      loop
         Get (S (Len));
         exit when End_Of_Line;
         Len := Len + 1;
      end loop;
      return new String'(S (1 .. Len));
   end Getid;

   ----------------------------------------
   procedure Write_List (L : List_Of_Clauses) is
   begin
      for I in 1 .. Length (L) loop
         Put (I); Put (" : ");
         Put (Dump_Clause (Get_Item (L, I)).all);
         Put_Line (".");
      end loop;
   end Write_List;

   ----------------------------------------
   procedure Write_Son_List (L : List_Of_Sons) is
   begin
      for I in 1 .. Length (L) loop
         Put (I); Put (" : ");
         Put (Dump_Clause (Get_Item (L, I)).all);
         Put_Line (".");
      end loop;
   end Write_Son_List;

   ----------------------------------------
   procedure Write_List_List (L : List_Of_Lists) is
   begin
      for I in 1 .. Length (L) loop
         Put (I); Put_Line (" : ");
         Write_List (Get_Item (L, I));
         New_Line;
      end loop;
   end Write_List_List;

begin
   Put_Line ("Welcome to the test interface program");
   New_Line;
   loop
      begin
         Put_Line ("Operations on Atoms:");
         Put_Line ("BuildInteger-2,    BuildId-3,  AttrKind-4");
         Put_Line ("IntegerAttr-5,     IdAttr-6");
         New_Line;
         New_Line;

         Put_Line ("Operations on Lists of Clauses:");
         New_Line;
         Put_Line ("CreateList-1,      Append-7,       CopyList-8,    " &
                     "GetItem-9");
         Put_Line ("DeleteItem-10,     DestroyList-11, ListLength-12");
         New_Line;
         New_Line;

         Put_Line ("Operations on Lists of Sons:");
         New_Line;
         Put_Line ("AppendSon-13,      CopySonList-14,   GetSonItem-15");
         Put_Line ("DeleteSonItem-16,  DestroySonList-17,");
         Put_Line ("SonListLength-18,  CreateSonList-41");
         New_Line;
         New_Line;


         Put_Line ("Operations on Clauses:");
         New_Line;
         Put_Line ("CopyClause-19,     DestroyClause-51,");
         Put_Line ("SetSon-20,         GetSon-21,        Arity-22,  " &
                     "IsRule-23,");
         Put_Line ("BuildRule-24,      GetHead-25,       GetTail-26,");
         Put_Line ("ReadClause-27,     DumpClause-28,");
         New_Line;
         New_Line;


         Put_Line ("Operations on Files:");
         New_Line;
         Put_Line ("ReadFile-29,        WriteFile-30,");
         Put_Line ("ReadFastFile-39,    WriteFastFile-40,");
         New_Line;
         New_Line;

         Put_Line ("Operations on Answers:");
         New_Line;
         Put_Line ("AnswerListLen-31,   GetAnswer-32,");
         Put_Line ("AnsLen-33,          GetBinding-34");
         Put_Line ("FirstQuery-36,      NextQuery-37,      Query-38,");
         Put_Line ("ListQuery-49,       ListFirstQuery-50, ListNextQuery-37");
         New_Line;
         New_Line;

         Put_Line ("Operations on Lists of Lists:");
         New_Line;
         Put_Line ("CreateListList-42,  AppendListList-43,");
         Put_Line ("CopyListList-44,    GetListItem-45,    " &
                     "DeleteListItem-46,");
         Put_Line ("DestroyListList-47, ListListLength-48,");
         New_Line;
         New_Line;

         Put ("Exit-(all else there is) : ");
         Get (Op);

         case Op is
            when 1 =>
               Op1 := Get_List ("Created");
               L (Op1) := Create;
               Put_Line ("***LIST :***");
               Write_List (L (Op1));

            when 2 =>
               Op1 := Getclause ("Created");
               C (Op1) := Build_Fact (Getinteger);
               Put_Line ("^^^CLAUSE :^^^" & Dump_Clause (C (Op1)).all & ".");

            when 3 =>
               Op1 := Getclause ("Created");
               C (Op1) := Build_Fact (Getid);
               Put_Line ("^^^CLAUSE :^^^" & Dump_Clause (C (Op1)).all & ".");

            when 4 =>
               Put_Line ("Attribute : " &
                           Attribute_Kind_Type'Image
                             (Attribute_Kind (C (Getclause))));

            when 5 =>
               Put_Line ("Integer : " &
                           Integer'Image
                             (Integer_Attribute (C (Getclause))));

            when 6 => Put_Line ("Id : " &
                                  Identifier_Attribute (C (Getclause)).all);

            when 7 => Put ("What kind of Append (CL-1,LC-2,LL-else) ? ");
               Get (Op1);
               case Op1 is
                  when 1 =>
                     Op2 := Getclause;
                     Op1 := Get_List;
                     Append (C (Op2), L (Op1));
                  when 2 =>
                     Op1 := Get_List;
                     Op2 := Getclause;
                     Append (L (Op1), C (Op2));
                  when others =>
                     Op1 := Get_List ("First");
                     Op2 := Get_List ("Second");
                     Append (L (Op1), L (Op2));
               end case;
               Put_Line ("***LIST :***");
               Write_List (L (Op1));

            when 8 =>
               Op1 := Get_List ("To");
               Copy (L (Get_List ("From")), L (Op1));
               Put_Line ("***LIST :***");
               Write_List (L (Op1));

            when 9 =>
               Op1 := Getclause ("Output");
               Copy_Clause (Get_Item (L (Get_List), Getinteger), C (Op1));
               Put_Line ("^^^CLAUSE :^^^" & Dump_Clause (C (Op1)).all & ".");

            when 10 =>
               Op1 := Get_List;
               Delete_Item (L (Op1), Getinteger);
               Put_Line ("***LIST :***");
               Write_List (L (Op1));

            when 11 =>
               Op1 := Get_List;
               Destroy (L (Op1));
               Put_Line ("***LIST :***");
               Write_List (L (Op1));

            when 12 =>
               Put (Length (L (Get_List)));
               New_Line;

            when 13 =>
               Put ("What kind of Append (CS-1,SC-2,SS-else) ? ");
               Get (Op1);
               case Op1 is
                  when 1 =>
                     Op2 := Getclause;
                     Op1 := Get_Son_List;
                     Append (C (Op2), S (Op1));
                  when 2 =>
                     Op1 := Get_Son_List;
                     Op2 := Getclause;
                     Append (S (Op1), C (Op2));
                  when others =>
                     Op1 := Get_Son_List ("First");
                     Op2 := Get_Son_List ("Second");
                     Append (S (Op1), S (Op2));
               end case;
               Put_Line ("+++SONLIST :+++");
               Write_Son_List (S (Op1));

            when 14 =>
               Op1 := Get_Son_List ("To");
               Copy (S (Get_List ("From")), S (Op1));
               Put_Line ("+++SONLIST :+++");
               Write_Son_List (S (Op1));

            when 15 =>
               Op1 := Getclause ("Output");
               Copy_Clause (Get_Item (S (Get_List), Getinteger), C (Op1));
               Put_Line ("^^^CLAUSE :^^^" & Dump_Clause (C (Op1)).all & ".");

            when 16 =>
               Op1 := Get_Son_List;
               Delete_Item (S (Op1), Getinteger);
               Put_Line ("+++SONLIST :+++");
               Write_Son_List (S (Op1));

            when 17 =>
               Op1 := Get_List;
               Destroy (L (Op1));
               Put_Line ("+++SONLIST :+++");
               Write_Son_List (S (Op1));

            when 18 =>
               Put (Length (S (Get_Son_List))); New_Line;

            when 19 =>
               Op1 := Getclause ("To");
               Copy_Clause (C (Getclause ("From")), C (Op1));
               Put_Line ("^^^CLAUSE :^^^" & Dump_Clause (C (Op1)).all & ".");

            when 20 =>
               Op1 := Getclause;
               Set_Son_List (C (Op1), S (Get_Son_List));
               Put_Line ("^^^CLAUSE :^^^" & Dump_Clause (C (Op1)).all & ".");

            when 21 =>
               Op1 := Get_Son_List;
               S (Op1) := Get_Son_List (C (Getclause));
               Put_Line ("+++SONLIST :+++");
               Write_Son_List (S (Op1));

            when 22 =>
               Put (Arity (C (Getclause))); New_Line;

            when 23 =>
               if Is_Rule (C (Getclause)) then
                  Put_Line ("Yes.");
               else
                  Put_Line ("No.");
               end if;

            when 24 =>
               Op1 := Getclause ("Output");
               C (Op1) := Build_Rule (C (Getclause ("Head")),
                                     S (Get_Son_List ("Tail")));
               Put_Line ("^^^CLAUSE :^^^" & Dump_Clause (C (Op1)).all & ".");

            when 25 =>
               Op1 := Getclause ("Output Head");
               C (Op1) := Get_Head (C (Getclause ("Input")));
               Put_Line ("^^^CLAUSE :^^^" & Dump_Clause (C (Op1)).all & ".");

            when 26 =>
               Op1 := Get_Son_List ("Output Tail");
               S (Op1) := Get_Tail (C (Getclause ("Input")));
               Put_Line ("+++SONLIST :+++");
               Write_Son_List (S (Op1));

            when 27 =>
               Op1 := Getclause ("Output");
               C (Op1) := Read_Clause (Getid ("Input"));
               Put_Line ("^^^CLAUSE :^^^" & Dump_Clause (C (Op1)).all & ".");

            when 28 =>
               Put_Line (Dump_Clause (C (Getclause)).all);

            when 29 =>
               Op1 := Get_List ("Output");
               L (Op1) := Read_File (Getid ("FileName").all);
               Put_Line ("***LIST :***");
               Write_List (L (Op1));

            when 30 =>
               Write_File (Getid ("FileName").all,  L (Get_List ("From")));

            when 35 =>
               Op1 := Get_Son_List ("Created");
               S (Op1) := Create;
               Put_Line ("+++SONLIST :+++");
               Write_Son_List (S (Op1));

            when 31 =>
               Put (Length (All_Answers));
               New_Line;

            when 32 =>
               This_Answer := Get_Answer (All_Answers,
                                          Getinteger ("Answer #"));
               Put_Line ("Answer now selected.");

            when 33 =>
               Put (Length (This_Answer)); New_Line;

            when 34 =>
               Put_Line (Dump_Clause (Get_Binding (This_Answer,
                                                   Getid ("Var"))).all);

            when 36 =>
               Query (C (Getclause ("Query")),
                      L (Get_List ("Dbase")),
                      Success,
                      This_Answer);
               if Success then
                  Put_Line ("Query Succeeded.");
               else
                  Put_Line ("Query Failed.");
               end if;

            when 37 =>
               Next_Answer (Success,  This_Answer);
               if Success then
                  Put_Line ("Query Succeeded.");
               else
                  Put_Line ("Query Failed.");
               end if;

            when 38 =>
               Query (C (Getclause ("Query")),
                      L (Get_List ("Dbase")),
                      Success,
                      All_Answers);
               if Success then
                  Put_Line ("Query Succeeded.");
               else
                  Put_Line ("Query Failed.");
               end if;

            when 39 =>
               Op1 := Get_List ("Output");
               L (Op1) := Read_Fast_File (Getid ("FileName").all);
               Put_Line ("***LIST :***");
               Write_List (L (Op1));

            when 40 =>
               Write_Fast_File (Getid ("FileName").all,
                                L (Get_List ("From")));

            when 41 =>
               S (Get_Son_List) := Create;

            when 42 =>
               Lol (Get_List_List) := Create;

            when 43 =>
               Put ("What kind of Append (LLi-1,LiL-2,LiLi-else) ? ");
               Get (Op1);
               case Op1 is
                  when 1 =>
                     Op2 := Get_List;
                     Op1 := Get_List_List;
                     Append (L (Op2), Lol (Op1));
                  when 2 =>
                     Op1 := Get_List_List;
                     Op2 := Get_List;
                     Append (Lol (Op1), L (Op2));
                  when others =>
                     Op1 := Get_List_List ("First");
                     Op2 := Get_List_List ("Second");
                     Append (Lol (Op1), Lol (Op2));
               end case;
               Put_Line ("$$$LIST OF LISTS :$$$");
               Write_List_List (Lol (Op1));

            when 44 =>
               Op1 := Get_List_List ("To");
               Copy (Lol (Get_List_List ("From")), Lol (Op1));
               Put_Line ("$$$LIST OF LISTS :$$$");
               Write_List_List (Lol (Op1));

            when 45 =>
               Op1 := Get_List ("Output");
               Copy (Get_Item (Lol (Get_List_List), Getinteger), L (Op1));
               Put_Line ("***LIST :***");
               Write_List (L (Op1));

            when 46 =>
               Op1 := Get_List_List;
               Delete_Item (Lol (Op1), Getinteger);
               Put_Line ("$$$LIST OF LISTS :$$$");
               Write_List_List (Lol (Op1));

            when 47 =>
               Op1 := Get_List_List;
               Destroy (Lol (Op1));
               Put_Line ("$$$LIST OF LISTS :$$$");
               Write_List_List (Lol (Op1));

            when 48 =>
               Put (Length (Lol (Get_List_List)));
               New_Line;

            when 49 =>
               Query (C (Getclause ("Query")),
                      Lol (Get_List_List ("Dbase")),
                      Success,
                      All_Answers);
               if Success then
                  Put_Line ("Query Succeeded.");
               else
                  Put_Line ("Query Failed.");
               end if;

            when 50 =>
               Query (C (Getclause ("Query")),
                      Lol (Get_List_List ("Dbase")),
                      Success,
                      This_Answer);
               if Success then
                  Put_Line ("Query Succeeded.");
               else
                  Put_Line ("Query Failed.");
               end if;

            when 51 =>
               Op1 := Getclause ("Destroy");
               Destroy_Clause (C (Op1));

            when others =>
               exit when True;
         end case;

      exception
         when others =>
            Put_Line ("Exception occured");
            Skip_Line;
      end;
   end loop;
   Put_Line ("Exiting Logic-I/O.");

end Test_Interface;
