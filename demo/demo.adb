with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Prolog.Ada_Logic;    use Prolog.Ada_Logic;
with Prolog.Errors;       use Prolog.Errors;


procedure Demo is

   procedure Write_List (L : List_Of_Clauses);
   --  Display the list, one element per line

   procedure Write_List (L : List_Of_Clauses) is
   begin
      for I in 1 .. Length (L) loop
         Put (I, Width => 3); Put (" : ");
         Put (Dump_Clause (Get_Item (L, I)).all);
         Put_Line (".");
      end loop;
   end Write_List;


   ----------------------------------------

   Q      : Clause;
   Result : Clause;
   L      : List_Of_Clauses;

   Error   : Error_Type;
   Answers : List_Of_Answers;
   Ans     : Answer;

   Success : Boolean;


   File  : File_Type;

   Filename     : String (1 .. 100);
   Query_String : String (1 .. 100);

   Last  : Natural;


begin
   Put ("Clause file ? ");
   Get_Line (Filename, Last);


   --  Load the list of facts & rules from the file
   L := Read_File (Filename (1 .. Last));

   --  show the user what was read in
   Write_List (L);

   loop
      Put ("Enter a search query (with an unbound X)...");
      Get_Line (Query_String, Last);


      --  the parser dies if there is no trailing full stop
      --  As this is such a common error, we'll insert one
      --  search backwards to see if one is there

      while Last > 0 and then Query_String (Last) = ' ' loop
         Last := Last - 1;

      end loop;

      exit when Last = 0;

      if Query_String (Last) /= '.' then
         Last := Last + 1;
         Query_String (Last) := '.';
      end if;

      Put ("Query string is '" & Query_String (1 .. Last) & ''');


      --  Convert the input into a query
      Q := Read_Clause (Query_String (1 .. Last));


      --  Apply the query to the List of clauses
      Query (Q, L, Success, Answers);

      if Success then
         Put_Line ("Query Succeeded.");
      else
         Put_Line ("Query Failed.");
      end if;

      Put ("Length of answer list is ");
      Put (Length (Answers));
      New_Line;

      --  Examine each response, and display it to the screen

      for i in 1 .. Length (Answers) loop
         Ans := Get_Answer (Answers, i);

         Result := Get_Binding (Ans, "X");
         Put (Dump_Clause (Result).all);
         New_Line;
      end loop;

   end loop;

exception
   when Name_Error =>
      Put ("Couldn't open that file");
      New_Line;

   when others =>
      Error := Prolog.Errors.Kind_Of_Error;

      Put (Error_Type'Image (Error));
      Put (", ");
      Put (Help_Error (Error));
      New_Line;

end Demo;
