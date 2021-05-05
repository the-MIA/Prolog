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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Prolog.Error_Pkg;      use Prolog.Error_Pkg;
with Prolog.Errors;         use Prolog.Errors;
with Prolog.Read_In;        use Prolog.Read_In;


package body Prolog.Input_Output is


   System_Mode : Boolean := False;
   pragma Unreferenced (System_Mode);
   --  Is a system file being seen at present?

   Linelisted : Boolean := False;
   pragma Unreferenced (Linelisted);
   --  Has the current line been listed?

   Maxlength : constant := 200;
   --  Maximum length of an input line and output buffer.

   type Input_File_Array  is array (1 .. Maxindepth)  of Unbounded_String;
   type Output_File_Array is array (1 .. Maxoutdepth) of File_Type;

   Input  : Input_File_Array;
   Output : Output_File_Array;
   pragma Unreferenced (Input);
   --  Input and output files.

   Prolib : File_Type;
   --  Input files for system.

   --  All about input.

   subtype Inrange is Integer range 0 .. Maxlength;

   In_Line_Buffer : Unbounded_String;
   Pos            : Positive := 1;
   --  the 'file' pointer for in_line_buf


   Charpos : array (1 .. Maxindepth) of Inrange := (others => 0);
   pragma Unreferenced (Charpos);
   --  Position in current input line.


   Inlinelength : array (1 .. Maxindepth) of Inrange := (others => 0);
   --  Length of current input line.


   type Acc is access String;
   Remaining_Input  : Acc := null;
   Collected_Output : Acc := null;
   pragma Unreferenced (Remaining_Input);

   subtype Outrange is Natural range 0 .. Rightmargin;

   Outlinelength : array (1 .. Maxoutdepth) of Outrange := (others => 0);
   --  Positions in the current output lines. One value for each file
   --  in the variable "output"

   Templength    : array (1 .. Maxoutdepth) of Outrange := (others => 0);
   --  Position in the output buffer.


   subtype Inbufftype is String (1 .. Maxlength);

   Outlinebuff   : array (1 .. Maxoutdepth) of Inbufftype :=
     (others => Inbufftype'(others => ' '));
   --  The output line.

   Tempbuff      : array (1 .. Maxoutdepth) of Inbufftype :=
     (others => Inbufftype'(others => ' '));
   --  The output buffer.

   Seeflag  : constant Boolean := False;
   Tellflag : Boolean := False;
   --  Is a file being seen/told?


   --  Input files are double buffered so that helpful syntax error messages
   --  can be produced.  Listing of the input text is controlled by the
   --  variable 'listing'.


   --  The input/ output of a line is organized around the global vars:
   --  maxoutdepth which is max depth to which files can be 'told'.
   --  maxindepth which is max depth to which files can be 'seen'.
   --  However changing maxout/indepth is not enough to increase
   --  the depth. All associated procedures in this package have to be
   --  changed.
   --  The input and output lines can be of Maxlength length but
   --  to list errors by printing out lines and put a cursor '^' at
   --  the place of error we should print out long lines in pieces of
   --  the tty width which is the constant RightMargin.
   --  The arrays OutlineBuff and In_Line_Buff hold lines for each of
   --  the out/infiledepth files.
   --  The lengths of these are given in the arrays In/OutlineLength.
   --  To implement the sort of error reporting outlined earlier
   --  the buffer TempLine is used. Notice this only has length rightmargin.
   --  the files output,output1-outfiledepth and input,input1-infiledepth
   --  are used.
   --  linelisted is set when the present line has been listed.
   --  mode is userM,progM,sysM.
   --  This implies user,program(init) or system mode.
   --  online is on when the input is being given online.

   ------------------
   --  Init_Input  --
   ------------------

   procedure Init_Input is
   begin
      In_File_Depth := 1;
      Inlinelength (1) := 0;
      Charpos (In_File_Depth) := 0;
   end Init_Input;

   --  Write out : ------------------------------------------------------

   -------------------
   --  Init_String  --
   -------------------

   procedure Init_String is
   begin
      Collected_Output := null;
   end Init_String;

   ------------------
   --  Get_String  --
   ------------------

   function Get_String return String is
   begin
      return Collected_Output.all;
   end Get_String;

   ----------------
   --  Put_Line  --
   ----------------

   procedure Putline is
      --  simply puts line physically on the output file given by
      --  outfiledepth ands updates Outlinelength.

      Len  : Integer renames Outlinelength (Out_File_Depth);
      Str  : String  renames Outlinebuff (Out_File_Depth);

   begin

      if Out_File_Depth = 1 then
         if Collected_Output = null then
            --  *** dispose (collected_output);
            Collected_Output := new String'(Str (1 .. Len));

         else
            --  *** dispose (collected_output);
            Collected_Output := new String'(Collected_Output.all
                                              & Str (1 .. Len)
                                           );

         end if;
         --  PUT_LINE (OUTLINEBUFF (OUTFILEDEPTH)
         --            (1 .. OUTLINELENGTH (OUTFILEDEPTH)));
      else
         Put_Line (Output (Out_File_Depth), Str (1 .. Len));
      end if;

      Len := 0;

   end Putline;

   ----------------
   --  Wr_Check  --
   ----------------

   procedure Wr_Check is
      --  Checks if tempbuff is overflowing and outputs if it is.
      Len      : Integer renames Outlinelength (Out_File_Depth);
      Temp_Len : Integer renames Templength    (Out_File_Depth);
      Str      : String  renames Outlinebuff   (Out_File_Depth);

   begin
      if Len + Temp_Len > Rightmargin then
         Putline;
      end if;

      Str (Len + 1 ..  Len + Temp_Len) :=
        Tempbuff (Out_File_Depth) (1 .. Temp_Len);

      Len := Len + Temp_Len;

      Temp_Len := 0;

   end Wr_Check;

   -------------
   --  Wr_Ln  --
   -------------

   procedure Wrln is
      --  Output present line
   begin
      Wr_Check;
      Putline;
   end Wrln;

   ----------
   --  Wr  --
   ----------

   procedure Wr (Ch : Character) is
      --  Put character in output buffer
   begin
      --  This should never happen.
      --  if TEMPLENGTH(OUTFILEDEPTH) >= RIGHTMARGIN then
      --     WRLN;
      --  end if;

      Templength (Out_File_Depth) := Templength (Out_File_Depth) + 1;
      Tempbuff   (Out_File_Depth) (Templength (Out_File_Depth)) := Ch;
   end Wr;

   --------------
   --  Wr_Int  --
   --------------

   procedure Wr_Int (N : Integer) is
      --  Put an integer in the output buffer
      Str : constant String := Integer'Image (N);
   begin
      if N < 0 then
         Wr_String (Str);
      else
         --  get rid of the leading space.
         for I in 2 .. Str'Length loop
            Wr (Str (I));
         end loop;
      end if;
   end Wr_Int;

   -----------------
   --  Wr_String  --
   -----------------

   procedure Wr_String (S : String) is
      --  This just outputs a string character by character
   begin
      for I in S'Range loop
         Wr (S (I));
      end loop;
   end Wr_String;

   -----------------
   --  List_Line  --
   -----------------

   procedure Listline is
      --  List the current line.
      --  the input line may be larger than output line, hence the tricks.
      --  max input line length is maxlength
      --  max output line length is rightmargin
   begin
      for I in 1 .. Inlinelength (In_File_Depth) - 1 loop
         if (I mod Rightmargin) = 0 then
            Wrln;
         end if;
         --  write out each time output buffer line i.e.tempbuff becomes full.
         --        Wr (In_Line_Buff (In_File_Depth)(I));
      end loop;
      Wrln;
      Linelisted := True;
      --  Linelisted is a boolean showing whether the present line has been
      --  listed.
   end Listline;
   pragma Unreferenced (Listline);


   --  Read in : -------------------------------------------------------

   ------------------
   --  Set_String  --
   ------------------

   procedure Set_String (S : String) is
   begin
      In_Line_Buffer := To_Unbounded_String (S);

      --  An extra space as yet another hack :-(
      --  Append (In_Line_Buffer , ' ');
      Append (In_Line_Buffer, ASCII.LF);
      Append (In_Line_Buffer, ASCII.NUL);
      Pos := 1;

      Reset_Lexan;

   end Set_String;

   ---------------
   --  Getchar  --
   ---------------

   function Getchar return Character is
      --  Get the next character of the current input file in 'ch'.
      Ch : Character;
   begin

      Ch := Element (In_Line_Buffer, Pos);

      --  Don't advance the pointer if we are at the end
      if Ch /= ASCII.NUL then
         Pos := Pos + 1;
      end if;

      if Debug then
         Put_Line ("->"
                     & Slice (In_Line_Buffer, Pos, Length (In_Line_Buffer))
                     & "<-");
      end if;

      return Ch;

   end Getchar;

   ------------------
   --  Line_Ended  --
   ------------------

   function Line_Ended return Boolean is
      --  Remember charpos points to one less than the next char to be read
   begin
      return Element (In_Line_Buffer, Pos) = ASCII.LF;
   end Line_Ended;

   ------------------
   --  File_Ended  --
   ------------------

   function File_Ended return Boolean is
   begin
      if Debug then
         Put_Line ("FE");
      end if;

      return Element (In_Line_Buffer, Pos) = ASCII.NUL;

   end File_Ended;

   --  File Stuff : -----------------------------------------------------

   procedure Load_File (File : File_Type);
   --  read the contents of file into the variable
   --  In_Line_Buffer.

   --  function Seefile (Filename : String) return Boolean;
   --  A function that is linked into the prolog engine.
   --  Prolog allows multiple nested "sees", but for now
   --  bad luck, only one level, until i can get a proper
   --  version of input working (i.e. inline rep. of
   --  end of line, end of file).


   -----------------
   --  Load_File  --
   -----------------

   procedure Load_File (File : File_Type) is

      Input : String (1 .. 200);
      Last  : Natural;

   begin
      In_Line_Buffer := Null_Unbounded_String;

      while not End_Of_File (File) loop
         Get_Line (File, Input, Last);
         Append (In_Line_Buffer, Input (1 .. Last));
         Append (In_Line_Buffer, ASCII.LF);

      end loop;

      Append (In_Line_Buffer, "   ");
      Append (In_Line_Buffer, ASCII.NUL);

      Pos := 1;
   end Load_File;

   ----------------
   --  See_File  --
   ----------------

   function See_File (Filename : String) return Boolean is
      --  Evaluable predicate 'see'. Open a file for reading.
      File : File_Type;
   begin
      Open (File, In_File, Filename);
      Load_File (File);
      Close (File);

      --  This will _NOT_ work when there are multiple files
      --  Need a better concept of how to manage multiple files
      --  (e.g. maintain a context for each lexar, i.e. make it
      --  an object, with a separate 'Ch' for each Lexar
      Reset_Lexan;

      return True;

   exception
      when Status_Error =>
         Moan (File_Status_Error, Abortz);
         pragma Assert (False);
         raise Program_Error;

      when Name_Error =>
         Moan (File_Name_Error, Abortz);
         pragma Assert (False);
         raise Program_Error;

   end See_File;

   -----------------
   --  Tell_File  --
   -----------------

   function Tell_File (Filename : String) return Boolean is
      --  Evaluable predicate 'tell'. Open a file for writing.
   begin
      Put_Line ("Tell file " & Filename);
      raise Program_Error;
      return True;
   end Tell_File;

   -------------------
   --  Seeing_File  --
   -------------------

   function Seeing_File return Boolean is
   begin
      return Seeflag;
   end Seeing_File;

   --------------------
   --  Telling_File  --
   --------------------

   function Telling_File return Boolean is
   begin
      return Tellflag;
   end Telling_File;

   -------------------
   --  Seen_File  --
   -------------------

   function Seen_File return Boolean is
   begin
      In_Line_Buffer := Null_Unbounded_String;
      Pos := 1;
      return True;
   end Seen_File;

   -----------------
   --  Told_File  --
   -----------------

   function Told_File return Boolean is
   begin
      if Out_File_Depth > 1 then
         Wrln;
         if Is_Open (Output (Out_File_Depth)) then
            Close (Output (Out_File_Depth));
         end if;
         Out_File_Depth := Out_File_Depth - 1;
         Tellflag := Out_File_Depth > 1;
         return True;
      else
         Tellflag := False;
         return False;
      end if;
   end Told_File;

   -----------------------
   --  See_System_File  --
   -----------------------

   function See_System_File (Filename : String) return Boolean is
      --  Open a systemfile for reading.
   begin
      Open (Prolib, In_File, Filename);
      System_Mode := True;
      return True;
   exception
      when others =>
         return False;
   end See_System_File;

   --------------------------
   --  Seeing_System_File  --
   --------------------------

   function Seeing_System_File return Boolean is
   begin
      return Is_Open (Prolib);
   end Seeing_System_File;

   ------------------------
   --  Seen_System_File  --
   ------------------------

   function Seen_System_File return Boolean is
   begin
      if Is_Open (Prolib) then
         Close (Prolib);
         System_Mode := False;
         return True;
      else
         return False;
      end if;
   end Seen_System_File;

begin

   In_File_Depth  := 1;
   Out_File_Depth := 1;
   Outlinelength (Out_File_Depth) := 0;
   Templength (Out_File_Depth)    := 0;

end Prolog.Input_Output;
