
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

--  This generic package deals with physical input and output to the
--  terminal and to files.
--  There are two 'modes' of input/output : the user mode and the system mode.
--  These two modes are exclusive, i.e. No user file can be accessed in the
--  system mode and vice versa.
--  The intended mode of use is :
--         SEE (SystemFile) ... SEEN ... SEE(UserFile) ... SEEN ...
--
--  The User files are pushed on a stack as new files are seen.
--  When a file is finished with it is popped off the stack and
--  the previous file now becomes the current file.
--  In system mode, the system file is the current file.


--  Possible errors raised : IN_FILE_DEPTH_ERROR
--                           OUT_FILE_DEPTH_ERROR
--                           FILE_STATUS_ERROR
--                           FILE_NAME_ERROR

with Prolog.Global_Objects; use Prolog.Global_Objects;

package Prolog.Input_Output is

   --  File Stuff :

   In_File_Depth  : Integer range 1 .. Maxindepth := 1;
   Out_File_Depth : Integer range 1 .. Maxoutdepth := 1;
   --  The current depth.

   function See_System_File (Filename : String) return Boolean;
   --  Open the system file 'filename' and connect to it.

   function See_File  (Filename : String) return Boolean;
   function Tell_File (Filename : String) return Boolean;
   --  Change depth by 1 and connect to this file.

   --  Seeing (Filename) and Telling(filename) are not implemented.
   --  The functions below give some idea about whether a file is
   --  being seen/told.

   function Seeing_System_File return Boolean;
   --  Is the system file being read?

   function Seeing_File return Boolean;
   --  Is a file being 'seen' at present?

   function Telling_File return Boolean;
   --  Is a file being 'told' at present?

   function Seen_System_File return Boolean;
   --  Close the system file.

   function Seen_File return Boolean;
   function Told_File return Boolean;
   --  Change depth back by 1 and connect to previous file.
   --  The functions return true if a file was actually being seen/told.

   --  Write out :

   Rightmargin : Natural := 78;
   --  The number of characters written on an output line before a newline
   --  is automatically written.

   Listing : Boolean;
   --  Listing of the input text is controlled by the
   --  variable 'listing'.

   procedure Wr_Check;
   --  Check if buffer full and output.

   procedure Wr_Ln;
   --  <newline> and output buffer.

   procedure Wr (Ch : Character);
   --  Put character in buffer.

   procedure Wr_Int (N : Integer);
   --  Put integer in buffer.

   procedure Wr_String (S : String);
   --  Put string in buffer.

   --  Read in :

   procedure Init_Input;

   function Get_Char return Character;
   --  Get character from input buffer.

   function Line_Ended return Boolean;
   --  Is next character the end-of-line character?

   function File_Ended return Boolean;
   --  Is next character the end-of-file character?

   procedure Set_String (S : String);
   --  Sets the input buffer to the string entered.
   --  Parsing can then work on this string

   procedure Init_String;
   --  Sets the Collected_Output (ptr to string) to null.

   function Get_String return String;
   --  Returns the Collected_Output string

end Prolog.Input_Output;
