
-- A generic implementation dependent package.  This will work for with
-- any machine where all relevant files are in one directory.  Directions
-- to enhance this package to any other machine are given in the body as
-- comments.
--
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Command_Line; use Ada.Command_Line;
--
package body Implementation_Dependent_Routines is

   --
   function Ada_Compiler_And_Machine_Name return String is
   begin
      return "GENERIC IMPLEMENTATION";
      -- insert your Ada compiler and machine name here.
   end Ada_Compiler_And_Machine_Name;
   --

   function Source_Directory return String is
   begin
      return "";
      -- Insert the name of the directory in which all source files exist.
      -- Read access to this directory will be required by the users.
   end Source_Directory;
   --

   function Xform_File_Name_Prefix return String is
   begin
      return "x";
      -- The transformation system creates data files.  What you insert
      -- here will be used as a prefix for the names of all these files.
   end Xform_File_Name_Prefix;
   --

   function Xform_Files_Subdirectory return String is
   begin
     return(".anna_semantics_info");
     -- Returns the subdirectory in the source directory, inside which all
     -- transformer files should be created.
   end Xform_Files_Subdirectory;
   --

   function Anna_Mklib_Done(In_Directory : String) return Boolean is
   -- Returns true if the anna library has been made (ie. anna.mklib
   -- has been done) in the in_directory.
   begin
     --if (file_names.exists(a_strings.to_a(in_directory & "/" & xform_files_subdirectory))) then
     raise Program_Error;
     if False then
       return(True);
     else
       return(False);
     end if;
   end Anna_Mklib_Done;
   --

   function Make_File_Name(Directory,File_Name:String) return String is
   begin
      return File_Name;
      -- Given the directory and file name, this routine must create a
      -- name that uniquely identifies this file.  For example, in UNIX
      -- this function would return DIRECTORY & "/" & FILE_NAME.
   end Make_File_Name;
   --

   function Make_Xform_File_Name(Directory,File_Name:String) return String is
   -- Returns the string corresponding to a file name that can be used to
   -- unambiguously refer to FILE_NAME in DIRECTORY/xform_files_subdirectory.
   begin
     return(Directory & "/" & Xform_Files_Subdirectory & "/" & File_Name);
   end Make_Xform_File_Name;
   --

   function Get_Directory(File_Name:String) return String is
   begin
      return "";
      -- Given a file name, this function must return the name of the
      -- directory in which this file exists.  This name must be able
      -- to identify the directory uniquely if possible.  For example,
      -- in UNIX, this function would do the following:
      --
      --     if the first character of FILE_NAME is not '/' then
      --        Invoke the UNIX routine GETWD to get the current working
      --        directory and prefix this to FILE_NAME.  Store the result
      --        in FILE_NAME (ie. FILE_NAME has a new value now).
      --     end if;
      --     Find I, the index of the last '/' in FILE_NAME.
      --     return FILE_NAME(FILE_NAME'FIRST .. I - 1);
      --
   end Get_Directory;
   --

   function Library_Directory(Directory:String) return Boolean is
   begin
      return False;
      -- Should return TRUE if DIRECTORY corresponds to a library directory,
      -- and returns FALSE otherwise.  In the case of the Verdix compiler
      -- for UNIX, the library directories are usually .../vads*/standard
      -- and .../vads*/verdixlib.
   end Library_Directory;
   --

   function Ada_Path(Directory:String) return String is
   begin
      return "" & Ascii.Nul;
      -- Should return the list of directories in the Ada search list of
      -- DIRECTORY in the form:
      -- "dir1 & null & dir2 & null & . . . & dirn & null"
      -- In the case of the Verdix compiler for UNIX, this information is
      -- available in the file "ada.lib" created by the VADS routines.
   end Ada_Path;
   --

   procedure Quit is
   begin
      Put_Line("QUIT not implemented in this version.");
      -- If the machine allows it, a call to a quit routine should be
      -- inserted here.  The program should be able to terminate
      -- execution gracefully.
   end Quit;
   --

   function Start_Up_Message_File return String is
   begin
      return "messages.sta";
      -- Replace the above filename with the name of the file that contains
      -- start-up messages.
   end Start_Up_Message_File;
   --

   function Message_File return String is
   begin
      return "messages.rep";
      -- Replace the above filename with the name of the file that contains
      -- messages that are displayed everytime the transformer is invoked.
   end Message_File;
   --

   function Cmd_Line_Args return Natural is
   begin
      return Argument_Count;
   end;
   --

   function Cmd_Line_Arg( Num : Natural ) return String is
   begin
      if Num = 0 then
         return Command_Name;
      else
         return Argument (Num);
      end if;
   end;
   --

   function Env_Arg( Arg : String ) return String is
   begin
      raise No_Env_Arg;
      return ""; -- Required for Ada correctness
   end;
   --

   procedure Initialize_Screen is
   begin
      null;
   end;
   --

   procedure Finalize_Screen is
   begin
      null;
   end;
   --

   function Get_Character return Character is
      S : String(1..1);
      Last : Natural;
   begin
      -- Implemented as non-blocking, requiring a <CR>.
      Ada.Text_Io.Get_Line( S, Last );
      return S(1);
   end;
   --

   function Term_Env_Arg return String is
   begin
      return "TERM";
   end;
   --

   function Convert_File return String is
   begin
      return "/usr/anna/spec_analyzer/dist/convert.pro";
   end;
   --

   function Span_Stand_File return String is
   begin
      return "/usr/anna/spec_analyzer/dist/span_stand.pro";
   end;
   
   function Ada_Logic_System_File return String is
   begin
      return "intlibc.pro";
   end;

   function Spawn_Process( Command : String ) return Integer is
   begin
      return 1;
   end;

   function Teach_Directory return String is
   begin
      return "/usr/anna/teach/dist/";
   end;

   function Predefined_Semantics_Directory return String is
   begin
      return "/usr/anna/safe/stnd/dist/.anna_semantics_info/";
   end;

   function Default_Config_File return String is
   begin
      return "/usr/anna/safe/stnd/dist/";
   end;

   function Cpu return Duration is
   begin
      return 0.0;
   end;


   function Standard_Directory return String is
   begin
      return ".";
   end;


end Implementation_Dependent_Routines;
