
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


package Implementation_Dependent_Routines is

   ----------------------------------------------------------------
   --  GENERAL PURPOSE ROUTINES
   ----------------------------------------------------------------

   function Cmd_Line_Args return Natural;
   --  Returns number of command line arguments passed to the executable.

   function Cmd_Line_Arg (Num : Natural) return String;
   --  Returns the Num'th command line argument.  The 0th argument is the
   --  program name; "real" parameters begin at 1.

   function Env_Arg (Arg : String) return String;
   --  Returns the value of the environment argument called Arg.

   No_Env_Arg : exception;
   --  Raised by Env_Arg if the environment argument Arg has no value.

   ----------------------------------------------------------------
   --  ROUTINES USED BY THE ANNA TRANSFORMER
   ----------------------------------------------------------------

   function Ada_Compiler_And_Machine_Name return String;
   --  Returns the name of the Ada compiler and machine for which this
   --  package has been implemented.

   function Source_Directory return String;
   --  Returns the name of the directory in which the Transformer source
   --  and data files exist.

   function Xform_File_Name_Prefix return String;
   --  Returns a string that is used as a prefix for all Transformer files.

   function Xform_Files_Subdirectory return String;
   --  Returns the subdirectory in the source directory, inside which all
   --  transformer files should be created.

   function Anna_Mklib_Done (In_Directory : String) return Boolean;
   --  Returns true if the anna library has been made (ie. anna.mklib
   --  has been done) in the in_directory.

   function Make_File_Name (Directory, File_Name : String) return String;
   --  Returns the string corresponding to a file name that can be used to
   --  unambiguously refer to FILE_NAME in DIRECTORY.

   function Make_Xform_File_Name (Directory, File_Name : String) return String;
   --  Returns the string corresponding to a file name that can be used to
   --  unambiguously refer to FILE_NAME in DIRECTORY/xform_files_subdirectory.

   function Get_Directory (File_Name : String) return String;
   --  Returns the directory in which FILE_NAME exists.

   function Library_Directory (Directory : String) return Boolean;
   --  If DIRECTORY corresponds to a predefined Ada library, then this
   --  function returns TRUE.  Otherwise it returns FALSE.

   function Ada_Path (Directory : String) return String;
   --  Returns the Ada search-list of DIRECTORY in the form:
   --  "dir1 & null & dir2 & null & . . . & dirn & null".

   procedure Quit;
   --  Can be called anytime to quit from the program, if this feature is
   --  available in the underlying machine.

   function Start_Up_Message_File return String;
   --  The contents of this file are displayed the first time the transformer
   --  is invoked in a directory.

   function Message_File return String;
   --  The contents of this file are displayed everytime the transformer is
   --  invoked.

   ----------------------------------------------------------------
   --  ROUTINES USED BY THE ADA_LOGIC SYSTEM
   ----------------------------------------------------------------

   function Ada_Logic_System_File return String;
   --  Location of the "intlibc.pro" system file for the Ada_Logic
   --  package.

   ----------------------------------------------------------------
   --  ROUTINES USED BY THE ANNA PACKAGE SPECIFICATION ANALYZER
   ----------------------------------------------------------------

   procedure Initialize_Screen;
   --  Performs any needed screen initialization for the Specification
   --  Analyzer.

   procedure Finalize_Screen;
   --  Performs any needed screen finalization for the Specification
   --  Analyzer.

   function Get_Character return Character;
   --  Gets a character from the keyboard (blocking).

   function Term_Env_Arg return String;
   --  The environment argument for returning terminal type.

   function Convert_File return String;
   --  Location of the "convert.p" file for the specification analyzer.

   function Span_Stand_File return String;
   --  Location of the "span_stand.p" file for the specification analyzer.

   ----------------------------------------------------------------
   --  ROUTINES USED BY ANNA TEACH
   ----------------------------------------------------------------

   function Spawn_Process (Command : String) return Integer;
   --  Spawns Command as a system process for Anna Teach.  Calls to
   --  this are inevitably system dependent; calls which Anna Teach
   --  makes to this function will have to be re-written.  Returns an
   --  integer error code; 0 for success.

   function Teach_Directory return String;
   --  Location of Anna Teach tool.

   function Predefined_Semantics_Directory return String;
   --  Location of the predefined semantics information.

   ----------------------------------------------------------------
   --  ROUTINES USED BY THE ANNA SEMANTIC ANALYZER
   ----------------------------------------------------------------

   function Default_Config_File return String;
   --  Location of the default configuration file.

   function Cpu return Duration;
   --  A non-decreasing representation of CPU time.

   function Standard_Directory return String;
   --  Returns the location of the predefined packages semanticized by
   --  the static checker.

end Implementation_Dependent_Routines;

