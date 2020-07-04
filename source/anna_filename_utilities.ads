
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
package Anna_Filename_Utilities is

   function Is_Readable (Name : String) return Boolean;

   function Readable_Or_Moan (Name : String) return String;

   function Is_Createable (Name : String) return Boolean;

   function Createable_Or_Moan (Name : String) return String;

   function Find (Basename         : String;
                  Default_Dir_Name : String;
                  Envv_Name        : String := "") return String;

   --  Looks for a readable text file, checking the following names in order:
   --      basename
   --      $envv_name/basename
   --      default_dir_name/basename
   --
   --  Return the name of the first one it finds, else return "".

   File_Not_Found : exception;

   function Find_Or_Moan (Basename         : String;
                          Default_Dir_Name : String;
                          Envv_Name        : String) return String;
   --  calls find. returns result if /= "", else send an error message
   --  to current_output and raise file_not_found.

end Anna_Filename_Utilities;

