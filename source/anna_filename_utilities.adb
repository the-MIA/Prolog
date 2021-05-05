
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
with Ada.Text_IO;     use Ada.Text_IO;
with Implementation_Dependent_Routines;

package body Anna_Filename_Utilities is

   function Is_Readable (Name : String) return Boolean is
      F : File_Type;
   begin
      Open (F, In_File, Name);
      Close (F);
      return True;
   exception
      when others =>
         return False;
   end Is_Readable;

   function Readable_Or_Moan (Name : String) return String is
   begin
      if Is_Readable (Name) then
         return Name;
      else
         Put_Line ("*** Cannot open " & Name & " for input.");
         raise File_Not_Found;
      end if;
   end Readable_Or_Moan;

   function Is_Createable (Name : String) return Boolean is
      F : File_Type;
   begin
      Create (F, Out_File, Name);
      Delete (F);
      return True;
   exception
      when others =>
         return False;
   end Is_Createable;


   function Createable_Or_Moan (Name : String) return String is
   begin
      if Is_Createable (Name) then
         return Name;
      else
         Put_Line ("*** Cannot create " & Name & '.');
         raise File_Not_Found;
      end if;
   end Createable_Or_Moan;

   function Find (Basename         : String;
                  Default_Dir_Name : String;
                  Envv_Name        : String := "") return String
   is
      package Id renames Implementation_Dependent_Routines;
   begin
      if Is_Readable (Basename) then
         return Basename;
      end if;

      begin
         if
           Envv_Name /= "" and then
           Is_Readable (Id.Env_Arg (Envv_Name) & '/' & Basename)
         then
            return Id.Env_Arg (Envv_Name) & '/' & Basename;
         end if;
      exception
         when Id.No_Env_Arg => null;
      end;

      if Is_Readable (Default_Dir_Name & '/' & Basename) then
         return Default_Dir_Name & '/' & Basename;
      else
         return "";
      end if;
   end Find;

   function Find_Or_Moan (Basename         : String;
                          Default_Dir_Name : String;
                          Envv_Name        : String) return String
   is
      Result : constant String := Find (Basename, Default_Dir_Name, Envv_Name);
   begin
      if Result = "" then
         Put_Line ("*** Cannot find " & Basename & '.');
         raise File_Not_Found;
      else
         return Result;
      end if;
   end Find_Or_Moan;

end Anna_Filename_Utilities;
