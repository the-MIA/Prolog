
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

package body Prolog.Errors is

   Err : Error_Type;

   type Acc_String is access constant String;


   Arity_Str          : aliased constant String := "evaluable predicate called with wrong arity";
   Assert_Str         : aliased constant String := "asserting unsuitable term";
   Atom_Space_Str     : aliased constant String := "out of atom space";
   Bad_Cdd_Str        : aliased constant String := ".   expected";
   Bad_Char_Str       : aliased constant String := "character value out of range";
   Bad_Delim_Str      : aliased constant String := "unexpected delimiter";
   Bad_Exp_Str        : aliased constant String := "malformed expression";
   Call_Str           : aliased constant String := "unsuitable arguments to  'call'";
   Clause_Str         : aliased constant String := "unsuitable arguments to 'clause' /'deny'";
   Comment_Str        : aliased constant String := "unterminated comment";

   Conflict_Str       : aliased constant String := "precedence conflict";
   Depth_Str          : aliased constant String := "nesting too deep: probably cyclic term";
   Divide_Str         : aliased constant String := "dividing by zero";
   Eof_Str            : aliased constant String := "unexpected end of file";
   Frame_Space_Str    : aliased constant String := "out of frame space";
   Functor_Str        : aliased constant String := "unsuitable arguments to  'functor'";
   Goal_Str           : aliased constant String := "unsuitable term appears as goal";
   Local_Space_Str    : aliased constant String := "out of local stack space";
   Long_Line_Str      : aliased constant String := "input line too long";
   Name_Arg_Str       : aliased constant String := "unsuitable arguments to  'name'";

   Need_Op_Str        : aliased constant String := "infix or postfix operator expected";
   Need_Quote_Str     : aliased constant String := "closing quote expected";
   Need_Rand_Str      : aliased constant String := "operand or prefix operator expected";
   Nvars_Str          : aliased constant String := "out of variable table space";
   Op_Str             : aliased constant String := "unsuitable arguments to ''op'";
   Prec_Str           : aliased constant String := "precedence violation";
   Prog_Fail_Str      : aliased constant String := "goal failed during program input";
   Read_Nest_Str      : aliased constant String := "nesting too deep in input";
   Read_Stack_Str     : aliased constant String := "read stack overflow";
   Sys_Proc_Str       : aliased constant String := "accessing or modifying system procedures";

   Var_Space_Str      : aliased constant String := "out of variable name space";
   Wierd_Ch_Str       : aliased constant String := "illegal character in input";
   Fault_Str          : aliased constant String := "internal error";
   In_File_Depth_Str  : aliased constant String := "nesting-level of  'see'  is to deep";
   Out_File_Depth_Str : aliased constant String := "nesting-level of 'tell' is to deep";
   File_Name_Str      : aliased constant String := "file-name error";
   File_Status_Str    : aliased constant String := "status error";
   Init_Str           : aliased constant String := "error during initialization";

   Err_Array : array (Error_Type) of Acc_String := (
                Arity_Error          => Arity_Str'Access,
                Assert_Error         => Assert_Str'Access,
                Atom_Space_Error     => Atom_Space_Str'Access,
                Bad_Cdd_Error        => Bad_Cdd_Str'Access,
                Bad_Char_Error       => Bad_Char_Str'Access,
                Bad_Delim_Error      => Bad_Delim_Str'Access,
                Bad_Exp_Error        => Bad_Exp_Str'Access,
                Call_Error           => Call_Str'Access,
                Clause_Error         => Clause_Str'Access,
                Comment_Error        => Comment_Str'Access,

                Conflict_Error       => Conflict_Str'Access,
                Depth_Error          => Depth_Str'Access,
                Divide_Error         => Divide_Str'Access,
                Eof_Error            => Eof_Str'Access,
                Frame_Space_Error    => Frame_Space_Str'Access,
                Functor_Error        => Functor_Str'Access,
                Goal_Error           => Goal_Str'Access,
                Local_Space_Error    => Local_Space_Str'Access,
                Long_Line_Error      => Long_Line_Str'Access,
                Name_Arg_Error       => Name_Arg_Str'Access,

                Need_Op_Error        => Need_Op_Str'Access,
                Need_Quote_Error     => Need_Quote_Str'Access,
                Need_Rand_Error      => Need_Rand_Str'Access,
                Nvars_Error          => Nvars_Str'Access,
                Op_Error             => Op_Str'Access,
                Prec_Error           => Prec_Str'Access,
                Prog_Fail_Error      => Prog_Fail_Str'Access,
                Read_Nest_Error      => Read_Nest_Str'Access,
                Read_Stack_Error     => Read_Stack_Str'Access,
                Sys_Proc_Error       => Sys_Proc_Str'Access,

                Var_Space_Error      => Var_Space_Str'Access,
                Wierd_Ch_Error       => Wierd_Ch_Str'Access,
                Fault_Error          => Fault_Str'Access,
                In_File_Depth_Error  => In_File_Depth_Str'Access,
                Out_File_Depth_Error => Out_File_Depth_Str'Access,
                File_Name_Error      => File_Name_Str'Access,
                File_Status_Error    => File_Status_Str'Access,
                Init_Error           => Init_Str'Access
              );

   ---------------------
   --  Kind_Of_Error  --
   ---------------------

   function Kind_Of_Error return Error_Type is
   --  This function returns a value of the above type.  It can
   --  be used to pin-point any error that occurs when the Ada-Prolog
   --  interface routines are called.
   begin
      return Err;
   end Kind_Of_Error;

   -------------------------
   --  Set_Kind_Of_Error  --
   -------------------------

   procedure Set_Kind_Of_Error ( E : Error_Type) is
   --  This procedure is used by the underlying Prolog
   --  reasoning tool to set the kind of error so that function
   --  KIND_OF_ERROR knows what to return.
   begin
      Err := E;
   end Set_Kind_Of_Error;

   ------------------
   --  Help_Error  --
   ------------------

   function Help_Error (E : Error_Type) return String is
   begin
      return Err_Array (E).all;
   end Help_Error;

end Prolog.Errors;
