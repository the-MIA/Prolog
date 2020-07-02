
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
package Prolog.Errors is

   Syntax_Error : exception;
   Fatal_Error  : exception;
   Other_Error  : exception;
   --  These are the three exceptions that can be raised by
   --  the routines described in the Ada--Prolog interface package.  That
   --  package interface describes which routines can raise these exceptions.
   --  Once one of these exceptions has been raised, the type and the
   --  operations described in the rest of the package can be used to
   --  pin-point the error.  See below for more details.
   
   type Error_Type is (
      Arity_Error,          --  evaluable predicate called with wrong arity.
      Assert_Error,         --  asserting unsuitable term.
      Atom_Space_Error,     --  out of atom space.
      Bad_Cdd_Error,        --  character value out of range.
      Bad_Char_Error,       --  . expected.
      Bad_Delim_Error,      --  unexpected delimiter.
      Bad_Exp_Error,        --  malformed expression.
      Call_Error,           --  unsuitable arguments to  `call'.
      Clause_Error,         --  unsuitable arguments to `clause'/`deny'.
      Comment_Error,        --  unterminated comment.
      Conflict_Error,       --  precedence conflict.
      Depth_Error,          --  nesting too deep: probably cyclic term.
      Divide_Error,         --  dividing by zero.
      Eof_Error,            --  unexpected end of file.
      Frame_Space_Error,    --  out of frame space.
      Functor_Error,        --  unsuitable arguments to `functor'.
      Goal_Error,           --  unsuitable term appears as goal.
      Local_Space_Error,    --  out of local stack space.
      Long_Line_Error,      --  input line too long.
      Name_Arg_Error,       --  unsuitable arguments to `name'.
      Need_Op_Error,        --  infix or postfix operator expected.
      Need_Quote_Error,     --  closing quote expected.
      Need_Rand_Error,      --  operand or prefix operator expected.
      Nvars_Error,          --  out of variable table space.
      Op_Error,             --  unsuitable arguments to ''op'.
      Prec_Error,           --  precedence violation.
      Prog_Fail_Error,      --  goal failed during program input.
      Read_Nest_Error,      --  nesting too deep in input.
      Read_Stack_Error,     --  read stack overflow.
      Sys_Proc_Error,       --  accessing or modifying system procedures.
      Var_Space_Error,      --  out of variable name space.
      Wierd_Ch_Error,       --  illegal character in input.
      Fault_Error,          --  internal error.
      In_File_Depth_Error,  --  nesting-level of `see' is too deep.
      Out_File_Depth_Error, --  nesting-level of `tell' is too deep.
      File_Name_Error,      --  name_error raised
      File_Status_Error,    --  status_error raised.
      Init_Error            --  Error during initialization.
      );

    procedure Set_Kind_Of_Error(E: Error_Type);
    --  error reporting
    --  This procedure is used by the underlying Prolog
    --  reasoning tool to set the kind of error so that function
    --  KIND_OF_ERROR knows what to return.

   function Kind_Of_Error return Error_Type;
   --  This function returns a value of the above type.  It can
   --  be used to pin-point any error that occurs when the
   --  Ada--Prolog interface routines are called.

   function Help_Error(E: Error_Type) return String;

end Prolog.Errors;
