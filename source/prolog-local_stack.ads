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

--with Prolog.Global_Objects; use Prolog.Global_Objects;
with Prolog.Term_Pkg;       use Prolog.Term_Pkg;

generic

   Maxframes : Natural;
   --  Maximum stack size.

   Maxlocsize : Natural;
   --  Total number of local vars in ALL environments.

package Prolog.Local_Stack is

   subtype Env is Integer range 0 .. Maxframes;

   --  When backtracking occurs, it is necessary to undo
   --  the variable bindings introduced during execution
   --  of the failed clauses.  For this purpose,
   --  certain critical bindings are recorded on an auxiliary
   --  stack called the trail.
   --  The critical bindings are those involving variables created in
   --  environments older than choicepoint: those newer than choicepoint will
   --  disappear when the stacks contract.

   type Trail is private;

   Choicepoint : Env := 0;

   Envtop : Natural := 0;

   Trailsize : Natural := 0;
   --  Size of the trail.

   Loctop : Integer range 0 .. Maxlocsize;
   --  Local vars at the present time.

   function Newenv
     (Callp   : Term;
      Envp    : Env;
      Clausep : Clptr;
      Listp   : Integer_List;
      Nvars   : Integer;
      Glblptr : Term;
      Trlptr  : Trail) return Env;

   --  Create a new environment with :
   --       Invoking goal : callp
   --       Environment for the goal : envp
   --       Active clause : clausep
   --       Number of local vars : nvars
   --       Global stack ptr : glblptr
   --       trail : trlptr
   --  Also sets : present choicepoint,present envtop.
   --  Creates space for NVARS variables on the local stack.

   function Getcall (E : Env) return Term;
   --  Return invoking goal.

   function Getenv (E : Env) return Env;
   --  Return environment for the invoking goal.

   function Getclause (E : Env) return Clptr;
   --  Return active clause

   function Getlist (E : Env) return Integer_List;
   --  Return active list.

   function Getchoice (E : Env) return Env;
   --  Return choicepoint.

   function Getglobal (E : Env) return Term;
   --  Return global ptr.

   function Gettrail (E : Env) return Trail;
   --  Return trail.

   function Envref (Offset : Integer; E : Env) return Term;
   --  Return the OFFSETth variable in environment E.

   procedure Disposeenv;
   --  Recover the top frame.

   procedure Cut (E : Env);
   --  Cut. This accesses the trail too.

   procedure Killlocal (Newtop : Env);
   --  Recover all environments over newtop. Accesses trail.

   procedure Trailvar (V : Term);
   --  Record V on trail if necessary.

   function Present_Trail return Trail;
   --  Value of the trail at the current time.

   function Returnvar (T : Trail) return Term;
   --  Return the most immediate var from trail.

   procedure Trimtrail (Base : Trail);
   --  Remove non-critical references newer than BASE.

   procedure Untrail (Newtrail : Trail);
   --  Undo variable bindings after NEWTRAIL.

private

   type Trailentry;
   type Trail is access Trailentry;

   type Trailentry is record
      Boundvar : Term;
      Chain    : Trail;
   end record;

end Prolog.Local_Stack;
