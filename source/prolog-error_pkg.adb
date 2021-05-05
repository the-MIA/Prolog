
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

package body Prolog.Error_Pkg is

   ------------
   --  Moan  --
   ------------

   procedure Moan (E : Error; A : Moanaction) is
      --  Output an error message.
   begin
      Set_Kind_Of_Error (E);
      case A is
         when Diez    =>  raise Fatal_Error;
         when Abortz  =>  raise Other_Error;
         when Syntaxz =>  raise Syntax_Error;
      end case;
   end Moan;

end Prolog.Error_Pkg;


