
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

generic
   type Tagname is (<>);
   Tag : Tagname;
   type Item (Tag : Tagname) is limited private;
   type Link is access Item;
package Prolog.Variant_Garbage_Collection is

   --  Doug Bryan, Aug. 85

   procedure Free (Item : in out Link);
   --  | out (item = null);
   --  if item = null, then do nothing.
   --  may raise storage_error;

   procedure Get (New_Item : in out Link);
   --  | out (new_item /= null);
   --  if new_item /= null then do nothing.
   --  may raise storage_error;

end Prolog.Variant_Garbage_Collection;


