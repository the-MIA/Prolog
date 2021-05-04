
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

package body Prolog.Variant_Garbage_Collection is

--  type item is limited private;
--  type link is access item;

    type Node_Type;
    type List_Type is access Node_Type;
    type Node_Type is
	record
	    L    : Link;
	    Next : List_Type;
	end record;

    Free_Nodes_With_No_Items, Free_Nodes_With_Free_Items : List_Type;

--    Free_Items   : List_Type renames Free_Nodes_With_Free_Items;
--    Free_Nodes   : List_Type renames Free_Nodes_With_No_Items;

--------------------------
   procedure Free (Item : in out Link) is
      Temp : List_Type;
   begin
      if Item /= null then

         if Free_Nodes_With_No_Items = null then
            Temp := new Node_Type;
         else
            Temp := Free_Nodes_With_No_Items;
            Free_Nodes_With_No_Items := Free_Nodes_With_No_Items.all.Next;
         end if;

         Temp.all := (L => Item, Next => Free_Nodes_With_Free_Items);
         Free_Nodes_With_Free_Items := Temp;
         Item := null;
      end if;
   end Free;

--------------------------
   procedure Get (New_Item : in out Link) is
      Temp : List_Type;
   begin
      if New_Item = null then
         if Free_Nodes_With_Free_Items = null then
            New_Item := new Item (Tag);

         else
            Temp := Free_Nodes_With_Free_Items;
            Free_Nodes_With_Free_Items := Free_Nodes_With_Free_Items.all.Next;
            New_Item := Temp.all.L;
            Temp.all := (L => null, Next => Free_Nodes_With_No_Items);
            Free_Nodes_With_No_Items := Temp;
         end if;
      end if;
   end Get;

--------------------------
end Prolog.Variant_Garbage_Collection;
