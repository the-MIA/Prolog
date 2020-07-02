
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
-- --------------------------------------------------------------------


with Ada.Text_IO;
with Prolog.Error_Pkg;      use Prolog.Error_Pkg;
with Prolog.Global_Objects; use Prolog.Global_Objects;
with Prolog.Errors;         use Prolog.Errors;
with Prolog.Vars;           use Prolog.Vars;

package body Prolog.Database is

    use Prolog.Term_Pkg.Atom_Pkg;


    --  These variables belong in the routine MAKECLAUSE. However to save
    --  time on elaboration each time that routine is called, they are
    --  defined at the top level.

    --  VARMAP_TYPE should be a variant record actually with Tag ALLOC.

    type Varmap_Type is
        record
            Alloc     : Boolean := False;
            Sourcevar : Term;    --  The term representing the variable.
            Firstref  : Term;    --  First reference (alloc => false).
            Address   : Integer; --  Offset (alloc => true).
        end record;

    Varmap    : array (1 .. Maxvars)  of Varmap_Type;

    Varcount  : Integer range 0 .. Maxvars;
    --  Number of total vars.

    Framesize : Integer;
    --  Number of non anon vars.

    C         : Clptr;

   --  The clauses which constitute the Prolog program are stored in skeletal
   --  Form, with each variable replaced either by an anonymous
   --  variable (If it occurs only once) or by (if it occurs more than once)
   --  a skeletal reference containing an offset from the base of a frame on
   --  the local stack.  The body of a clause is represented by a collection
   --  of terms chained together by the 'brother' fields of their root nodes.

function Hash(X: Term; E: Env) return Integer is

    --  Hash function for terms.  The value of Hash depends on the root
    --  of the first argument of the term.  Zero is returned if the first
    --  argument is a variable.

    Infinity : constant := 859; --  Arbitrary constant ?
    Y, Z: Term;
    Value : Integer;
begin
    Y := Deref(X, E);
    if Y.Arity = 0 then
        Value := Infinity;
    else
        Z := Deref(Y.Son, E);
        case Z.Tag is
            when Funct => Value := Infinity + Get_Atomno(Z.Name);
            when Intt  => Value := Z.Ival - Boolean'Pos(Z.Ival <= 0);
            when Vart  => Value := 0;
	    when Skelt => Value := 0;
        end case;
    end if;
    return Value;
end Hash;

procedure Findclause(X : Term;
                     E: Env;
                     Dbase : in out Integer_List;
                     Cl: in out Clptr;
                     Value : out Boolean) is

    --  Advance cl to the first applicable clause. Hash is used to compare
    --  clause heads with the goal x. If either has hash function zero, a
    --  variable is present and a match is always possible.

    Ok : Boolean;

begin --  findclause
    if Cl = null then
        Value := False;
    elsif Dbase = null then
        Value := False;
    else
        C := Get_Info(Cl.Head.Name).Proc;
        loop
            Findclause(X,E,Dbase.I,Cl,Ok);
            exit when Ok or (Dbase.Next = null);
            Dbase := Dbase.Next;
        end loop;
        Value := Ok;
    end if;
end Findclause;

procedure Findclause(X : Term;
                     E: Env;
                     Dbase : Integer;
                     Cl: in out Clptr;
                     Value : out Boolean) is

    --  Advance cl to the first applicable clause. Hash is used to compare
    --  clause heads with the goal x. If either has hash function zero, a
    --  variable is present and a match is always possible.
    K: Integer := Hash(X,E);
    Ok: Boolean := False;
    C : Clptr;

begin --  findclause
    if Cl = null then
        Value := False;
    else
        C := Get_Info(Cl.Head.Name).Proc;
        loop
            if (Cl.Dbase /= Dbase) or 
               ((Cl.Keyval /= 0) and (K /= 0) and (Cl.Keyval /= K)) then
                Cl := Cl.Next;
            else
                Ok := True;
            end if;
        exit when (Cl = C) or Ok;
        end loop;
        Value := Ok;
    end if;
end Findclause;

function Makeclause(P : Term; E : Env; Claus : Boolean) return Clptr is
    --  Produce a skeleton for p and add it to the database.  The new clause
    --  is added at the front of the clause chain if asserta is true,
    --  otherwise at the end.

    Q: Term;
    Newhead, Newbody: Term;
    Newkey: Integer;
    Tempcls : Clptr;

    procedure Skelvar(V1: Term;V2 : in out Term) is
        --  Produce a skeleton for a variable v. When the first occurrence of
        --  v is encountered, it is tentatively translated as an anonymous
        --  variable, and a pointer to this variable is stored in the
        --  'varmap' entry. If a second occurrence is encountered, the
        --  anonymous variable is changed to a skeletal reference.
        N: Integer range 0 .. Maxvars;
        W: Term;
        Found: Boolean;
        Id1, Id2 : Varstring;
    begin
      if (V1.Tag = Vart) and then (V1.Id = Anon_String) then
        Skel_Garb.Free(V2);
        Skel_Garb.Get(V2);
        V2.all := Node'(Skelt,null,Heapf,0,null,0,Anon_String,True);
      else    
        N := 0;
        Found := False;
        while (N /= Varcount) and not Found loop
            N := N + 1;
            if V1.Tag = Vart then
                Id1 := V1.Id;
            else
                Id1 := V1.St;
            end if;
            if Varmap(N).Sourcevar.Tag = Vart then
                Id2 := Varmap(N).Sourcevar.Id;
            else
                Id2 := Varmap(N).Sourcevar.St;
            end if;
            Found := Id1 = Id2;
        end loop;
        if Found then
            --  This is not the first occurrence.
            if not Varmap(N).Alloc then
                --  This is the second occurrence -
                --  allocate space on the local stack.
                Framesize := Framesize + 1;
                Varmap(N).Firstref.Anont  := False;
                if Varmap(N).Sourcevar.Tag = Vart then
                    Varmap(N).Firstref.St  := Varmap(N).Sourcevar.Id;
                else
                    Varmap(N).Firstref.St  := Varmap(N).Sourcevar.St;
                end if;
                Varmap(N).Firstref.Offset  := Framesize;
                Varmap(N).Alloc :=  True;
                Varmap(N).Address := Framesize;
            end if;
            Skel_Garb.Free(V2);
            Skel_Garb.Get(V2);
            if Varmap(N).Sourcevar.Tag = Vart then
                V2.all := Node'(Skelt,null,Heapf,0,null,Varmap(N).Address,
                                Varmap(N).Sourcevar.Id,False);
            else
                V2.all := Node'(Skelt,null,Heapf,0,null,Varmap(N).Address,
                                Varmap(N).Sourcevar.St,False);
            end if;
        else
            --  This is the first occurrence - make an anonymous
            --  variable and keep a pointer to it.
            if Varcount >= Maxvars then
                Moan(Nvars_Error, Abortz);
            end if;
            Varcount := Varcount + 1;
            Skel_Garb.Free(V2);
            Skel_Garb.Get(V2);
            if V1.Tag = Vart then
                V2.all := Node'(Skelt,null,Heapf,0,null,0,V1.Id,True);
            else
                V2.all := Node'(Skelt,null,Heapf,0,null,0,V1.St,True);
            end if;
            Varmap(Varcount).Alloc := False;
            Varmap(Varcount).Sourcevar := V1;
            Varmap(Varcount).Firstref := V2;
        end if;
      end if;
    end Skelvar;

    function Skeleton(X: Term; Depth: Integer) return Term is
        --  Produce a skeleton for x.
        Y,Z : Term;

        Temp : Term;

        function Skelargs(S: Term) return Term is
            --  Produce a skeleton for the arguments of a functor node.
            T, U, V: Term;
            Value  : Term;
        begin --  skelargs
            if S = null then
                return null;
            else
                U := Skeleton(S, Depth + 1);
                T := S.Brother;
                V := U;
                while T /= null loop
                    V.Brother := Skeleton(T, Depth + 1);
                    T := T.Brother;
                    V := V.Brother;
                end loop;
                return U;
            end if;
        end Skelargs;

    begin --  Skeleton
        if Depth > Maxdepth then
            Moan(Depth_Error, Abortz);
        end if;
        Y := Deref(X, E);
        case Y.Tag is
            when Funct =>
                Func_Garb.Get(Temp);
                Temp.all := Node'(Funct,null,Heapf,0,null,
                                 Y.Name,Y.Arity,Skelargs(Y.Son));
                return Temp;
            when Intt =>
                Int_Garb.Get(Temp);
                Temp.all := Node'(Intt,null,Heapf,0,null,Y.Ival);
                return Temp;
            when Vart =>
                Skelvar(Y,Z);
                return Z;
            when Skelt => 
                if Y.Anont then
                    Skel_Garb.Get(Temp);
                    Temp.all :=  Node'(Skelt,null,Heapf,0,null,0,Anon_String,
                                     True);
                    return Temp;
                else
                    Skelvar(Y,Z);
                    return Z;
                end if;
        end case;
    end Skeleton;

    function Skelcall(X: Term) return Term is
        --  Produce a skeleton for a goal in a clause body. A variable
        --  a is mapped onto call(X).
        Y : Term;
        Z : Term;
        Temp : Term;
    begin --  skelcall
        Y := Deref(X, E);
        case Y.Tag is
            when Skelt => 
                if Y.Anont then
                    return Y;
                else
                    Moan(Assert_Error, Abortz);
                end if;
            when Funct => return Skeleton(Y, 0);
            when Vart  =>
                Skelvar(Y,Z);
                Func_Garb.Get(Temp);
                Temp.all := Node'(Funct,null,Heapf,0,null,Calla,1,Z);
                return Temp;
            when Intt =>
                Moan(Assert_Error, Abortz);
        end case;

        pragma Assert (False);
        raise Program_Error;
    end Skelcall;

    function Skelhead(X: Term) return Term is
        --  Produce a skeleton for the clause head X.
        Y: Term;
        Value : Term;
    begin
        Y := Deref(X, E);
        if Y.Tag /= Funct then
            Moan(Assert_Error, Abortz);
        end if;
        Value  := Skeleton(Y, 0);
        Newkey := Hash(Y, E);
	return Value;
    end Skelhead;

    function Skelbody(X: Term; Depth: Integer) return Term is
        --  Produce a skeleton for a clause body.
        Y, Z: Term;
        Value : Term;
    begin
        if Depth > Maxdepth then
            Moan(Depth_Error, Abortz);
        end if;
        Y := Deref(X, E);
        if Claus then
            Z := Skelcall(Y);
            if Y.Brother /= null then
                Z.Brother := Skelbody(Y.Brother,Depth+1);
            else
                Z.Brother := null;
            end if;
            Value := Z;
        else
            if Isfunc(Y, Commaa, 2) then
                Z := Skelcall(Y.Son);
                Z.Brother := Skelbody(Y.Son.Brother, Depth + 1);
                Value := Z;
            else
                Value := Skelcall(Y);
            end if;
        end if;
        return Value;
    end Skelbody;

begin --  MAKECLAUSE
    Varcount := 0;
    Framesize := 0;
    Q := Deref(P, E);

    if Isfunc(Q, Arrowa, 2) then
        Newhead := Skelhead(Q.Son);
        Newbody := Skelbody(Q.Son.Brother, 0);
    else
        Newhead := Skelhead(Q);
        Newbody := null;
    end if;

    if Get_Info (Newhead.Name).Sys and not
          (Mode = Sysm and
           Get_Info (Newhead.Name).Pclass = Normp) then
        Moan (Sys_Proc_Error, Abortz);
    end if;

    if Mode = Sysm then
        Get_Info (Newhead.Name).Sys := True;
    end if;

    Tempcls := null;
    Clause_Garb.Get (Tempcls);
    Tempcls.all :=  Cls'(Head => Newhead, The_Body => Newbody,Typ => Madec,
                    Nvars => Framesize, Keyval => Newkey,
                    Dbase => -1, Previous => null, Next => null);
    return Tempcls;
end Makeclause;

function Makeclause(P : Term; E : Env) return Clptr is
begin
    return Makeclause(P,E,False);
end Makeclause;

function Makeclause(Head : Term; Tail : Term; E : Env) return Clptr is
    Temp : Term;
begin
    if Tail = null then
        return Makeclause(Head,E,True);
    else
        Head.Brother := Tail;
        Func_Garb.Get(Temp);
        Temp.all := Node'(Funct,null,Heapf,0,null,Arrowa,2,Head);
        return Makeclause(Temp,E,True);
    end if;
end Makeclause;


-----------------
--  Addclause  --
-----------------

function Addclause(P       : Term;
                   E       : Env;
                   Dbase   : Integer;
                   Asserta : Boolean) return Clptr is
    Cl : Clptr;
begin
    Cl := Makeclause (P, E);
    Addclause (Cl, Dbase, Asserta);
    return Cl;

end Addclause;


-----------------
--  Addclause  --
-----------------

procedure Addclause(C       : in out Clptr;
                    Dbase   : in     Integer;
                    Asserta : in     Boolean) is
    Cl : Clptr;

    procedure Pluga(Cp, C : in out Clptr) is
        --  Insert the new clause at the beginning of chain cp.
    begin
        if Cp = null then
            Cp := C;
            Cp.Previous := Cp;
            Cp.Next := Cp;
        else
            C.Previous := Cp.Previous;
            C.Next := Cp;
            Cp.Previous.Next := C;
            Cp.Previous := C;
            Cp := C;
        end if;
    end Pluga;

    procedure Plugz(Cp, C : in out Clptr) is
        --  Insert the new clause at the end of chain cp.
    begin
        if Cp = null then
            Pluga(Cp,C);
        else
            C.Next := Cp;
            C.Previous := Cp.Previous;
            Cp.Previous := C;
            C.Previous.Next := C;
        end if;
    end Plugz;

begin
    C.Dbase := Dbase;
    if Asserta then
	Pluga(Get_Info(C.Head.Name).Proc,C);
    else
	Plugz(Get_Info(C.Head.Name).Proc,C);
    end if;
end Addclause;

procedure Zapclause(Cl : Clptr) is
    --  Delete the clause entry pointed to by CL.
    Temp : Clptr;
begin
    if Cl = null then
        null;
    elsif Cl.Previous = Cl then
        Get_Info(Cl.Head.Name).Proc := null;
        Temp := Cl;
        Clause_Garb.Free(Temp);
    elsif Get_Info(Cl.Head.Name).Proc = Cl then
        Get_Info(Cl.Head.Name).Proc := Cl.Next;
        Cl.Previous.Next := Cl.Next;
        Cl.Next.Previous := Cl.Previous;
        Temp := Cl;
        Clause_Garb.Free(Temp);
    else
        Cl.Previous.Next := Cl.Next;
        Cl.Next.Previous := Cl.Previous;
        Temp := Cl;
        Clause_Garb.Free(Temp);
    end if;
end Zapclause;

end Prolog.Database;
