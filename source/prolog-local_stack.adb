
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
with Prolog.Error_Pkg;  use Prolog.Error_Pkg;
with Prolog.Errors;     use Prolog.Errors;
with Prolog.Vars;       use Prolog.Vars;

with Prolog.Garbage_Collection;

package body Prolog.Local_Stack is

    package Trail_Garb is
       new Prolog.Garbage_Collection (Trailentry, Trail);

    --The abstract Prolog machine contains two stacks, the local stack and
    --the global stack.  The local stack is held in the global array
    --'display', with local variables in the global array 'locstack'.  These
    --arrays have stack pointers 'envtop' and 'loctop' respectively.

    type Display_Type is record
        Fcall   : Term;    -- Invoking goal.
        Fenv    : Env;     -- Environment for the goal
        Fchoice : Env;     -- Choicepoint at activation.
        Fclause : Clptr;   -- Active clause.
        Flist   : Integer_List; -- Active list.
        Ftrail  : Trail;   -- Head of trail at activation.
        Fglotop : Term;    -- Top of global stack at activation.
        Fbase   : Integer range 0 .. Maxlocsize;
        -- Base of frame in locstack.
    end record;

    Display : array (1 .. Maxframes) of Display_Type;

    type Locstacktype is array (1 .. Maxlocsize) of Term;
    Locstack : Locstacktype;

    Trailend : Trail;

    function Newenv(Callp : Term;
                    Envp : Env;
                    Clausep : Clptr;
                    Listp : Integer_List;
                    Nvars : Integer;
                    Glblptr : Term;
                    Trlptr : Trail) return Env is
    -- Create a new environment e.
        E : Env;
    begin
        if Envtop >= Maxframes then
            Moan(Frame_Space_Error, Abortz);
        end if;
        Envtop := Envtop + 1;
        E := Envtop;
        Display(E) := (Callp,Envp,Choicepoint,Clausep,Listp,Trlptr,
                       Glblptr,Loctop);
        Envtop := E;
        if Loctop + Nvars > Maxlocsize then
            Moan(Local_Space_Error, Abortz);
        end if;
        for N in Loctop + 1 .. Loctop + Nvars loop
            if Locstack(N) = null then
                Var_Garb.Get(Locstack(N));
                Locstack(N).all := Node'(Vart,null,Localf,N,null,
                                         null,Null_String);
            else
                Locstack(N).Brother := null;
                Locstack(N).Field := Localf;
                Locstack(N).Scope := N;
                Locstack(N).Chain := null;
                Locstack(N).Val := null;
                Locstack(N).Id := Null_String;
            end if;
        end loop;
        Loctop := Loctop + Nvars;
        return E;
    end Newenv;

    function Getcall(E : Env) return Term is
    -- return invoking goal.
    begin
        return Display(E).Fcall;
    end;

    function Getenv(E : Env) return Env is
    -- return environment for the invoking goal.
    begin
        return Display(E).Fenv;
    end;

    function Getclause(E : Env) return Clptr is
    -- return active clause
    begin
        return Display(E).Fclause;
    end;

    function Getlist(E : Env) return Integer_List is
    -- return active list
    begin
        return Display(E).Flist;
    end;

    function Getchoice(E : Env) return Env is
    -- return choicepoint.
    begin
        return Display(E).Fchoice;
    end;

    function Getglobal(E : Env) return Term is
    -- return global ptr.
    begin
        return Display(E).Fglotop;
    end;

    function Gettrail(E : Env) return Trail is
    -- return trail.
    begin
        return Display(E).Ftrail;
    end;

    function Envref(Offset: Integer; E: Env) return Term is
    -- return the OFFSETth variable in environment E
    begin
        return Locstack(Display(E).Fbase + Offset);
    end;

    procedure Disposeenv is
    -- Recover the top frame on the local stack.
        -- TEMP : ENV;
    begin
        Loctop := Display(Envtop).Fbase;
        Envtop := Envtop - 1;
    end Disposeenv;

    procedure Cut(E : Env) is
    -- Cut environment DISPLAY(E).  On entry, all goals on the local stack 
    -- above e must be descended from e.  The newest ancestor 
    -- of e (including e itself) which is not a clause for (_, _), (_; _)
    -- or call(_) is made determinate.  Local stack space above e is 
    -- reclaimed.
        Envp : Env := E;
        Cl : Clptr := Display(E).Fclause;
    begin
        while (Display(Envp).Fchoice > 0) and ((Cl = Andg) or
              (Cl = Or1g) or (Cl = Or2g) or (Cl = null)) loop
            Envp := Display(Envp).Fenv;
            Cl := Display(Envp).Fclause;
        end loop;
        Choicepoint := Display(Envp).Fchoice;
        Trimtrail(Display(Envp).Ftrail);
        if Envtop > E then
            Loctop := Display(E+1).Fbase;
            Envtop := E;
        end if;
    end Cut;

    procedure Killlocal(Newtop : Env) is
    --Dispose of all environments after newtop, together with all
    --associated global storage, and undo critical variable bindings.
    begin
        if Envtop > Newtop then
            Untrail(Display(Newtop+1).Ftrail);
            Choicepoint := Display(Newtop+1).Fchoice;
            Loctop := Display(Newtop+1).Fbase;
            Envtop := Newtop;
        end if;
        for I in reverse Newtop+1..Envtop loop
           Disposeenv;
        end loop;
end Killlocal;

    function Critical(V : Term) return Boolean is
    -- Need V be recorded on the TRAIL.
    begin
        if Choicepoint = 0 then
            return False;
        else
            case V.Field is
                when Globalf =>
                    return (V.Scope <= 
                            Display(Choicepoint).Fglotop.Scope);
                when Localf =>
                    return (V.Scope <= Display(Choicepoint).Fbase);
                when Heapf =>
                    Moan(Fault_Error, Diez);
                    pragma Assert(False, "Moan failed");
                    -- This can never happen, but put it here anyway.
                    return False;
            end case;
        end if;
    end Critical;

    procedure Trailvar(V: Term) is
    -- Record v on the trail if necessary.
        P : Trail;
    begin
        if V.Tag = Vart then
            if Critical(V) then
                Trail_Garb.Get(P);
                P.all := Trailentry'(V,null);
                Trailend.Chain := P;
                Trailend := P;
                Trailsize := Trailsize + 1;
            end if;
        else
            Moan(Fault_Error,Diez);
        end if;
    end Trailvar;

    function Present_Trail return Trail is
    begin
        return Trailend;
    end;

    procedure Trimtrail(Base: Trail) is
    -- Remove references to variables newer than choicepoint. Some of the
    -- Ftrail entries in 'display' may be made invalid by this operation,
    -- but it doesn't matter, since they will never be used for
    -- backtracking.
        P, Q : Trail;
    begin
        P := Base; Q := P.Chain;
        while Q /= null loop
            if not Critical(Q.Boundvar) then
                P.Chain := Q.Chain;
                Q.Boundvar := null;
                Trail_Garb.Free(Q);
                Trailsize := Trailsize - 1;
            else
                P := Q;
            end if;
            Q := P.Chain;
        end loop;
        Trailend := P;
    end Trimtrail;

    procedure Untrail(Newtrail: Trail) is
    --Undo all variable bindings recorded a final segment of the trail,
    --starting with the one after 'newtrail'. Untriail is also used at the
    --end of execution to recover the storage used for the trail.
        P, Q: Trail;
    begin
        Trailend := Newtrail;
        P := Trailend.Chain;
        Trailend.Chain := null;
        while P /= null loop
            P.Boundvar.Val := null;
            Q := P.Chain;
            P.Boundvar := null;
            Trail_Garb.Free(P);
            P := Q;
            Trailsize := Trailsize - 1;
        end loop;
    end Untrail;

    procedure Inittrail is
    -- Set up the trail with a dummy list head.
    begin
        Trailend := new Trailentry'(null,null);
        Trailsize := 0;
    end Inittrail;

    function Returnvar(T : Trail) return Term is
    -- return the most immediate var from trail.
    begin
        return T.Boundvar;
    end;

begin
    Inittrail;
end Prolog.Local_Stack;