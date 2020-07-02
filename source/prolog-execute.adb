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
with Ada.Text_Io;           use Ada.Text_IO;
with Prolog.Evaluable_Predicates;  use Prolog.Evaluable_Predicates;
with Prolog.Vars;                  use Prolog.Vars;

with Prolog.Global_Objects;        use Prolog.Global_Objects;
with Prolog.Write_Out;             use Prolog.Write_Out;
with Prolog.Error_Pkg;             use Prolog.Error_Pkg;
with Prolog.Errors;                use Prolog.Errors;
with Prolog.Ada_Logic;             use Prolog.Ada_Logic;
with Prolog.Transformations;       use Prolog.Transformations;
with Prolog.Database;              use Prolog.Database;

package body Prolog.Execute is

    use Local;
    use Atom_Pkg;

    Temp : Term;

    type State_Type is
             (Callq, Procq, Bodyq, Returnq, Failq, Finalq);

    type Uaction_Type is
             (Funcu,    Intu,     Vtbindu,  Tvbindu,  Vvbindu,  Succeedu,
              Failu);

    Uaction : array (Nodetag range Funct .. Skelt,
                     Nodetag range Funct .. Skelt) of Uaction_Type :=
                                       (others => (others => Failu));
    -- Table of actions for Unify.

    Callp: Term;
    Envp, Callenv, Baseenv, Goalenv: Integer;
    Clausep: Clptr;
    Listp : Integer_List;
    State: State_Type;
    This_Dbase : Integer_List;

    Indent : Integer := 0;

    User_Interrupt_Active : Boolean := False;
    -- Has ^C been pressed?

    -- task to handle interrupts.

    task Interrupt is
        entry Control_C;
--        for CONTROL_C use at address'ref(IFACE_INTR.SIGINT);
    end;
    task body Interrupt is
    begin
       loop
          select
	     accept Control_C do
		 -- TEXT_IO.PUT_LINE("Got the interrupt");
		 User_Interrupt_Active := True;
	     end;
          or terminate;
          end select;
       end loop;
    end;

    procedure Killstacks(E : Integer) is
        T : Term;
    begin
        T := Getglobal(E+1);
        Killlocal(E);
        Killglobal(T);
    end Killstacks;

   function Execute return Boolean is
   -- Execute a goal.

      Value : Boolean;
      Dummy : Boolean;
      Temp1, Temp2 : Term;

   begin  -- execute
      User_Interrupt_Active := False;
      loop
          if User_Interrupt_Active then
              User_Interrupt_Active := False;
              raise User_Interrupt;
          end if;
          case State is
              when Callq  =>
                  -- 'callp' holds a goal and 'callenv' its environment.
                  if Tracing then
                      Trace(Goald, Callp, Callenv);
                  end if;
                  case Get_Info(Callp.Name).Pclass is
                      when Normp =>
                          Clausep := Get_Info(Callp.Name).Proc;
                          Listp := This_Dbase;
                          State := Procq;
                      when Evalp =>
                          if Callp.Name = Calla then
                              Temp1 := Callp.Brother;
                              Temp2 := Deref(Callp.Son,Callenv);
                              Callp := Makefunc(Temp2.Name,Temp2.Arity,
                                                Temp2.Son);
                              Callp.Brother := Temp1;
                              State := Callq;

                          elsif Call_Eval_Pred(Callp, Callenv,
                                             Get_Info(Callp.Name).Routine,
                                             Get_Info(Callp.Name).Arity,
                                             This_Dbase) then
                              State := Returnq;
                          else
                              State := Failq;
                          end if;
                  end case;
              when Procq =>
                  -- 'clausep' points to a chain of untried clauses
                  -- for the goal in 'callp'.
                  Findclause(Callp, Callenv, Listp, Clausep, Dummy);
                  if Dummy then
                      Envp := Newenv(Callp, Callenv, Clausep,Listp,
                             Clausep.Nvars,Glotop,Present_Trail);
                      if Clausep.Next /= Get_Info(Clausep.Head.Name).Proc
                                                                       then
                          Choicepoint := Envp;
                      elsif Listp.Next /= null then
                          Choicepoint := Envp;
                      else
                          null;
                      end if;
--###
--###                    declare
--###                        Temp : Term := clausep.the_body;
--###                    begin
--###                        Put("Unifying : head : ");
--###                        TRACE(goald,Clausep.head,envp);
--###                        Put(" :- ");
--###                        while Temp /= null loop
--###                            TRACE(goald,Temp,envp);
--###                          temp := temp.brother;
--###                        end loop;
--###                        Put(" end ");
--###                        Put(" and ");
--###                        Trace(goald,callp,callenv);
--###                   end;
                      if Unify(Clausep.Head, Callp, Envp,
                               Callenv, 0) then
--###
--###                    declare
--###                        Temp : Term := clausep.the_body;
--###                    begin
--###                        Put("Unified : head : ");
--###                        TRACE(goald,Clausep.head,envp);
--###                        Put(" :- ");
--###                        while Temp /= null loop
--###                            TRACE(goald,Temp,envp);
--###                            temp := temp.brother;
--###                        end loop;
--###                        Put(" end ");
--###                        Put(" and ");
--###                        Trace(goald,callp,callenv);
--###                        Put("Brothers of callp : ");
--###                   Temp := callp.brother;
--###                        while Temp /= null loop
--###                            TRACE(goald,Temp,envp);
--###                            temp := temp.brother;
--###                        end loop;
--###                        Put(" end ");
--###                   end;
--###
                          Callp := Clausep.The_Body;
                          Callenv := Envp;
                          State := Bodyq;
                      else
                          State := Failq;
                      end if;
                  else
                      State := Failq;
                  end if;
              when Bodyq =>
                  -- 'callp' points to a chain of uncalled goals in
                  --  the body of some clause, and 'callenv' to the 
                  --  environment for the clause.
                  if Callp = null then
                      Envp := Callenv;
                      Callp := Getcall(Envp);
                      Callenv := Getenv(Envp);
                      Clausep := Getclause(Envp);
                      Listp := Getlist(Envp);
                      if Envp > Choicepoint then
                          Disposeenv;
                      end if;
                      if Tracing then
                          Trace(Provedd, Callp, Callenv);
                      end if;
                      State := Returnq;
                  else
                      State := Callq;
                  end if;
              when Returnq =>
                  -- The subgoal in 'callp' has just succeeded.
                  if Callenv > Goalenv then
                      Callp := Callp.Brother;
                      State := Bodyq;
                  else
                      Value   := True;
                      State := Finalq;
                  end if;
              when Failq =>
                  -- Failure has occurred.  'choicepoint' is the newest
                  -- environment with a nondeterminate choice.
                   if Choicepoint > Baseenv then
                      Callp := Getcall(Choicepoint);
                      Callenv := Getenv(Choicepoint);
                      Clausep := Getclause(Choicepoint);
                      Listp := Getlist(Choicepoint);
                      if (Clausep.Next =
                          Get_Info(Clausep.Head.Name).Proc) and
                         (Listp.Next = null) then
                          Clausep := null;
                      elsif (Clausep.Next =
                             Get_Info(Clausep.Head.Name).Proc) then
                          Clausep := Clausep.Next;
                          Listp := Listp.Next;
                      else
                          Clausep := Clausep.Next;
                      end if;
                      Killstacks(Choicepoint - 1);
                      State := Procq;
                  else
                      Value   := False;
                      State := Finalq;
                  end if;
              when Finalq =>
                  null;
          end case;
      exit when State = Finalq;
      end loop;
      return Value;
   end Execute;
                
    function Unify(X1, X2: Term; E1, E2: Integer; Depth: Integer) return
                                                       Boolean is
        -- Unify x1 and x2.  Perform the matching substitution
        -- by binding variables.
        Y1, Y2: Term;
        Value : Boolean;

        function Unifyargs(S1, S2: Term) return Boolean is
            -- Unify the arguments of a pair of functor nodes.
            T1, T2: Term;
            Ok: Boolean;
        begin
            T1 := S1;
            T2 := S2;
            Ok := True;
            while (T1 /= null) and Ok loop
                Ok := Unify(T1, T2, E1, E2, Depth + 1);
                T1 := T1.Brother;
                T2 := T2.Brother;
            end loop;
            return Ok;
        end Unifyargs;

    begin -- Unify
        if Depth > Maxdepth then
            Moan(Depth_Error, Abortz);
        end if;
        Y1 := Deref(X1, E1);
        Y2 := Deref(X2, E2);
        case Uaction(Y1.Tag, Y2.Tag) is
            when Funcu =>
                if (Y1.Name = Y2.Name) and
                   (Y1.Arity = Y2.Arity) then
                    Value := Unifyargs(Y1.Son, Y2.Son);
                else
                    Value := False;
                end if;
            when Intu =>
                Value := Y1.Ival = Y2.Ival;
            when Vtbindu =>
                Bind(Y1, Y2, E2, 0);
                Trailvar(Y1);
                Value := True;
            when Tvbindu =>
                Bind(Y2, Y1, E1, 0);
                Trailvar(Y2);
                Value := True;
            when Vvbindu =>
                Bindvars(Y1, Y2);
                Value := True;
            when Succeedu =>
                Value := True;
            when Failu =>
                Value := False;
        end case;
	return Value;
    end Unify;

   function Get_Solutions(Q : Term; E : Integer) return Solution is
       A1, A2 : Solution := null;
       First_Var : Boolean := True;
       
       function Total_Deref(T : Term; E : Integer) return Term is
           Y,Z,W,V : Term;
       begin
            Y := Deref(T,E);
            case Y.Tag is
                when Funct =>
                    Z := new Node'(Funct,null,Heapf,0,null,Y.Name,Y.Arity,
                                   null);
                    if (Y.Arity > 0) then
                        Z.Son := Total_Deref(Y.Son,E);
                        W := Z.Son;
                        V := Y.Son.Brother;
                        for I in 1 .. (Z.Arity - 1) loop
                            W.Brother := Total_Deref(V,E);
                            V := V.Brother;
                            W := W.Brother;
                        end loop;
                    end if;
                when Vart =>
                    Z := new Node'(Vart,null,Heapf,0,null,null,Y.Id);
                when Skelt =>
                    Z := new Node'(Skelt,null,Heapf,0,null,0,Anon_String,True);
                when Intt =>
                    Z := new Node'(Intt,null,Heapf,0,null,Y.Ival);
            end case;                   
            return Z;
       end Total_Deref;
               
       procedure Found_Var(V : Term) is
           W : Term;
       begin
           W := Total_Deref(V,E);
           if First_Var then
               A1 := new Answer_Record'((new String'(To_String(V.Id)),
                                         W),null);
               A2 := A1;
               First_Var := False;
           else
               A2.Next := new Answer_Record'((new String'(To_String(V.Id)),
                                              W),null);
               A2 := A2.Next;
           end if;
       end Found_Var;

       procedure Collect_Vars(Q : Term) is
           P : Term;
       begin
           case Q.Tag is
               when Funct =>
                   P := Q.Son;
                   for I in 1 .. Q.Arity loop
                      Collect_Vars(P);
                      P := P.Brother;
                   end loop;
               when Vart => Found_Var(Q);
               when Intt | Skelt => null;
           end case;
       end Collect_Vars;

   begin
       Collect_Vars(Q);
       return A1;
   end Get_Solutions;

   procedure First_Answer(Goalp: Term; Envp: Integer;
                          Dbase: Integer_List; Ans : out Solution;
                          Success : out Boolean) is
    -- Execute a goal.
        G : Term := Deref(Goalp,Envp);
    begin
        Killstacks(0);
        Callp := Makefunc(G.Name,G.Arity,G.Son);
        Callenv := Envp;
        Goalenv := Envp;
        Baseenv := Envtop;
        State := Callq;
        This_Dbase := new Integer_Record'(0,Dbase);
        if Execute then
            Success := True;
            Ans := Get_Solutions(Callp,Callenv);
        else
            Success := False;
            Ans := null;
        end if;
    exception
        when User_Interrupt =>
            Success := False;
            Ans := null;
            raise;
    end First_Answer;

    procedure Next_Answer(Ans : out Solution; Success : out Boolean) is
    begin
        State := Failq;
        if Execute then
            Success := True;
            Ans := Get_Solutions(Callp,Callenv);
        else
            Success := False;
            Ans := null;
        end if;
    exception
        when User_Interrupt =>
            Success := False;
            Ans := null;
            raise;
    end Next_Answer;

    procedure Initunify is
        -- Set up table of actions for Unify.
    begin
        Uaction(Funct, Funct)  := Funcu;
        Uaction(Intt,  Intt )  := Intu;
        Uaction(Vart,  Funct)  := Vtbindu;
        Uaction(Vart,  Intt )  := Vtbindu;
        Uaction(Funct, Vart )  := Tvbindu;
        Uaction(Intt,  Vart )  := Tvbindu;
        Uaction(Vart,  Vart )  := Vvbindu;
        Uaction(Skelt, Funct)  := Succeedu;
        Uaction(Skelt, Intt )  := Succeedu;
        Uaction(Skelt, Vart )  := Succeedu;
        Uaction(Funct, Skelt)  := Succeedu;
        Uaction(Intt,  Skelt)  := Succeedu;
        Uaction(Vart,  Skelt)  := Succeedu;
        Uaction(Skelt, Skelt)  := Succeedu;
        Uaction(Funct, Intt )  := Failu;
        Uaction(Intt,  Funct)  := Failu;
    end Initunify;

begin
    Initunify;
end Prolog.Execute;
