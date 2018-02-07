with Empire.Math;
with Empire.Objects;
with Empire.Ui;

package body Empire.Attack is
--  Empire.Attack -- handle an attack between two pieces. We do everything
--  from fighting it out between the pieces to notifying the user who won
--  and killing off the losing object. Somewhere far above, our caller is
--  responsible for actually removing the object from its list and actually
--  updating the player's view of the world.
   --
   --  Find object being attacked. If it is a city, attacker has 50% chance
   --  of taking city. If successful, give city to attacker. Otherwise kill
   --  attacking piece. Tell user who won.
   --
   --  If attacked object is not a city, loop. On each iteration, select one
   --  piece to throw a blow. Damage the opponent by the strength of the blow
   --  thrower. Stop looping when one object has 0 or fewer hits. Kill off the
   --  dead object. Tell user who won and how many hits her piece has left, if
   --  any.

   procedure Attack (Att_Obj : in Piece_Info_P; Loc : in Location_T) is
   begin
      if Map (Loc).Contents = '*' then
         Attack_City (Att_Obj, Loc);
      else
         Attack_Obj (Att_Obj, Loc);
      end if;
   end Attack;

   procedure Attack_City (Att_Obj : in Piece_Info_P; Loc : in Location_T) is
      Cityp                 : City_Info_P;
      Att_Owner, City_owner : Owner_T;
   begin
      Cityp := Objects.Find_City_At_Loc (Loc);
      if Cityp = null then
         raise Program_Error;
      end if;

      Att_Owner  := Att_Obj.Owner;
      City_owner := Cityp.Owner;

      if Math.Rand_Long (2) = 0          --  attack fails
      then
         if Att_Owner = USER            --  we attack, either possible defender
         then
            Ui.Info
              ("The enemy defending the city at " &
               Location_T'Image (Loc) &
               " crushed our attacking forces.");
         elsif City_owner = USER        --  comp attacks, we defend
         then
            Ui.Info
              ("Your city at " & Location_T'Image (Loc) & " is under attack.");
         end if; -- no message when the computer fails to take over a neutral city

         Objects.Kill_Obj (Att_Obj, Loc);
      else               --  attack succeeds
         Objects.Kill_City (Cityp.all);
         Cityp.Owner := Att_Owner;
         --  successful attacker becomes city garrison
         Objects.Kill_Obj (Att_Obj, Loc);

         if Att_Owner = USER            --  we attack, either defender
         then
            Ui.Info
              ("City at " & Location_T'Image (Loc) & " has been conquered!");
            Ui.Info ("Your army has been dispersed to enforce control.");
            Objects.Ask_Prod (Cityp.all);
         elsif City_owner = USER        --  comp attacks, we defend
         then
            Ui.Info
              ("City at " &
               Location_T'Image (Loc) &
               " has been lost to the enemy!");
         end if; --  no message when the computer takes over a neutral city
      end if;

      --  let city owner see all results
      if City_owner /= UNOWNED then
         Objects.Scan (City_owner, Loc);
      end if;
   end Attack_City;

   --  Attack a piece other than a city. The piece could be anyone's. First we
   --  have to figure out what is being attacked.

   procedure Attack_Obj (Att_Obj : in Piece_Info_P; Loc : in Location_T) is
      Def_Obj : Piece_Info_P;
      Owner   : Owner_T;
   begin
      Def_Obj := Objects.Find_Obj_At_Loc (Loc);

      if Def_Obj = null then
         raise Program_Error;
      end if;

      if Piece_Attr (Def_Obj.Piece_Type).Class = SPACECRAFT then
         return;           --  can't attack satellite
         --  XXX XXX XXX note implication -- we can't move under a satellite
         --  XXX XXX XXX ground and air/space units should be able to co-exist
      end if;

      while Att_Obj.Hits > 0 and Def_Obj.Hits > 0 loop
         if Math.Rand_Long (2) > 0            --  defender hits
         then
            Att_Obj.Hits :=
              Att_Obj.Hits - Piece_Attr (Def_Obj.Piece_Type).Strength;
         else
            Def_Obj.Hits :=
              Def_Obj.Hits - Piece_Attr (Att_Obj.Piece_Type).Strength;
         end if;
      end loop;

      if Att_Obj.Hits > 0 then        --  attacker won
         Describe (Att_Obj, Def_Obj, Loc);
         Owner := Def_Obj.Owner;
         Objects.Kill_Obj (Def_Obj, Loc);
         Survive (Att_Obj, Loc);         --  moves into space
      else                              --  defender won
         Describe (Def_Obj, Att_Obj, Loc);
         Owner := Att_Obj.Owner;
         Objects.Kill_Obj (Att_Obj, Loc);
         Survive (Def_Obj, Loc);
      end if;

      --  show results to first killed
      Objects.Scan (Owner, Loc);
   end Attack_Obj;

   --  Here we look to see if any cargo was killed in the attack. If a ships
   --  contents exceeds its capacity, some of the survivors fall overboard and
   --  drown. We also move the survivor to the given location.

   procedure Survive (Obj : in Piece_Info_P; Loc : in Location_T) is
   begin
      while Objects.Obj_Capacity (Obj) < Obj.Count loop
         Objects.Kill_Obj (Obj.Cargo, Loc);
      end loop;

      Objects.Move_Obj (Obj, Loc);
   end Survive;

   procedure Describe
     (Win_Obj  : in Piece_Info_P;
      Lose_Obj : in Piece_Info_P;
      Loc      :    Location_T)
   is
      Diff : Integer;
   begin
      --  original had case for unit attacking own-side, due to cutesy-ism in
      --  user_move...
      if Win_Obj.Owner = USER then
         --  XXX XXX XXX this should really move above, leaving Describe pure.
         User_Score :=
           User_Score + Piece_Attr (Lose_Obj.Piece_Type).Build_Time;

         Ui.Info
           ("Enemy " &
            Strings.To_String (Piece_Attr (Lose_Obj.Piece_Type).Name) &
            " at " &
            Location_T'Image (Loc) &
            " has been destroyed.");
         Ui.Info
           ("Your " &
            Strings.To_String (Piece_Attr (Win_Obj.Piece_Type).Name) &
            " has " &
            Integer'Image (Win_Obj.Hits) &
            " hits left.");

         Diff := Win_Obj.Count - Objects.Obj_Capacity (Win_Obj);
         if Diff > 0 then
            case Win_Obj.Cargo.Piece_Type is
               when ARMY =>
                  Ui.Info
                    (Integer'Image (Diff) &
                     " armies were lost on the victorious transports.");
               when FIGHTER =>
                  Ui.Info
                    (Integer'Image (Diff) &
                     " fighters were lost on the victorious carriers.");
               when others =>
                  raise Program_Error;
            end case;
         end if;
      else
         --  XXX XXX XXX this should really move above, leaving Describe pure.
         Comp_Score :=
           Comp_Score + Piece_Attr (Lose_Obj.Piece_Type).Build_Time;
         Ui.Info
           ("Your " &
            Strings.To_String (Piece_Attr (Lose_Obj.Piece_Type).Name) &
            " at " &
            Location_T'Image (Loc) &
            " was destroyed.");
      end if;
   end Describe;

end Empire.Attack;
