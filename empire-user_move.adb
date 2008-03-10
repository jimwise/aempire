with Empire.Attack;
with Empire.Editing;
with Empire.Game;
with Empire.Locations;
with Empire.Mapping;
with Empire.Math;
with Empire.Objects;
with Empire.Ui;

package body Empire.User_Move is

   procedure User_Move is
     Sec, Sec_Start : Location_T;
     Obj, Next_Obj : Piece_Info_P;
     Prod : Piece_Type_T;
   begin
      -- First we loop through objects to update the user's view
      -- of the world and perform any other necessary processing.
      -- We would like to have the world view up to date before
      -- asking the user any questions.  This means that we should
      -- also scan through all cities before possibly asking the
      -- user what to produce in each city (XXX but we don't -- Jim).

      for N in User_Obj'Range
      loop
         Obj := User_Obj(N);
         while Obj /= null
         loop
            Obj.Moved := 0;
            Objects.Scan(User_Map, Obj.Loc);    --  refresh user's view of world
            Obj := Obj.Piece_Link.Next;
         end loop;
      end loop;

      -- produce new hardware
      for I in City'Range
      loop
         if City(I).Owner = User
         then
            Objects.Scan(User_Map, City(I).Loc);
            Prod := City(I).Prod;
            if Prod = NOPIECE
            then
               Objects.Set_Prod(City(I));       --  ask user what to produce
            elsif City(I).Work >= Piece_Attr(Prod).Build_Time
            then
               Ui.Info("City at " & Integer'Image(City(I).Loc) & " has compeleted " & Piece_Attr(Prod).Article.all);
               Objects.Produce(City(I));
            else
              City(I).Work := City(I).Work + 1;
            end if;
         end if;
      end loop;

      -- move all satellites (they're not user-controled)
      Obj := User_Obj(SATELLITE);
      while Obj /= null
      loop
         -- cache next satellite, as the satellite may run out of fuel and burn (losing its linkage in Kill_Obj)
         Next_Obj := Obj.Piece_Link.Next;
         Objects.Move_Sat(Obj.all);
         Obj := Next_Obj;
      end loop;

      Sec_Start := Ui.Cur_Sector;          --  get currently displayed sector.
      Sec := Sec_Start;
      -- loop through sectors, moving every piece in the sector
      loop
         Ui.Sector_Change;              --  allow screen to be redrawn

         for N in Move_Order'Range
         loop
            -- note that this means that move_order is obeyed only within each sector
            -- it would make a better simulation, but a worst UI, to reverse the nesting of the loops
            -- bonus question:  can this be gamed (say to move a slow unit at the edge of a sector early?)
            -- a: probably not to advantage.  could be a disadvantage (transport has to move before fighter sweep, eg
            Obj := User_Obj(Move_Order(N));

            while Obj /= null
            loop
               Next_Obj := Obj.Piece_Link.Next; --  cache next object, as this object may die in the mean time
               if Obj.Moved /= 0
                 then
                    if Locations.Loc_Sector(Obj.Loc) = Sec
                    then
                       Piece_Move(Obj.all);
                    end if;
               end if;

               if Ui.Cur_Sector = Sec
               then
                  Ui.Print_Sector_U(Sec);
               end if;

               Obj := Next_Obj;
            end loop;
         end loop;

         if Sec = Sector_T'Last
         then
            Sec := Sector_T'First;
         else
            Sec := Sector_T'Succ(Sec);
         end if;
         exit when Sec = Sec_Start;  --  we've come full circle
      end loop;

      if Save_Movie
      then
         Game.Save_Movie_Screen;
      end if;
   end User_Move;


   -- Move a piece.  We loop until all the moves of a piece are made.  Within
   -- the loop, we first awaken the piece if it is adjacent to an enemy piece.
   -- Then we attempt to handle any preprogrammed function for the piece.  If
   -- the piece has not moved after this, we ask the user what to do.

   procedure Piece_Move (Obj : in out Piece_Info_T) is
      Changed_Loc : Boolean := false;
      Need_Input : Boolean := false;
      Awoke : Boolean := False;
      Saved_Moves : Integer;
      Saved_Loc : Location_T;
      Cityp : City_Info_P;
   begin
      -- set func for piece if on city
      Cityp := Objects.Find_City_At_Loc(Obj.Loc);
      if Cityp /= null and then Cityp.Func(Obj.Piece_Type) /= NOFUNC
      then
         User_Obj_Func(Obj, Cityp.Func(Obj.Piece_Type));
      end if;

      -- this recomputes, which is needed because Obj_Moves(Obj) can drop
      -- if Obj takes damage

      while Obj.Moved < Objects.Obj_Moves(Obj)
      loop
         Saved_Moves := Obj.Moved;
         Saved_Loc := Obj.Loc;
         Awake(Obj, Awoke);
         if Awoke or Need_Input
         then
            Ask_User(Obj);
            -- XXX should we also display before asking, or is this guaranteed by sequence?
            Ui.Display_Loc_U(Obj.Loc);     --  let user see result at once
            Need_Input := False;
         end if;

         if Obj.Moved = Saved_Moves     --  non-move (user didn't move object)
         then
            case Obj.Func is
               when NOFUNC =>
                  null;                 --  will result in need_input being set, below
               when RANDOM =>
                  Move_Random(Obj);
               when SENTRY =>           --  for now, sentry means `guard in place'.  some empire
                  Obj.Moved := Objects.Obj_Moves(Obj); --  games have a roving sentry function...
               when FILL =>
                  Move_Fill(Obj);
               when LAND =>
                  Move_Land(Obj);
               when EXPLORE =>
                  Move_Explore(Obj);
               when ARMYATTACK =>
                  Move_Armyattack(Obj);
               when REPAIR =>
                  Move_Repair(Obj);
               when WFTRANSPORT =>
                  Move_Transport(Obj);
               when MOVE_N .. MOVE_NW =>
                  Move_Dir(Obj);
               when others =>   --  MOVE_TO_DEST -- the two COMP_ funcs won't occur
                  Move_Path(Obj);
            end case;
         end if;

         -- if Obj.Func was NOFUNC, or object failed to move:
         if Obj.Moved = Saved_Moves
         then
            Need_Input := True;
         end if;

         -- handle aircraft specially.  If in a city or carrier, turn
         -- is over and reset range to max.  Otherwise, if
         -- range = 0, fighter crashes and burns and turn is over.

         -- note that aircraft fuel state is checked in Awake.

         -- XXX can we get here with an object with zero remaining hits?
         if Piece_Attr(Obj.Piece_Type).Class = AIRCRAFT and Obj.Hits > 0
         then
            if (User_Map(Obj.Loc).Contents = 'O' or User_Map(Obj.Loc).Contents = 'C') and Obj.Moved > 0
            then
               Obj.Piece_Range := Piece_Attr(Obj.Piece_Type).Piece_Range; --  refuel
               Obj.Moved := Objects.Obj_Moves(Obj); --  end move
               User_Obj_Func(Obj, NOFUNC);                    --  end function
               Ui.Info("Landing confirmed.");
            elsif Obj.Piece_Range = 0
            then
              Ui.Info("Fighter at " & Integer'Image(Obj.Loc) & " crashed and burned.");
              Objects.Kill_Obj(Obj, Obj.Loc);
            end if;
         end if;

         if Saved_Loc /= Obj.Loc
         then
            Changed_Loc := True;
         end if;
      end loop;

      -- if a boat is in port, damaged, and never moved, fix some damage
      if Obj.Hits > 0 and            --  still alive?  XXX again, can we get here otherwise?
        Piece_Attr(Obj.Piece_Type).Class = SHIP and
        Obj.Hits < Piece_Attr(Obj.Piece_Type).Max_Hits and
        User_Map(Obj.Loc).Contents = 'O'
      then
         Obj.Hits := Obj.Hits + 1;
      end if;
   end piece_Move;

   -- Move a piece at random.  We create a list of empty squares to which
   -- the piece can move.  If there are none, we do nothing, otherwise we
   -- move the piece to a random adjacent square.

   procedure Move_Random (Obj : in out Piece_Info_T) is
      Loc_List : array (0..7) of Location_T;
      NLoc : Integer := 0;
      Loc : Location_T;
      I : Integer;
   begin
      for I in Direction_T'Range
      loop
         Loc := Obj.Loc + Dir_Offset(I);
         if Objects.Good_Loc(Obj, Loc)
         then
            Loc_List(NLoc) := Loc;
            NLoc := NLoc + 1;
         end if;
      end loop;
      if Nloc = 0
      then
         return;                         --  no legal move
      end if;
      I := Math.Rand_Long(Nloc -1 );    --  randomize among found good directions
      Objects.Move_Obj(Obj, Loc_List(I));
   end Move_Random;

   -- Have a piece explore.  We look for the nearest unexplored territory
   -- which the piece can reach and have to piece move toward the
   -- territory.

   procedure Move_Explore(Obj : in out Piece_Info_T) is
      PMap : Path_Map;
      Loc : Location_T;
      Terrain : Acceptable_Terrain_Array;
   begin
      case Piece_Attr(Obj.Piece_Type).Class is
         when GROUND =>
            Mapping.Vmap_Find_Ground_Obj(Loc, PMap, User_Map, Obj.Loc, User_Army);
            Terrain := ('+' => True, others => False);
         when AIRCRAFT =>
            Mapping.Vmap_Find_Aircraft_Obj(Loc, PMap, User_Map, Obj.Loc, User_Fighter);
            Terrain := ('+'|'.'|'O' => True, others => False);
         when SHIP =>
            Mapping.Vmap_Find_Ship_Obj(Loc, PMap, User_Map, Obj.Loc, User_Ship);
            Terrain := ('.'|'O' => True, others => False);
         when SPACECRAFT =>
            raise Program_Error;        --  spacecraft can't have a function
      end case;

      if Loc = Obj.Loc
      then
         return;                        --  nothing to explore (that's reachable)
      end if;

      if User_Map(Loc).Contents = ' ' and PMap(Loc).Cost = 2
      then
         Mapping.Vmap_Mark_Adjacent(PMap, Obj.Loc);
      else
         Mapping.Vmap_Mark_Path(PMap, User_Map, Loc);
      end if;

      Mapping.Vmap_Find_Dir(Loc, PMap, User_Map, Obj.Loc, Terrain, (' ' => 1, others => 0));

      if Loc /= Obj.Loc
      then
         Objects.Move_Obj(Obj, Loc);
         end if;
   end Move_Explore;

   -- Move an army onto a transport when it arrives.  We scan around the
   -- army to find a non-full transport.  If one is present, we move the
   -- army to the transport and waken the army.  Otherwise, we wait until
   -- next turn.

   procedure Move_Transport (Obj : in out Piece_Info_T) is
      Loc : Location_T;
      Found : Boolean;
   begin
      -- look for an adjacent transport
      Objects.Find_Transport (USER, Obj.Loc, Found, Loc);

      if Found
      then
         Objects.Move_Obj(Obj, Loc);
         User_Obj_Func(Obj, NOFUNC);
      else
         Obj.Moved := Objects.Obj_Moves(Obj);
      end if;
   end Move_Transport;

   -- cempire had a move_armyload, defined as:

   -- Move an army toward the nearest loading transport.
   -- If there is an adjacent transport, move the army onto
   -- the transport, and awaken the army.

   -- but it was set to panic.
   -- and an empty stub of a reciprocal function, Tt_load, but it
   -- too was set to panic

   -- Move an army toward an attackable city or enemy army

   procedure Move_Armyattack (Obj : in out Piece_Info_T) is
      PMap : Path_Map;
      Loc : Location_T;
   begin
      if Obj.Piece_Type /= ARMY
      then
         raise Program_Error;
      end if;

      Mapping.Vmap_Find_Ground_Obj(Loc, PMap, User_Map, Obj.Loc, User_Army_Attack);

      if Loc = Obj.Loc
      then
         return;                        --  nothing to attack
      end if;

      Mapping.Vmap_Mark_Path(PMap, User_Map, Loc);
      Mapping.Vmap_Find_Dir(Loc, PMap, User_Map, Obj.Loc,
                            ('+' => True, others => False),
                            ('X' => 3, '*' => 2, 'a' => 1, others => 0));

      if Loc /= Obj.Loc
      then
         Objects.Move_Obj(Obj, Loc);
      end if;
   end Move_Armyattack;

   -- Move a ship toward port.  If the ship is healthy, wake it up.

   procedure Move_Repair (Obj : in out Piece_Info_T) is
      Pmap : Path_Map;
      Loc : Location_T;
   begin
      if Piece_Attr(Obj.Piece_Type).Class /= SHIP
      then
         raise Program_Error;
      end if;

      if Obj.Hits = Piece_Attr(Obj.Piece_Type).Max_Hits
      then
         -- we don't need repair
         User_Obj_Func(Obj, NOFUNC);
         return;
      end if;

      if User_Map(Obj.Loc).Contents = 'O' --  in port
      then
         Obj.Moved := Obj.Moved + 1;
         return;
      end if;

      Mapping.Vmap_Find_Ship_Obj(Loc, PMap, User_Map, Obj.Loc, User_Ship_Repair);

      if Loc = Obj.loc                  --  no reachable city (how?)
      then
         return;
      end if;

      Mapping.Vmap_Mark_Path(Pmap, User_Map, Loc);

      --  try to avoid land (literally, stay adjacent to ocean)
      Mapping.Vmap_Find_Dir(Loc, PMap, User_Map, Obj.Loc, ('.'|'O' => True, others => False), ('.' => 1, others => 0));
      if Loc /= Obj.Loc
      then
        Objects.Move_Obj(Obj, Loc);
      end if;
   end Move_Repair;

   -- Here we have a transport or carrier waiting to be filled.  If the
   -- object is not full, we set the move count to its maximum value.
   -- Otherwise we awaken the object.

   procedure Move_Fill (Obj : in out Piece_Info_T) is
   begin
      if Obj.Count = Objects.Obj_Capacity(Obj)
      then
         User_Obj_Func(Obj, NOFUNC);
      else
         Obj.Moved := Objects.Obj_Moves(Obj);
      end if;
   end Move_Fill;

      -- Here we have a piece that wants to land at the nearest carrier or
      -- owned city.  We scan through the lists of cities and carriers looking
      -- for the closest one.  We then move toward that item's location.
      -- The nearest landing field must be within the object's range.

   procedure Move_Land (Obj : in out Piece_Info_T) is
      Best_Dist, New_Dist : Integer;
      Best_Loc, Carrier_loc : Location_T;
      Carrier_Found : Boolean;
   begin
      Objects.Find_Nearest_City(Obj.Loc, USER, Best_Loc, Best_Dist);

      -- see if we can find a carrier closer than the nearest city
      Objects.Find_Carrier(USER, Obj.Loc, Carrier_Found, Carrier_Loc);
      if Carrier_Found
      then
         New_Dist := Math.Dist(Carrier_Loc, Obj.Loc);
         New_Dist := New_Dist + Carrier_Bias; --  see discussion in Awake()
         if New_Dist < Best_Dist
         then
            Best_Dist := New_Dist;
            Best_Loc := Carrier_Loc;
         end if;
      end if;

      if Best_Dist = 0                  --  we're already there
      then
         Obj.Moved := Obj.Moved + 1;    --  XXX why?
      elsif Best_Dist < Obj.Piece_Range
      then
         Move_To_Dest(Obj, Best_Loc);
      else
         User_Obj_Func(Obj, NOFUNC);   --  can't reach city or carrier
      end if;
   end Move_Land;

   -- Move a piece in the specified direction if possible.
   -- XXX If the object is an aircraft which has travelled for half its range,
   -- XXX we should wake it up. (original code's comment claimed to do so,
   -- XXX but no such code was present)

   procedure Move_Dir (Obj : in out Piece_Info_T) is
      Loc : Location_T;
      Dir : Direction_T;
   begin
      Dir := Move_Func_Directions(Obj.Func);
      Loc := Obj.Loc + Dir_Offset(Dir);

      if Objects.Good_Loc(Obj, Loc)
      then
          Objects.Move_Obj(Obj, Loc);
      end if;
   end Move_Dir;

   -- Move a piece toward a specified destination if possible.  For each
   -- direction, we see if moving in that direction would bring us closer
   -- to our destination, and if there is nothing in the way.  If so, we
   -- move in the first direction we find.

   procedure Move_Path (Obj : in out Piece_Info_T) is
   begin
      if Obj.Func /= MOVE_TO_DEST
      then
         raise Program_Error;
      end if;

      if Obj.Dest = Obj.Loc
      then
         User_Obj_Func(Obj, NOFUNC);    --  we're heeeeere
      else
         Move_To_Dest(Obj, Obj.Dest);
      end if;
   end Move_Path;

   -- Move a piece toward a specific destination.  We first map out
   -- the paths to the destination, if we can't get there, we return.
   -- Then we mark the paths to the destination.  Then we choose a
   -- move.

   procedure Move_To_Dest (Obj : in out Piece_Info_T; Dest : in Location_T) is
      PMap : Path_Map;
      Fterrain : Terrain_T;
      Mterrain : Acceptable_Terrain_Array;
      New_Loc : Location_T;
   begin
      case Piece_Attr(Obj.Piece_Type).Class is
         when GROUND =>
            Fterrain := T_LAND;
            Mterrain := ('+' => True, others => False);
         when AIRCRAFT =>
            Fterrain := T_AIR;
            Mterrain := ('+'|'.'|'O' => True, others => False);
         when SHIP =>
            Fterrain := T_WATER;
            Mterrain := ('.'|'O' => True, others => False);
         when SPACECRAFT =>
            raise Program_Error;        --  spacecraft can't make controlled moves
      end case;

      Mapping.Vmap_Find_Dest(New_Loc, PMap, User_Map, Obj.Loc, Dest, USER, Fterrain);

      if New_Loc = Obj.Loc
      then
         return;                        --  can't get there -- user_move will remove func setting
      end if;

      Mapping.Vmap_Mark_Path(PMap, User_Map, Dest);
      Mapping.Vmap_Find_Dir(New_Loc, PMap, User_Map, Obj.Loc, Mterrain, (' ' => 2, '.' => 1, others => 0));

      if New_Loc = Obj.loc              --  no good move along path
      then
         return;
      end if;

      if not Objects.Good_Loc(Obj, New_Loc)
      then
         raise Program_Error;
      end if;

      Objects.Move_Obj(Obj, New_Loc);
   end Move_To_Dest;

   -- Ask the user to move his own darn piece

   procedure Ask_User(Obj : in out Piece_Info_T) is
      C : Character;
   begin
      loop
         -- XXX original code did this here, too, then used
         -- second display_loc_u to `reposition cursor'.
         -- let's try without...
         -- Ui.Display_Loc_U(Obj.Loc);
         Ui.Describe_Obj(Obj.Loc, Obj);
         Ui.Display_Score;
         Ui.Display_Loc_U(Obj.Loc);        --  XXX XXX

         C := Ui.Get_Chx;               --  get command (no echo)

         -- XXX this should be table driven, esp. as it has to match
         -- XXX data in two places in empire.ads (help and func names)
         case C is
            when 'Q' =>
               User_Dir(Obj, NORTHWEST);
               return;                  --  so we don't loop
            when 'W' =>
               User_Dir(Obj, NORTH);
               return;
            when 'E' =>
               User_Dir(Obj, NORTHEAST);
               return;
            when 'D' =>
               User_Dir(Obj, EAST);
               return;
            when 'C' =>
               User_Dir(Obj, SOUTHEAST);
               return;
            when 'X' =>
               User_Dir(Obj, SOUTH);
               return;
            when 'Z' =>
               User_Dir(Obj, SOUTHWEST);
               return;
            when 'A' =>
              User_Dir(Obj, WEST);
              return;

            when 'J' =>
              Editing.Edit(Obj.Loc);
              Reset_Func(Obj);
              return;

            when 'V' =>
               User_Set_City_Func(Obj.Loc);
               Reset_Func(Obj);
               return;

            when ' ' =>
               User_Skip(Obj);
               return;
            when 'F' =>
               User_Obj_Func(Obj, FILL, (TRANSPORT|CARRIER => True, others => False));
               return;
            when 'I' =>
               User_Set_Dir(Obj);
               return;
            when 'R' =>
               User_Obj_Func(Obj, RANDOM);
               return;
            when 'S' =>
               User_Obj_Func(Obj, SENTRY);
               return;
            when 'L' =>
               User_Obj_Func(Obj, LAND, (FIGHTER => True, others => False));
               return;
            when 'G' =>
               User_Obj_Func(Obj, EXPLORE);
               return;
            when 'T' =>
               User_Obj_Func(Obj, WFTRANSPORT, (ARMY=> True, others => False));
               return;
            when 'U' =>
               User_Obj_Func(Obj, REPAIR, (ARMY|FIGHTER|SATELLITE => False, others => True));
               return;
            when 'Y' =>
               User_Obj_Func(Obj, ARMYATTACK, (ARMY => True, others => False));
               return;

            when 'B' =>
               User_Build(Obj.Loc);
               -- from here in we loop, thus asking user again after command is complete
            when '?' =>
               Ui.Help(Help_User);
            when 'K' =>
               User_Obj_Func(Obj, NOFUNC);
            when 'O' =>
               User_Cancel_Auto;
            when 'P'|Character'Val(12) =>
               Ui.Redraw;
            when '=' =>
               Ui.Describe_Obj(Obj.Loc, Obj);

            when others =>
               -- Uh.Huh, maybe?
               Ui.Alert;
         end case;
      end loop;
   end Ask_User;

   -- Here, if the passed object is on a city, we assign
   -- the city's function to the object.  However, we then awaken the
   -- object if necessary (because the user only changed the city
   -- function, and did not tell us what to do with the object).

   procedure Reset_Func(Obj : in out Piece_Info_T) is
      Cityp : City_Info_P;
      B : Boolean;
   begin
      Cityp := Objects.Find_City_At_Loc(Obj.Loc);

      if Cityp /= null and then Cityp.Func(Obj.Piece_Type) /= NOFUNC
      then
         User_Obj_Func(Obj, Cityp.Func(Obj.Piece_Type));
         Awake(Obj, B);
      end if;
   end Reset_Func;

   procedure User_Obj_Func
     (
      Obj : in out Piece_Info_T;
      Func : in Function_T;
      Ptypes : in Acceptable_Piece_Array := (SATELLITE => FALSE, others => TRUE);
      Dest_If_Move_To_Dest : Location_T := 0
     ) is
   begin
      if Ptypes(Obj.Piece_Type)
      then
         Obj.Func := Func;
         if Func = MOVE_TO_DEST
         then
            Obj.Dest := Dest_If_Move_To_Dest;
         end if;
      else
         -- XXX should be Ui.Huh, ala empire-editing.adb?
         Ui.Alert;
      end if;
   end User_Obj_Func;

   -- Increment the number of moves a piece has used.  If the piece
   -- is an army and the army is in a city, move the army to
   -- the city.

   procedure User_Skip(Obj : in out Piece_Info_T) is
   begin
      if Obj.Piece_Type = ARMY and User_Map(Obj.Loc).Contents = 'O'
      then
         Move_Army_To_City(Obj, Obj.Loc);
      else
         Obj.Moved := Obj.Moved + 1;
      end if;
   end User_Skip;

   -- Set an object's function to move in a certain direction.
   procedure User_Set_Dir (Obj : in out Piece_Info_T) is
      C : Character;
   begin
      C := Ui.Get_Chx;
      -- XXX should loop until valid result?
      case C is
         when 'Q' =>
            User_Obj_Func(Obj, MOVE_NW);
         when 'W' =>
            User_Obj_Func(Obj, MOVE_N);
         when 'E' =>
            User_Obj_Func(Obj, MOVE_NE);
         when 'D' =>
            User_Obj_Func(Obj, MOVE_E);
         when 'C' =>
            User_Obj_Func(Obj, Move_SE);
         when 'X' =>
            User_Obj_Func(Obj, MOVE_S);
         when 'Z' =>
            User_Obj_Func(Obj, MOVE_SW);
         when 'A' =>
            User_Obj_Func(Obj, MOVE_W);
         when others =>
            Ui.Alert;
      end case;
   end User_Set_Dir;


   -- Set a city's function
   -- XXX XXX XXX this is identical to E_City_Func, except that that func supports
   -- XXX XXX XXX a path.  they should be merged.

   procedure User_Set_City_Func (Loc : in Location_T) is
      Ptype : Piece_Type_T;
      E : Character;
      Cityp : City_Info_P;
   begin
      Cityp := Objects.Find_City_At_loc(Loc, (USER => True, Others => False));

      if Cityp = null
      then
         Ui.Alert;
         return;
      end if;

      Ptype := Objects.Get_Piece_Name;
      if Ptype = NOPIECE
      then
         -- XXX maybe we should raise program_error -- how did we get here?
         Ui.Alert;
         return;
      end if;

      -- XXX maybe a prompt?
      E := Ui.Get_Chx;

      case E is
         when 'F' =>
            if (Ptype = TRANSPORT) or (Ptype = CARRIER)
            then
               Editing.E_User_City_Func(Loc, FILL, Ptype);
            else
               Ui.Huh;
            end if;
         when 'G' =>
            Editing.E_User_City_Func(Loc, EXPLORE, Ptype);
         when 'I' =>
            Editing.E_City_Move_Direction(Loc, Ptype);
         when 'K' =>
            Editing.E_User_City_Func(Loc, NOFUNC, Ptype);
         when 'R' =>
            Editing.E_User_City_Func(Loc, RANDOM, Ptype);
         when 'U' =>
            Editing.E_City_Repair(Loc, Ptype);
         when 'Y' =>
            if Ptype = ARMY
            then
               Editing.E_User_City_Func(Loc, ARMYATTACK, Ptype);
            else
               Ui.Huh;
            end if;
         when others =>
            Ui.Huh;
      end case;
   end User_Set_City_Func;

   -- Change a city's production
   procedure User_Build (Loc : in Location_T) is
      Cityp : City_Info_P;
   begin
      Cityp := Objects.Find_City_At_Loc(Loc, (USER => True, others => False));

      if Cityp = null
      then
         Ui.Alert;
         return;
      end if;
      Objects.Set_Prod(Cityp.all);
   end User_Build;

   -- Move a piece in the direction specified by the user.
   -- This routine handles attacking objects.

   procedure User_Dir (Obj : in out Piece_Info_T; Dir : Direction_T) is
      Loc : Location_T;
   begin
      Loc := Obj.Loc + Dir_Offset(Dir);

      if Objects.Good_Loc(Obj, Loc)
      then
        Objects.Move_Obj(Obj, Loc);
        return;
      end if;
      if not Map(Loc).On_Board
      then
         Ui.Error("You cannot move off the edge of the world.");
         return;
      end if;

      case Piece_Attr(Obj.Piece_Type).Class is
         when GROUND =>
            User_Dir_Ground(Obj, Loc);
         when AIRCRAFT =>
            User_Dir_Aircraft(Obj, Loc);
         when SHIP =>
            User_Dir_Ship(Obj, Loc);
         when SPACECRAFT =>
            raise Program_Error;        --  satellites are never under user control
      end case;
   end User_Dir;

   -- We have an army that wants to attack something or move onto some
   -- unreasonable terrain.  We check for errors, question the user if
   -- necessary, and attack if necessary.

   procedure User_Dir_Ground (Obj : in out Piece_Info_T; Loc : in Location_T) is
   begin
      -- remember, if we get here, this is NOT an uncontested move -- so see
      -- what the problem is, and if it's an enemy, have at it.

      -- original code had terrible cutesy-ism -- various fatal actions were
      -- allowed, after a snarky yes/no prompt.  If we want this, we can add
      -- a `disband' command.

      if User_Map(Loc).Contents = 'O'   --  moving into own city
      then
         Move_Army_To_City(Obj, Loc);
      elsif User_Map(Loc).Contents = 'T'
      then
         -- if we got here, the transport is full
         Ui.Error("The transport at location " & Location_T'Image(Loc) & " is full.");
      elsif Map(Loc).Contents = '.'
      then
         -- more cutesy-ism was here.  We die if we walk out to sea, but if there was
         -- an enemy there, we get to attack him first.
         -- XXX this is gone, but some form of army attack out to sea might be worthwhile
         Ui.Error("Troops can't walk on water, sir."); --  message apparently from Craig Hansen
      elsif User_Content(User_Map(Loc).Contents)
      then
        -- XXX XXX XXX ideally, we could coexist with a satellite or airborne aircraft, actually
        Ui.Error("Sir, those are our own men.  We can't attack them!");
      else
         Attack.Attack (obj, loc);
      end if;
   end User_Dir_Ground;

   -- Here we have a fighter wanting to attack something.  There are only
   --  three cases:  attacking a city, attacking ourself, attacking the enemy.
   -- this had the same cutesy-ism as above in the original

   procedure User_Dir_Aircraft (Obj : in out Piece_Info_T; Loc : Location_T) is
   begin
      if Map(Loc).Contents = '*'
      then
         Ui.Error("The air defenses over the city at " & Location_T'Image(Loc) & " are too strong, we can't go there.");
      elsif User_Content(User_Map(Loc).Contents)
      then
        -- XXX XXX XXX ideally, we could coexist with anything but another aircraft
        Ui.Error("Sir, those are our own men.  We can't attack them!");
      else
         Attack.Attack(Obj, Loc);
      end if;
   end User_Dir_Aircraft;

   -- Here we have a ship attacking something, or trying to move on
   -- shore.  Our cases are: moving ashore (and subcases), attacking
   -- a city, attacking self, attacking enemy.
   -- needless by now to say, this had the same cutesy-ism as the other two

   procedure User_Dir_Ship (Obj : in out Piece_Info_T; Loc : in Location_T) is
   begin
      if Map(Loc).Contents = '*'
      then
         Ui.Error("The port defenses of the city at " & Location_T'Image(Loc) & " are too strong, we can't go there.");
      elsif Map(Loc).Contents = '+'
      then
         Ui.Error("Ships need sea to float sir.  We can't go ashore.");
         -- XXX original code let any ship attack a shore unit, but the ship always died.  What
         -- XXX we really want is to allow a battleship or destroyer to bombard the shore (costs one move,
         -- XXX doesn't move the ship).
      elsif User_Content(User_Map(Loc).Contents)
      then
        -- XXX XXX XXX ideally, we could coexist with a satellite or airborne aircraft, actually
        Ui.Error("Sir, those are our own men.  We can't attack them!");
      else
         Attack.Attack(Obj, Loc);
      end if;
   end User_Dir_Ship;

   -- Here a user wants to move an army to a city.  If the city contains
   -- a non-full transport, we make the move.  Yes, there was more cutesy-ism.

   procedure Move_Army_To_City (Obj : in out Piece_Info_T; Loc : Location_T) is
      Tt : Piece_Info_P;
   begin
      Tt := Objects.Find_Obj_At_Loc(Loc, (TRANSPORT => True, others => False),
                                    (USER => True, others => False), True);

      if Tt /= null
      then
         Objects.Move_Obj(Obj, Loc);
      else
         Ui.Error("There's no garrison room left in the city at " & Location_T'Image(Loc) &
                    " and no transport waiting to load.");
      end if;
   end Move_Army_To_City;

   -- Cancel automove mode

   procedure User_Cancel_Auto is
   begin
      if not Automove
      then
         Ui.Error("Not in auto mode.");
      else
         Automove := False;
         Ui.Info("Auto mode cancelled.");
      end if;
   end User_Cancel_Auto;

   --  Awaken an object if it needs to be.  Normally, objects are awakened
   -- when they are next to an enemy object or an unowned city.  Armies
   -- on troop transports are not awakened if they are surrounded by sea.
   --  We return true if the object is now awake.  Objects are never
   -- completely awoken here if their function is a destination.  But we
   -- will return TRUE if we want the user to have control.

   procedure Awake (Obj : in out Piece_Info_T; Awoken : out Boolean) is
      C : Content_Display_T;
   begin
      if Piece_Attr(Obj.Piece_Type).Class = GROUND and Mapping.Vmap_At_Sea(User_Map, Obj.Loc)
      then
         Obj.Moved := Objects.Obj_Moves(Obj);
         Awoken := False;
         return;
      end if;

      if Obj.Func = NOFUNC
      then
         Awoken := True;
         return;                        --  object was already awake
      end if;

      if Piece_Attr(Obj.Piece_Type).Class = AIRCRAFT and Obj.Func /= Land
      then
      -- original didn't consider carriers, and didn't consider aircraft with
      -- a destination.
      -- XXX this still isn't quite right -- a carrier might have room now, and be
      -- XXX in range now, but not by the time we get there.  We try to compensate
      -- XXX for this by biasing against carriers, but the right solution, IMO, is
      -- XXX to have an AC which is manually landed on a carrier `bind' to that carrier
      -- XXX, and always take range ONLY to that specific carrier until manually landed
      -- XXX elsewhere
         declare
            Refuel_Loc : Location_T;
            Refuel_Range : Integer;
            Carrier_Found : Boolean;
            Carrier_Loc : Location_T;
            Carrier_Range : Integer;
         begin
            Objects.Find_Nearest_City(Obj.Loc, USER, Refuel_Loc, Refuel_Range);
            Objects.Find_Carrier(USER, Obj.Loc, Carrier_Found, Carrier_loc);
            if Carrier_Found
            then
               Carrier_Range := Math.Dist(Obj.Loc, Carrier_Loc) + Carrier_Bias;
               if Carrier_Range < Refuel_Range
               then
                  Refuel_Range := Carrier_Range;
               end if;
            end if;
            if Obj.Piece_Range <= Refuel_Range + 2
            then
               User_Obj_Func(Obj, NOFUNC);
               Awoken := True;
               return;
              -- XXX XXX XXX maybe we should instead go for the fuel (LOAD) we know we need
              -- XXX XXX XXX otherwise, we should really alert the user, or they may move in
              -- the wrong direction, and be stranded
            end if;
         end;
      end if;

      for I in Direction_T'Range
      loop
         C := User_Map(Obj.Loc + Dir_Offset(I)).Contents;
         if C = '*' or Comp_Content(C)
         then
            if Obj.Func /= MOVE_TO_DEST
            then
               User_Obj_Func(Obj, NOFUNC);
               Awoken := True;
               return;
            end if;
         end if;
      end loop;

      Awoken := False;
      return;
   end Awake;

end Empire.User_Move;
