--  routines for manipulating objects

with Empire.Lists;
with Empire.Math;
with Empire.Ui;
with Empire.Utility;

package body Empire.Objects is

   --  Given the location of a city, return the index of that city

   function Find_City_At_Loc
     (Loc    : in Location_T;
      Owners : in Acceptable_Owner_Array :=
        (others => True))
      return City_Info_P
   is
   begin
      if Map (Loc).CityP /= null and then Owners (Map (Loc).CityP.Owner) then
         return Map (Loc).CityP;
      else
         return null;
      end if;
   end Find_City_At_Loc;

   --  Find the nearest city to a location. Return the location of the city
   --  and the estimated cost to reach the city. Distances are computed as
   --  straight-line distances.

   procedure Find_Nearest_City
     (Loc            : in     Location_T;
      Owner          : in     Owner_T;
      Found          :    out Boolean;
      City_Loc       :    out Location_T;
      Estimated_Cost :    out Integer)
   is
      Best_Dist : Integer    := INFINITY;
      New_dist  : Integer;
      Best_Loc  : Location_T := Loc;
   begin
      --  will end up true unless owner has no cities left
      Found := False;
      for I in City'Range loop
         if City (I).Owner = Owner then
            New_dist := Math.Dist (Loc, City (I).Loc);
            if New_dist < Best_Dist then
               Best_Dist := New_dist;
               Best_Loc  := City (I).Loc;
            end if;
         end if;
      end loop;
      City_Loc       := Best_Loc;
      Estimated_Cost := Best_Dist;
      if Estimated_Cost < INFINITY then
         Found := True;
      end if;
   end Find_Nearest_City;

   --  Search a list of objects at a location for any kind of object. We prefer
   --  transports and carriers to other objects.

   function Find_Obj_At_Loc
     (Loc   : in Location_T;
      Types : in Acceptable_Piece_Array :=
        (SATELLITE => False, others => True);
      Owners      : Acceptable_Owner_Array := (others => True);
      Ignore_Full : Boolean                := False) return Piece_Info_P
   is
      Best : Piece_Info_P := null;
      P    : Piece_Info_P;
   begin
      P := Map (Loc).ObjP;
      while P /= null loop
         if Types (P.Piece_Type) and Owners (P.Owner) then
            if Ignore_Full = False or P.Count < Obj_Capacity (P) then
               --  if there are multiple acceptable objects at loc, pick the
               --  highest value one XXX XXX `preference' is by location in
               --  definition of Piece_Type_T. This should XXX XXX change to
               --  a tunable piece_value_array XXX XXX XXX is this actually
               --  useful? Do we actually count on finding XXX XXX XXX highest
               --  value obj at loc? (probably, but check)

               if Best = null or else P.Piece_Type > Best.Piece_Type then
                  Best := P;
               end if;
            end if;
         end if;
         P := P.Links (Loc_Link).Next;
      end loop;

      return Best;
   end Find_Obj_At_Loc;

   procedure Find_Nearest_Obj
     (Loc   : in Location_T;
      Owner : in Owner_T;
      Types : in Acceptable_Piece_Array :=
        (SATELLITE => False, others => True);
      Max_Range      : in     Integer := INFINITY;
      Ignore_Full    : in     Boolean := False;
      Found          :    out Boolean;
      Obj_Loc        :    out Location_T;
      Estimated_Cost :    out Integer)
   is
      P         : Piece_Info_P;
      Dist      : Integer;
      Best_Loc  : Location_T   := Loc;
      Best_Dist : Integer      := INFINITY;
      Best_Obj  : Piece_Info_P := null;
   begin
      --  will end up true unless Owner has no pieces left (matching criteria)
      Found := False;

      for I in Object'Range loop
         P := Object (I);

         if P.Hits > 0 and
           P.Owner = Owner and
           Types (P.Piece_Type) and
           (Ignore_Full = False or else P.Count < Obj_Capacity (P))
         then
            Dist := Math.Dist (Loc, P.Loc);
            if Dist < Best_Dist and Dist <= Max_Range then
               --  as with Find_Obj_At_Loc, make sure we pick the `best' object
               --  at location
               if Best_Obj = null
                 or else
                 (P.Loc /= Best_Loc or P.Piece_Type > Best_Obj.Piece_Type)
               then
                  Best_Obj  := P;
                  Best_Dist := Dist;
                  Best_Loc  := P.Loc;
               end if;
            end if;
         end if;
      end loop;

      Obj_Loc        := Best_Loc;
      Estimated_Cost := Best_Dist;
      if Estimated_Cost < INFINITY then
         Found := True;
      end if;
   end Find_Nearest_Obj;

   --  Return the number of moves an object gets to make. This amount is
   --  based on the damage the object has suffered and the number of moves it
   --  normally gets to make. The object always gets to make at least one move,
   --  assuming it is not dead. Damaged objects move at a fraction of their
   --  normal speed. An object which has lost half of its hits moves at
   --  half-speed, for example.

   --  XXX XXX XXX I've fixed several places where this wasn't being used XXX
   --  XXX XXX (i.e. unmodified base speed was always used). This needs XXX XXX
   --  XXX to be checked, and ideally made private to force use of XXX XXX XXX
   --  this func.

   function Obj_Moves (Obj : in Piece_Info_P) return Integer is
   begin
      return
        (Piece_Attr (Obj.Piece_Type).Speed * Obj.Hits +
         Piece_Attr (Obj.Piece_Type).Max_Hits -
         1) /
        Piece_Attr (Obj.Piece_Type).Max_Hits;
   end Obj_Moves;

   --  Figure out the capacity for an object XXX XXX XXX need to check, as with
   --  obj_moves, for places where raw capacity is being used, and XXX XXX XXX
   --  fix them, then make the raw capacity private.

   function Obj_Capacity (Obj : in Piece_Info_P) return Integer is
   begin
      return
        (Piece_Attr (Obj.Piece_Type).Capacity * Obj.Hits +
         Piece_Attr (Obj.Piece_Type).Max_Hits -
         1) /
        Piece_Attr (Obj.Piece_Type).Max_Hits;
   end Obj_Capacity;

   --  If an object is on a ship, remove it from that ship

   procedure Disembark (Obj : in Piece_Info_P) is
   begin
      if Obj.Ship /= null then
         Lists.Unlink (Obj.Ship.Cargo, Obj, Cargo_Link);
         Obj.Ship.Count := Obj.Ship.Count - 1;
         Obj.Ship       := null;
      end if;
   end Disembark;

   --  Move an object onto a ship

   procedure Embark (Ship : in Piece_Info_P; Obj : in Piece_Info_P) is
   begin
      --  XXX XXX XXX should check that we have room here, not just in caller!
      Obj.Ship := Ship;
      Lists.Link (Ship.Cargo, Obj, Cargo_Link);
      Ship.Count := Ship.Count + 1;
   end Embark;

   --  Kill an object. We scan around the piece and free it. If there is
   --  anything in the object, it is killed as well.

   procedure Kill_Obj (Obj : in Piece_Info_P; Loc : Location_T) is
   begin
      while Obj.Cargo /= null    --  kill contents
      loop
         Kill_One (Obj.Cargo);
      end loop;
      Kill_One (Obj);

      --  XXX XXX XXX many callers duplicate this, IIRC
      Scan (Obj.Owner, Loc);
   end Kill_Obj;

   --  kill an object without scanning
   procedure Kill_One (Obj : in Piece_Info_P) is
   begin
      Disembark (Obj);
      Obj.Hits  := 0;
      Obj.Moved :=
        Piece_Attr (Obj.Piece_Type).Speed; --  XXX XXX XXX should not be needed

      case Obj.Owner is
         when USER =>
            Lists.Unlink (User_Obj (Obj.Piece_Type), Obj, Piece_Link);
         when COMP =>
            Lists.Unlink (Comp_Obj (Obj.Piece_Type), Obj, Piece_Link);
      end case;

      Lists.Unlink (Map (Obj.Loc).ObjP, Obj, Loc_Link);
      Lists.Link (Free_List, Obj, Piece_Link);
   end Kill_One;

   --  Kill a city. We kill off all objects in the city and set its type to
   --  unowned. We scan around the city's location.

   procedure Kill_City (City : in out City_Info_T) is
      P, Next_P : Piece_Info_P;
   begin
      --  XXX XXX XXX right now, armies at a location are destroyed, while XXX
      --  XXX XXX other hardware is captured. This works well to discourage XXX
      --  XXX XXX big-ticket building near the front, but may still be too XXX
      --  XXX XXX harsh
      P := Map (City.Loc).ObjP;
      while P /= null loop
         --  stash next link, in case we kill this object in between
         Next_P := P.Links (Loc_Link).Next;
         case Piece_Attr (P.Piece_Type).Class is
            when GROUND | AIRCRAFT =>
               --  NOTA BENE: original code allowed aircraft to be captured too
               --  I don't like that idea (I'm iffy on ships, too)
               Kill_Obj (P, City.Loc);
            when SHIP =>
               while P.Cargo /= null
               loop
                  --  original ran only for TRANSPORT.  since we now also
                  --  kill fighters, run for any
                  Kill_One (P.Cargo);
               end loop;
               case P.Owner is
                  when USER =>
                     Lists.Unlink (User_Obj (P.Piece_Type), P, Piece_Link);
                     P.Owner := COMP;
                     Lists.Link (Comp_Obj (P.Piece_Type), P, Piece_Link);
                     P.Func := NOFUNC;
                  when COMP =>
                     Lists.Unlink (User_Obj (P.Piece_Type), P, Piece_Link);
                     P.Owner := COMP;
                     Lists.Link (Comp_Obj (P.Piece_Type), P, Piece_Link);
                     P.Func := NOFUNC;
               end case;
            when SPACECRAFT =>
               null;
         end case;

         P := Next_P;
      end loop;

      if City.Owner /= UNOWNED then
         Scan (City.Owner, City.Loc);
      end if;
      City.Owner := UNOWNED;         --  caller sets owner next
      City.work  := 0;
      City.Prod  := NOPIECE;

      for I in Piece_Type_T'Range loop
         City.Func (I) := NOFUNC;
      end loop;
   end Kill_City;

   --  Produce an item for a city
   procedure Produce (City : in out City_Info_T) is
      New_obj : Piece_Info_P;
      Sat_Dir : constant array (Integer range 0 .. 3) of Function_T :=
        (MOVE_NW, MOVE_SW, MOVE_NE, MOVE_SE);
   begin
      --  XXX this can only result in non-zero in the re-tooling case XXX
      --  probably a fairer handling of this case to always revert to XXX
      --  zero after production
      City.work := City.work - Piece_Attr (City.Prod).Build_Time;

      --  XXX of course, the static storage model should go at some point
      --  XXX (either make free_list growable, or allocate all dynamically,
      --  XXX allowing us to move away from 'access all' ptrs)
      if Free_List = null then
         raise Storage_Error;
      end if;

      New_obj := Free_List;
      Lists.Unlink (Free_List, New_obj, Piece_Link);
      case City.Owner is
         when USER =>
            Lists.Link (User_Obj (City.Prod), New_obj, Piece_Link);
         when COMP =>
            Lists.Link (Comp_Obj (City.Prod), New_obj, Piece_Link);
         when UNOWNED =>
            Utility.Panic ("Producing an object in an unowned city");
      end case;
      Lists.Link (Map (City.Loc).ObjP, New_obj, Loc_Link);

      --  XXX this should be hidden in empire.lists
      New_obj.Links (Cargo_Link).Next := null;
      New_obj.Links (Cargo_Link).Prev := null;

      New_obj.Loc         := City.Loc;
      --  if city has a func for the obj, this will be handled by caller
      New_obj.Func        := NOFUNC;
      New_obj.Hits        := Piece_Attr (City.Prod).Max_Hits;
      New_obj.Owner       := City.Owner;
      New_obj.Piece_Type  := City.Prod;
      New_obj.Moved       := 0;
      New_obj.Cargo       := null;
      New_obj.Ship        := null;
      New_obj.Count       := 0;
      New_obj.Piece_Range := Piece_Attr (City.Prod).Piece_Range;

      if New_obj.Piece_Type = SATELLITE then
         --  XXX XXX XXX new satellites get a random direction. this isn't
         --  really ideal -- the user should XXX XXX XXX get to choose, as
         --  otherwise it is not so useful to make multiple satellites at
         --  one XXX XXX XXX location (and all the effort which went into a
         --  satellite can be lost if it goes in XXX XXX XXX a more-or-less
         --  useless direction
         New_obj.Func := Sat_Dir (Math.Rand_Long (4));
      end if;
   end Produce;

   --  Move an object to a location. We mark the object moved, we move the
   --  object to the new square, and we scan around the object. We also do
   --  lots of little maintenance like updating the range of an object,
   --  keeping track of the number of pieces on a boat, etc.

   procedure Move_Obj (Obj : in Piece_Info_P; New_Loc : Location_T) is
      Old_Loc : Location_T;
      P       : Piece_Info_P;
   begin
      if Obj.Hits = 0 then
         raise Program_Error;
      end if;
      --      vmap = MAP(obj->owner);

      Old_Loc         := Obj.Loc;
      Obj.Moved       := Obj.Moved + 1;
      Obj.Loc         := New_Loc;
      Obj.Piece_Range := Obj.Piece_Range - 1;

      Disembark
        (Obj);                   --  if we were on a ship, we aren't any more

      Lists.Unlink (Map (Old_Loc).ObjP, Obj, Loc_Link);
      Lists.Link (Map (New_Loc).ObjP, Obj, Loc_Link);

      --  move any objects contained in object XXX XXX XXX hmmmmmmm.... is it
      --  really necessary for contained objects to be linked to map?
      P := Obj.Cargo;
      while P /= null loop
         P.Loc := New_Loc;
         Lists.Unlink (Map (Old_Loc).ObjP, P, Loc_Link);
         Lists.Link (Map (New_Loc).ObjP, P, Loc_Link);
         P := P.Links (Cargo_Link).Next;
      end loop;

      case Obj.Piece_Type is
         when FIGHTER =>
            if Find_City_At_Loc (Obj.Loc) = null then
               --  fighters prefer a city to a carrier
               P :=
                 Find_Obj_At_Loc
                   (Obj.Loc,
                    (CARRIER => True, others => False),
                    Ignore_Full => True);
               if P /= null then
                  Embark (P, Obj);
               end if;
            end if;
         when ARMY =>
            --  armies aren't allowed to stay in cities, so embark even if one
            P :=
              Find_Obj_At_Loc
                (Obj.Loc,
                 (TRANSPORT => True, others => False),
                 Ignore_Full => True);
            if P /= null then
               Embark (P, Obj);
            end if;
         when others =>
            null;
      end case;

      if Piece_Attr (Obj.Piece_Type).Class = SPACECRAFT then
         Scan_Sat (Obj.Owner, Obj.Loc);
      end if;
      --  needed even in satellite case?
      Scan (Obj.Owner, Obj.Loc);
   end Move_Obj;

   --  Move a satellite. It moves according to the preset direction. Satellites
   --  bounce off the edge of the board.
   --
   --  We start off with some preliminary routines.

   --  Return next direction for a sattellite to travel

   function Bounce
     (Loc              : in Location_T;
      Dir1, Dir2, Dir3 : in Direction_T) return Direction_T
   is
      New_Loc : Location_T;
   begin
      New_Loc := Loc + Dir_Offset (Dir1);
      if Map (New_Loc).On_Board then
         return Dir1;
      end if;

      New_Loc := Loc + Dir_Offset (Dir2);
      if Map (New_Loc).On_Board then
         return Dir2;
      end if;

      return Dir3;
   end Bounce;

   --  Move a satellite one square

   procedure Move_Sat1 (Obj : in Piece_Info_P) is
      Dir     : Direction_T;
      New_Loc : Location_T;
   begin
      Dir     := Move_Func_Directions (Obj.Func);
      New_Loc := Obj.Loc + Dir_Offset (Dir);

      if not Map (New_Loc).On_Board then
         case Obj.Func is
            when MOVE_NE =>
               Dir := Bounce (Obj.Loc, NORTHWEST, SOUTHEAST, SOUTHWEST);
            when MOVE_NW =>
               Dir := Bounce (Obj.Loc, NORTHEAST, SOUTHWEST, SOUTHEAST);
            when MOVE_SE =>
               Dir := Bounce (Obj.Loc, SOUTHWEST, NORTHEAST, NORTHWEST);
            when MOVE_SW =>
               Dir := Bounce (Obj.Loc, SOUTHEAST, NORTHWEST, NORTHEAST);
            when others =>
               raise Program_Error;     --  satellite not moving diagonall
         end case;
         Obj.Func := Move_Dir_Functions (Dir);
         New_Loc  := Obj.Loc + Dir_Offset (Dir);
      end if;

      Move_Obj (Obj, New_Loc);
   end Move_Sat1;

   --  Now move the satellite all of its squares. Satellite burns iff it's
   --  range reaches zero.

   procedure Move_Sat (Obj : in Piece_Info_P) is
   begin
      if Piece_Attr (Obj.Piece_Type).Class /= SPACECRAFT then
         raise Program_Error;
      end if;

      --  XXX shouldn't this always be true on entry? or no?
      Obj.Moved := 0;

      while Obj.Moved < Obj_Moves (Obj) loop
         Move_Sat1 (Obj);
         if Obj.Piece_Range = 0 then
            if Obj.Owner = USER then
               Ui.Info
                 ("Satellite at " &
                  Location_T'Image (Obj.Loc) &
                  " ran out of fuel and burned up on re-entry.");
            end if;
            Kill_Obj (Obj, Obj.Loc);
         end if;
      end loop;
   end Move_Sat;

   --  Return true if a piece can move to a specified location. We are passed
   --  the object and the location. The location must be on the board, and
   --  the player's view map must have an appropriate terrain type for the
   --  location. Boats may move into port, armies may move onto transports,
   --  and fighters may move onto cities or carriers.

   function Good_Loc
     (Obj : in Piece_Info_P;
      Loc : in Location_T) return Boolean
   is
      C : Content_Display_T;
      P : Piece_Info_P;
   begin
      if not Map (Loc).On_Board then
         return False;
      end if;

      C := View (Obj.Owner) (Loc).Contents;

      if C = '.' or
        C = '+'
      then                             --  empty terrain of given type
         return Piece_Attr (Obj.Piece_Type).Terrain (C);
      end if;

      --  armies can move into unfull transports
      if Obj.Piece_Type = ARMY then
         P :=
           Find_Obj_At_Loc
             (Loc,
              (TRANSPORT => True, others => False),
              Ignore_Full => True);
         --  XXX easier to check owner here than to pass dynamic
         --  acceptable_owner_array above?
         if P /= null and then P.Owner = Obj.Owner then
            return True;
         else
            return False;     --  armies can't move into cities, so end here
         end if;
      end if;

--  ships and fighters can enter cities, and fighters prefer cities to carriers
      if Map (Loc).CityP /= null
        and then Map (Loc).CityP.Owner = Obj.Owner
      then
         return True;
      end if;

      if Obj.Piece_Type = FIGHTER then
         P :=
           Find_Obj_At_Loc
             (Loc,
              (CARRIER => True, others => False),
              Ignore_Full => True);
         --  XXX easier to check owner here than to pass dynamic
         --  acceptable_owner_array above?
         if P /= null and then P.Owner = Obj.Owner then
            return True;
         end if;
      end if;

      return False;
   end Good_Loc;

   procedure Describe_Obj (Obj : in Piece_Info_P) is
      Dest  : Bstring;
      Other : Bstring;
   begin

      if Obj.Func = MOVE_TO_DEST then
         --  XXX XXX XXX should convert to row x column, here and elsewhere
         Dest :=
           Strings.To_Bounded_String
             (" (" & Location_T'Image (Obj.Dest) & ")");
      else
         Dest := Strings.To_Bounded_String ("");
      end if;

      --  XXX XXX may be able to dispatch on class for some of this
      case Obj.Piece_Type is
         when FIGHTER | SATELLITE =>
            Other :=
              Strings.To_Bounded_String
                ("; fuel = " & Integer'Image (Obj.Piece_Range));
         when TRANSPORT =>
            Other :=
              Strings.To_Bounded_String
                ("; armies = " & Integer'Image (Obj.Count));
         when CARRIER =>
            Other :=
              Strings.To_Bounded_String
                ("; fighters = " & Integer'Image (Obj.Count));
         when others =>
            Other := Strings.To_Bounded_String ("");
      end case;

      Ui.Prompt
        (Strings.To_String (Piece_Attr (Obj.Piece_Type).Name) &
         " at " &
         Location_T'Image (Obj.Loc) &
         ": moves = " &
         Integer'Image (Obj_Moves (Obj) - Obj.Moved) &
         "; hits = " &
         Integer'Image (Obj.Hits) &
         "; func = " &
         Strings.To_String (Function_Name (Obj.Func)) &
         Strings.To_String (Dest) &
         Strings.To_String (Other));
   end Describe_Obj;

   --  Display info on a city
   procedure Describe_City (City : in City_Info_T) is
      Objp : Piece_Info_P;

      Aircraft_Count : Integer := 0;
      Ship_Count     : Integer := 0;

      Content : Bstring;
      Func    : Bstring;
      Prod    : Bstring;
   begin
      Objp := Map (City.Loc).ObjP;
      while Objp /= null loop
         case Piece_Attr (Objp.Piece_Type).Class is
            when AIRCRAFT =>
               --  XXX XXX in truth, when we have more than one type of A/C,
               --  we'll probably want to differentiate here...
               Aircraft_Count := Aircraft_Count + 1;
            when SHIP =>
               Ship_Count := Ship_Count + 1;
            when others =>
               null;
         end case;

         Objp := Objp.Links (Loc_Link).Next;
      end loop;

      Content :=
        Strings.To_Bounded_String
          (Integer'Image (Aircraft_Count) & " aircraft landed, ");

      Strings.Append (Content, Integer'Image (Ship_Count) & " ship");
      if Ship_Count /= 1 then
         Strings.Append (Content, 's');
      end if;
      Strings.Append (Content, " docked");

      Func := Strings.To_Bounded_String ("");
      for I in Piece_Type_T'Range loop
         if City.Func (I) /= NOFUNC then
            Strings.Append
              (Func,
               Content_Display_T'Image (Piece_Attr (I).U_Cont));
            Strings.Append (Func, ':');
            Strings.Append (Func, Function_Name (City.Func (I)));
         end if;

         if City.Func (I) = MOVE_TO_DEST then
            Strings.Append (Func, " (");
            Strings.Append (Func, Location_T'Image (City.Dest (I)));
            Strings.Append (Func, ")");
         end if;

         if City.Func (I) /= NOFUNC and I /= Piece_Type_T'Last then
            --  avoid a trailing ';', or '; ;'
            Strings.Append (Func, "; ");
         end if;
      end loop;

      Prod :=
        Strings.To_Bounded_String
          ("City at location " &
           Location_T'Image (City.Loc) &
           " will complete " &
           Strings.To_String (Piece_Attr (City.Prod).Article) &
           " on round " &
           Integer'Image
             (Date + Piece_Attr (City.Prod).Build_Time - City.work));

      Ui.Info (Strings.To_String (Prod));
      Ui.Info (Strings.To_String (Content));
      Ui.Info (Strings.To_String (Func));
   end Describe_City;

--  Scan around a location to update a player's view of the world. For each
--  surrounding cell, we remember the date the cell was examined, and the
--  contents of the cell. Notice how we carefully update the cell to first
--  reflect land, water, or city, then army or fighter, then boat, and finally
--  city owner. This guarantees that the object we want to display will appear
--  on top.

   procedure Scan (Owner : in Piece_Owner_T; Loc : in Location_T) is
      New_Loc : Location_T;
   begin
      Utility.Check;

      if not Map (Loc).On_Board then
         raise Program_Error;
      end if;

      for D in Direction_T'Range loop
         New_Loc := Loc + Dir_Offset (D);
         Update (Owner, New_Loc);
      end loop;
      --  update current location as well (e.g. if we are updating a city we
      --  just lost)
      Update (Owner, Loc);
   end Scan;

   --  Scan a portion of the board on behalf of a satellite
   procedure Scan_Sat (Owner : in Piece_Owner_T; Loc : in Location_T) is
      New_Loc : Location_T;
   begin
      if not Map (Loc).On_Board then
         raise Program_Error;
      end if;

      for D in Direction_T'Range loop
         begin
            New_Loc := Loc + 2 * Dir_Offset (D);
            if Map (New_Loc).On_Board then
               Scan (Owner, Loc);
            end if;
         exception
            when Constraint_Error =>
               null; --  2 * dir_offset took us off-offboard
         end;
      end loop;

      Scan (Owner, Loc);
   end Scan_Sat;

   --  Update a location. We set the date seen, the land type, object contents
   --  starting with armies, then fighters, then boats, and the city type.

   procedure Update (Owner : in Piece_Owner_T; Loc : in Location_T) is
      P : Piece_Info_P;
      C : City_Info_P;
   begin
      C := Find_City_At_Loc (Loc);
      P := Find_Obj_At_Loc (Loc);

      View (Owner) (Loc).Seen := Date;

      if C /= null then
         View (Owner) (Loc).Contents := City_char (C.Owner);
      elsif P /= null then
         case P.Owner is
            when USER =>
               View (Owner) (Loc).Contents := Piece_Attr (P.Piece_Type).U_Cont;
            when COMP =>
               View (Owner) (Loc).Contents := Piece_Attr (P.Piece_Type).C_Cont;
         end case;
      else
         View (Owner) (Loc).Contents := Map (Loc).Contents;
      end if;

      Ui.Display_Locx (Owner, Loc);
   end Update;

   --  Ask the production for a city. We make sure the city is displayed on the
   --  screen, and we ask the user for the new production. We keep asking until
   --  we get a valid answer.

   procedure Ask_Prod (City : in out City_Info_T) is
      T : Piece_Choice_T;
   begin
      Scan (USER, City.Loc);         --  in case something has moved away
      Ui.Display_Loc (USER, City.Loc);

      loop
         Ui.Prompt
           ("What do you want the city at " &
            Location_T'Image (City.Loc) &
            " to produce? ");
         T := Ui.Get_Piece_Name;

         if T = NOPIECE then
             --  XXX kinda snarky.  consider changing. (esp. as letters are
             --  XXX unclear!
            Ui.Error ("I don't know how to build those.");
         else
            City.Prod := T;
            --  retooling time
            City.work := -(Piece_Attr (T).Build_Time / RETOOLING_DENOMINATOR);
            exit;
         end if;
      end loop;
   end Ask_Prod;

end Empire.Objects;
