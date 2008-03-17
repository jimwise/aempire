-- Routines to handle edit mode commands

with Empire.Locations;
with Empire.Objects;
with Empire.Ui;

package body Empire.Editing is

   procedure Edit (Cur_Cursor : in Location_T) is
      Path : Path_T;
      Cmd : Character;
      Edit_Cursor : Location_T := Cur_Cursor;
   begin

      loop                              -- until user exits editing mode
         Ui.Display_Loc(USER, Edit_Cursor); -- position cursor
         E_Cursor(Edit_Cursor, Cmd);    -- handle cursor movement

         case Cmd is
            when 'B' =>                 -- change city production
               E_Prod(Edit_Cursor);
            when 'F' =>
               E_User_Obj_Func(Edit_Cursor, FILL);
            when 'G' =>
               E_User_Obj_Func(Edit_Cursor, EXPLORE);
            when 'I' =>
               E_Move_Direction(Edit_Cursor);
            when 'K' =>                 -- wake up
               E_Wake(Edit_Cursor);
            when 'L' =>
               E_User_Obj_Func(Edit_Cursor, LAND);
            when  'M' =>                -- start to set move destination
               E_Start_Path(Path, Edit_Cursor, NOPIECE);
            when 'N' =>                 -- finish setting move destination
               E_End_Path(Path, Edit_Cursor);
            when 'O' =>                 -- leave edit mode
               return;
            when 'P' =>                 -- print_new_sector
               E_Goto_Sector(Edit_Cursor);
            when 'R' =>
               E_User_Obj_Func(Edit_Cursor, RANDOM);
            when 'S' =>
               E_User_Obj_Func(Edit_Cursor, SENTRY);
            when 'T' =>
               E_User_Obj_Func(Edit_Cursor, WFTRANSPORT, Ptypes => (ARMY => TRUE, others => FALSE));
            when 'U' =>
               -- there ought to be a way to do this by class.  hmm.  XXX
               E_User_Obj_Func(Edit_Cursor, REPAIR, Ptypes => (PATROL|DESTROYER|SUBMARINE|TRANSPORT|BATTLESHIP|CARRIER => TRUE, others => FALSE));
            when 'V' =>                 -- set city function
               E_City_Func(Edit_Cursor, Path);
            when 'Y' =>
               E_User_Obj_Func(Edit_Cursor, ARMYATTACK, Ptypes => (ARMY => TRUE, others => FALSE));
            when '=' =>                 -- request info
               E_Info(Edit_Cursor);
            when Character'Val(12) =>      -- c-l (redraw screen)
               Ui.Redraw;
            when '?' =>
               Ui.Help(Help_Edit);
            when others =>
               Ui.Huh;
         end case;
      end loop;
   end Edit;

-- Get the next command.  We handle cursor movement here

   procedure E_Cursor (Edit_Cursor : in out Location_T; Cmd : out Character) is
   begin
      loop
         Cmd := Ui.Get_Chx;
         if Dir_Chars(Cmd) /= NODIRECTION
         then
            exit;
         else
            Ui.Move_Cursor(Edit_Cursor, Dir_Chars(Cmd));
         end if;
      end loop;
   end E_Cursor;

-- Goto new sector

   procedure E_Goto_Sector (Edit_Cursor : in out Location_T) is
      Sector : Sector_T;
   begin
      -- XXX better to have a `ui.get_sector' which could validate on its own
      Sector := Ui.Get_Int("New Sector? ", Sector_T'First, Sector_T'Last);

      Edit_Cursor := Locations.Sector_Loc(Sector); -- center cursor within sector
      Ui.Sector_Change;                 -- allow change of sector
   end E_Goto_Sector;

-- Set function for the piece at a location
-- XXX could merge with Empire.User_Move.User_Obj_Func?
   procedure E_User_Obj_Func
     (Loc : in Location_T;
      Func : in Function_T;
      Ptypes : in Acceptable_Piece_Array := (SATELLITE => FALSE, others => TRUE);
      Dest_If_Move_To_Dest : Location_T := 0
      ) is
      Obj : Piece_Info_P;
   begin
      Obj := Objects.Find_Obj_At_Loc(Loc, Types => Ptypes, Owners => (USER => TRUE, others => FALSE));

      if Obj /= null
      then
         if Func = MOVE_TO_DEST
         then
            Obj.Dest := Dest_If_Move_To_Dest;
         end if;
         Obj.Func := Func;
      else
         Ui.Huh;
      end if;
   end E_User_Obj_Func;

-- Set function for a type of piece produced by the city at a location

   procedure E_User_City_Func
     (Loc : in Location_T;
      Func : in Function_T;
      Ptype : in Piece_Type_T;
      Dest_If_Move_To_Dest : Location_T := 0) is
      Cityp : City_Info_P;
   begin
      Cityp := Objects.Find_City_At_Loc(Loc, Owners => (USER => TRUE, others => FALSE));

      if Cityp /= null
      then
         Cityp.Func(Ptype) := Func;
         if Func = MOVE_TO_DEST
         then
            Cityp.Dest(Ptype) := Dest_If_Move_To_Dest;
         end if;
      else
         Ui.Huh;
      end if;
   end E_User_City_Func;

   procedure E_City_Repair (Loc : in Location_T; Ptype : Piece_Type_T) is
   begin
      if Piece_Attr(Ptype).Class = SHIP
      then
         E_User_City_Func(Loc, REPAIR, Ptype);
      else
         Ui.Huh;
      end if;
   end E_City_Repair;

-- Set object to move in a direction

   procedure E_Move_Direction (Loc : in Location_T) is
      C : Character;
      D : Direction_T;
      F : Function_T;
      Obj : Piece_Info_P;
   begin
      Obj := Objects.Find_Obj_At_Loc(Loc, Owners => (USER => TRUE, others => FALSE));
      if Obj = null
      then
         Ui.Huh;
         return;
      end if;

      C := Ui.Get_Chx;                  -- XXX XXX should prompt
      D := Dir_Chars(C);
      if D = NODIRECTION
      then
         Ui.Huh;
      else
         F := Dir_Funcs(D);
         E_User_Obj_Func(Loc, F);
      end if;
   end E_Move_Direction;

   procedure E_City_Move_Direction (Loc : in Location_t; Ptype : Piece_Type_T) is
      E : Character;
      D : Direction_T;
      F : Function_T;
   begin
      E := Ui.Get_Chx;                  -- get a direction
      D := Dir_Chars(E);

      if D = NODIRECTION
      then
         Ui.Huh;
      else
         F := Dir_Funcs(D);
         E_User_City_Func(Loc, F, Ptype);
      end if;
   end E_City_Move_Direction;

-- Wake up anything and everything

   procedure E_Wake (Loc : in Location_T) is
      Obj : Piece_Info_P;
   begin
      for I in Piece_Type_T'Range
      loop
         E_User_City_Func(Loc, NOFUNC, I);
      end loop;

      Obj := Map(Loc).Objp;
      while Obj /= null
      loop
         Obj.Func := NOFUNC;
      end loop;
   end E_Wake;

--  Set a city's function.  We get the piece type to set, then the function itself.

   -- XXX XXX XXX this is identical to User_Set_City_Func -- except that it
   -- XXX XXX XXX supports a path.  the two should be merged
   procedure E_City_Func (Loc : in Location_T; Path : out Path_T) is
      Ptype : Piece_Type_T;
      E : Character;
      Cityp : City_Info_P;
   begin
      Cityp := Objects.Find_City_At_Loc(Loc, Owners => (USER => TRUE, others => FALSE));

      if Cityp = null
      then
         Ui.Huh;
         return;
      end if;

      Ptype := Ui.Get_Piece_Name;

      if Ptype = NOPIECE
      then
         Ui.Huh;
         return;
      end if;

      -- XXX XXX XXX should prompt here
      E := Ui.Get_Chx;

      case E is
         when 'F' =>
            if (Ptype = TRANSPORT) or (Ptype = CARRIER)
            then
               E_User_City_Func(Loc, FILL, Ptype);
            else
               Ui.Huh;
            end if;
         when 'G' =>
            E_User_City_Func(Loc, EXPLORE, Ptype);
         when 'I' =>
            E_City_Move_Direction(Loc, Ptype);
         when 'K' =>
            E_User_City_Func(Loc, NOFUNC, Ptype);
         when 'M' =>
            E_Start_Path(Path, Loc, Ptype);
         when 'R' =>
            E_User_City_Func(Loc, RANDOM, Ptype);
         when 'U' =>
            E_City_Repair(Loc, Ptype);
         when 'Y' =>
            if Ptype = ARMY
            then
               E_User_City_Func(Loc, ARMYATTACK, Ptype);
            else
               Ui.Huh;
            end if;
         when others =>
            Ui.Huh;
      end case;
   end E_City_Func;

-- Beginning of move to location

   procedure E_Start_Path (Path: in out Path_T; Loc : in Location_T; Ptype : in Piece_Type_T) is
   begin
      if (Objects.Find_Obj_At_Loc(Loc, Owners => (USER => TRUE, others => FALSE)) = null) and
          (Objects.Find_City_At_Loc(Loc, Owners => (USER => TRUE, others => FALSE)) = null)
      then
         Ui.Huh;
      else
         Path.Start := Loc;
         Path.Started := TRUE;
         Path.Ptype := Ptype;
      end if;
   end E_Start_Path;

-- Finish setting up move to location

   procedure E_End_Path (Path : in out Path_T; Loc : in Location_T) is
   begin
      if not Path.Started
      then
         Ui.Huh;
         return;
      end if;

      if Path.Ptype = NOPIECE           -- XXX this means this is not a city path
      then
         E_User_Obj_Func(Path.Start, MOVE_TO_DEST, Dest_If_Move_To_Dest => Loc);
      else
         E_User_City_Func(Path.Start, MOVE_TO_DEST, Path.Ptype, Dest_If_Move_To_Dest => Loc);
      end if;

      Path.Started := FALSE;
   end E_End_Path;

-- Print out information about a piece

   procedure E_Info (Loc : in Location_T) is
      Cityp : City_Info_P;
      Objp : Piece_Info_P;
      Whose : Acceptable_Owner_Array := (USER => TRUE, others => FALSE);
   begin
      if Debug
      then
         Whose(COMP) := TRUE;
      end if;

      Cityp := Objects.Find_City_At_Loc(Loc, Whose);
      if Cityp /= null
      then
         Objects.Describe_City(Cityp.all);
         return;
      end if;

      Objp := Objects.Find_Obj_At_Loc(Loc, Types => (others => TRUE), Owners => Whose);
      if Objp /= null
      then
         Objects.Describe_Obj(Objp);
         return;
      end if;

      Ui.Huh;
   end E_Info;

-- Change city production

   procedure E_Prod (Loc : in Location_T) is
      Cityp : City_Info_P;
   begin
      Cityp := Objects.Find_City_At_Loc(Loc, Owners => (USER => TRUE, others => FALSE));

      if Cityp = null
      then
         Ui.Huh;
      else
         Objects.Ask_Prod(Cityp.all);
      end if;
   end E_Prod;

end Empire.Editing;
