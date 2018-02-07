package Empire.Editing is

   procedure Edit (Cur_Cursor : in Location_T);

   --  XXX XXX if called with Func = MOVE_TO_DEST and no Dest_If_Move_To_Dest
   --  argument, sets piece/pieces made by city to move to destination 0.
   procedure E_User_Obj_Func
     (Loc    : in Location_T;
      Func   : in Function_T;
      Ptypes : in Acceptable_Piece_Array :=
        (SATELLITE => False, others => True);
      Dest_If_Move_To_Dest : Location_T := 0);
   procedure E_User_City_Func
     (Loc                  : in Location_T;
      Func                 : in Function_T;
      Ptype                : in Piece_Type_T;
      Dest_If_Move_To_Dest :    Location_T := 0);
   procedure E_City_Move_Direction (Loc : in Location_T; Ptype : Piece_Type_T);

   procedure E_City_Repair (Loc : in Location_T; Ptype : Piece_Type_T);

private
   --  array mapping direction chars to dir_offset indices (1=>N .. 8=>NW).
   --  0 => not a valid direction char (XXX)
   type Direction_Offset_Array is array (Character) of Direction_Choice_T;
   Dir_Chars : constant Direction_Offset_Array :=
     ('W'    => NORTH,
      'E'    => NORTHEAST,
      'D'    => EAST,
      'C'    => SOUTHEAST,
      'X'    => SOUTH,
      'Z'    => SOUTHWEST,
      'A'    => WEST,
      'Q'    => NORTHWEST,
      others => NODIRECTION);
   --  map move directions onto move functions
   type Direction_Function_Array is array (Direction_T) of Function_T;
   --  hopefully optimized away.
   Dir_Funcs : constant Direction_Function_Array :=
     (NORTH     => MOVE_N,
      NORTHEAST => MOVE_NE,
      EAST      => MOVE_E,
      SOUTHEAST => MOVE_SE,
      SOUTH     => MOVE_S,
      SOUTHWEST => MOVE_SW,
      WEST      => MOVE_W,
      NORTHWEST => MOVE_NW,
      others    => NOFUNC);

   --  information on a path being constructed. No path end, as end is edit
   --  location at time path is ended. XXX XXX this should grow waypoint
   --  support
   type Path_T is record
      Started : Boolean := False;
      Start   : Location_T;
      --  XXX type path is for if this is a city path, NOPIECE for a piece path
      Ptype : Piece_Choice_T;
   end record;

   procedure E_Cursor (Edit_Cursor : in out Location_T; Cmd : out Character);

   procedure E_City_Func (Loc : in Location_T; Path : out Path_T);
   procedure E_End_Path (Path : in out Path_T; Loc : in Location_T);
   procedure E_Info (Loc : in Location_T);
   procedure E_Start_Path
     (Path  : in out Path_T;
      Loc   : in     Location_T;
      Ptype : in     Piece_Choice_T);
   procedure E_Goto_Sector (Edit_Cursor : in out Location_T);
   procedure E_Move_Direction (Loc : in Location_T);
   procedure E_Prod (Loc : in Location_T);
   procedure E_Wake (Loc : in Location_T);

end Empire.Editing;
