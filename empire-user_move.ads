package Empire.User_Move is
   procedure User_Move;
private
   procedure Ask_User (Obj : in Piece_Info_P);
   procedure Awake (Obj : in Piece_Info_P; Awoken : out Boolean);
   procedure Piece_Move (Obj : in Piece_Info_P);

   procedure Move_Armyattack (Obj : in Piece_Info_P);
   procedure Move_Dir (Obj : in Piece_Info_P);
   procedure Move_Explore (Obj : in Piece_Info_P);
   procedure Move_Fill (Obj : in Piece_Info_P);
   procedure Move_Land (Obj : in Piece_Info_P);
   procedure Move_Path (Obj : in Piece_Info_P);
   procedure Move_Random (Obj : in Piece_Info_P);
   procedure Move_Repair (Obj : in Piece_Info_P);
   procedure Move_Transport (Obj : in Piece_Info_P);

   procedure Move_Army_To_City (Obj : in Piece_Info_P; Loc : in Location_T);
   procedure Move_To_Dest (Obj : in Piece_Info_P; Dest : in Location_T);

   procedure Reset_Func (Obj : in Piece_Info_P);

   procedure User_Obj_Func
     (Obj    : in Piece_Info_P;
      Func   : in Function_T;
      Ptypes : in Acceptable_Piece_Array :=
        (SATELLITE => False, others => True);
      Dest_If_Move_To_Dest : Location_T := 0);

   procedure User_Build (Loc : in Location_T);
   procedure User_Cancel_Auto;
   procedure User_Dir (Obj : in Piece_Info_P; Dir : in Direction_T);
   procedure User_Dir_Ground (Obj : in Piece_Info_P; Loc : in Location_T);
   procedure User_Dir_Aircraft (Obj : in Piece_Info_P; Loc : in Location_T);
   procedure User_Dir_Ship (Obj : in Piece_Info_P; Loc : in Location_T);
   procedure User_Set_City_Func (loc : in Location_T);
   procedure User_Set_Dir (Obj : in Piece_Info_P);
   procedure User_Skip (Obj : in Piece_Info_P);

   --  when choosing a refuel location, we bias against carriers, as they may
   --  move or fill up while we are en route. This amount is how much shorter
   --  than a city range a carrier range has to be to be considered better. XXX
   --  untested, may need tweaking
   Carrier_Bias : constant Integer := 4;
end Empire.User_Move;
