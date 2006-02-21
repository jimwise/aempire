package Empire.Editing is
   procedure Edit (Edit_cursor : in Location_T);

   procedure E_City_Attack (Cityp : in out City_Info_T; Ptype : Piece_Type_T);
   procedure E_City_Explore (Cityp : in out City_Info_T; Ptype : Piece_Type_T);
   procedure E_City_Fill (Cityp : in out City_Info_T; Ptype : Piece_Type_T);
   procedure E_City_Random (Cityp : in out City_Info_T; Ptype : Piece_Type_T);
   procedure E_City_Repair (Cityp : in out City_Info_T; Ptype : Piece_Type_T);
   procedure E_City_Stasis (Cityp : in out City_Info_T; Ptype : Piece_Type_T);
   procedure E_City_Wake (Cityp : in out City_Info_T; Ptype : Piece_Type_T);
end Empire.Editing;
