package Empire.Attack is
   procedure Attack (Att_Obj : in out Piece_Info_T; Loc : in Location_T);
private
   procedure Attack_City (Att_Obj : in out Piece_Info_T; Loc : in Location_T);
   procedure Attack_Obj (Att_Obj : in out Piece_Info_T; Loc : in Location_T);
   procedure Describe
     (
      Win_Obj : in Piece_Info_T;
      Lose_Obj : in Piece_Info_T;
      Loc : in Location_T
      );
   procedure Survive (Obj : in out Piece_Info_T; Loc : in Location_T);
end Empire.Attack;
