package Empire.Game is

   No_Saved_Game : exception;
   Corrupt_Saved_Game : exception;
   procedure Init_Game;
   procedure Replay_Movie;
   procedure Restore_Game;
   procedure Save_Game;
   procedure Save_Movie_Screen;

private

   procedure Find_Cont;
   procedure Find_Next (Mapi : in out Location_T; Found : out Boolean);
   function Good_Cont (Mapi : in Location_T) return Boolean;
   procedure Make_Map;
   procedure Make_Pair;
   procedure Mark_Cont (Mapi : in Location_T);
   procedure Place_Cities;
   procedure Read_Embark (List : in Piece_Info_P; Ptype : in Piece_Type_T);
   procedure Regen_Land (Placed : in Natural; Num_Land : out Natural);
   procedure Remove_Land (Loc : in Location_T; Num_Land : in out Natural);
   function Select_Cities return Boolean;
   procedure Stat_Display(Vmap : in View_Map_T; Round : in Natural);

-- static int      xread (FILE *, void *, int);
-- static int      xwrite (FILE *, void *, int);

end Empire.Game;
