package Empire.Game is
   No_Saved_Game : Exception;
   Corrupt_Saved_Game : Exception;
   procedure Init_Game;
   procedure Replay_Movie;
   procedure Restore_Game;
   procedure Save_Game;
   procedure Save_Movie_Screen;
end Empire.Game;
