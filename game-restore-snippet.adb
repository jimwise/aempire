      begin
         Game.Restore_Game
      exception
         when Game.No_Saved_Game =>
            Game.Init_Game;
         when Game.Corrupt_Saved_Game =>
            Ui.Error("Saved game is corrupted!");
            Ui.Prompt("Overwrite with new game? ");
            E := Ui.Get_Yn;
            if ...
