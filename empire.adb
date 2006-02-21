-- this file contains initialization code, the main command
-- parser, and the simple commands.

with Empire.Commands;
with Empire.Comp_Move;
with Empire.Editing;
with Empire.Game;
with Empire.Locations;
with Empire.Math;
with Empire.Ui;
with Empire.User_Move;

package body Empire is


   procedure Run_Empire is
      Order : Character;
      Turn : Natural := 0; -- distinct from date to track save_interval even if we loaded a saved game
   begin
      Emp_Start;

      Ui.Clear;
      Ui.Info(VERSION_STRING);

      Commands.Restore;

      loop
         if Automove
         then
            User_Move.User_Move;
            Comp_Move.Comp_Move;
            Turn := Turn + 1;
            if (Turn mod Save_Interval) = 0
            then
               Game.Save_Game;
            end if;
         else
            Ui.Prompt("Your orders? ");
            Order := Ui.Get_Chx;
            Do_Command(Order);
         end if;
      end loop;

   exception
      when User_Quit =>
         Ui.End_Ui;
         return;
   end Run_Empire;

-- This provides a single place for collecting all startup routines

   procedure Emp_Start is
   begin
      Ui.Init_Ui;                          -- init tty, and info and status windows
      Ui.Init_Map;                      -- init map window
      Math.Rand_Init;                   -- init random number generator
   end;

-- This provides a single place for collecting all cleanup routines */

   procedure Emp_End is
   begin
      raise User_Quit;
   end Emp_End;

-- execute a command

   procedure Do_Command (Orders : in Character)
   is
      E : Character;
      Ncycle : Integer;
      Sec : Sector_T;
   begin
      case Orders is
         when 'A' =>                    -- turn on auto-move mode
            Automove := TRUE;
            Ui.Info("Entering Auto-Mode");
            Game.Save_Game;             -- restart auto-save clock

         when 'C' =>                    -- give a city to the computer
            Commands.Give;

         when 'D' =>                    -- display round number
            Ui.Error("Round " & Integer'Image(Date)); -- XXX spaces right?

         when 'E' =>                    -- examine enemy map
            if Resigned
            then
               Commands.Examine;
            else
               Ui.Huh;            -- would be better to tell user why not
            end if;

         when 'F' =>                    -- print map to file
            Commands.Map;

         when 'G' =>                 -- give one free enemy move
            Comp_Move.Comp_Move;

         when '?' =>                  -- help
            Ui.Help(Help_Cmd);

         when 'J' =>                    -- edit mode
            begin
               Sec := Ui.Cur_Sector;
               Editing.Edit(Locations.Sector_Loc(Sec));
            exception
               when Ui.No_Current_Sector =>
                  Ui.Print_Sector_U(Sector_T'First);
                  Editing.Edit(Locations.Sector_Loc(Sec));
            end;

         when 'M' =>                    -- move
            User_Move.User_Move;
            Comp_Move.Comp_Move;
            Game.Save_Game;

         when 'N' =>                    -- give enemy free moves
            Ncycle := Ui.Get_Int("Number of free enemy moves: ", 1, 1000);
            for I in 1 .. Ncycle
            loop
               Comp_Move.Comp_Move;
            end loop;
            Game.Save_Game;

         when 'P' =>                    -- print a sector
            Commands.Sector;

         when 'Q' =>              -- quit.  XXX XXX C code also does so for \026
            Commands.Quit;

         when 'R' =>                    -- restore saved game
            Ui.Clear;
            Commands.Restore;

         when 'S' =>                    -- save game (XXX error check)
            Game.Save_Game;

         when 'T' =>                    -- trace (toggle save_movie flag)
            Save_Movie := not Save_Movie;
            if Save_Movie
            then                        -- XXX name?
               Ui.Info("Saving movie screens to 'empmovie.dat'.");
            else
               Ui.Info("No longer saving movie screens.");
            end if;

         when 'W' =>                    -- watch movie
            if Resigned or Debug
            then
               Game.Replay_Movie;
            else
               Ui.Error("You cannot watch the movie until the computer resigns.");
            end if;

         when 'Z' =>                    -- print compressed map
            Ui.Print_Zoom(User_Map);
            Ui.Redraw;

         when Character'Val(12) =>      -- c-l (redraw screen)
            Ui.Redraw;

         when '+' =>                    -- change debug state
            E := Ui.Get_Chx;
            if E = '+'
            then
                Debug := TRUE;
            elsif E = '-'
            then
               Debug := FALSE;
            else
               Ui.Huh;
            end if;

         when others =>
            if Debug                    -- may be a debug command, try it
            then
               Commands.Debug(Orders);
            else
               Ui.Huh;
            end if;
      end case;
   end Do_Command;

end Empire;