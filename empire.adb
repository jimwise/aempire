--  this file contains initialization code, the main command parser, and the
--  simple commands.

with Ada.Exceptions;

with Empire.Commands;
with Empire.Ai;
with Empire.Editing;
with Empire.Game;
with Empire.Locations;
with Empire.Math;
with Empire.Ui;
with Empire.User_Move;

package body Empire is

   procedure Run_Empire is
      Order : Character;
      --  distinct from date as we may have loaded a saved game
      Turn : Natural := 0;
   begin
      --  DEBUGGING CONFIG GOES HERE XXX this should move to empire.ads
      Debug       := True;
      Print_Debug := True;
      Print_Vmap  := '0';
      Trace_Pmap  := False;

      Emp_Start;

      Ui.Clear;
      Ui.Info (VERSION_STRING);

      Commands.Restore;

      loop
         if Automove then
            Ui.Prompt ("User_Move...");
            User_Move.User_Move;
            Ui.Prompt ("Comp_Move...");
            Ai.Comp_Move;
            Ui.Debug_Info ("checking for endgame");
            Game.Check_Endgame;
            Turn := Turn + 1;
            Date := Date + 1;
            if (Turn mod Save_Interval) = 0 then
               Game.Save_Game;
            end if;
            if Save_Movie then
               Game.Save_Movie_Screen;
            end if;
            Ui.Display_Score;
            Ui.Display_Turn;
            Ui.Prompt ("");
         else
            Ui.Prompt ("Your orders? ");
            Order := Ui.Get_Chx;
            Do_Command (Order);
         end if;
      end loop;

   exception
      when User_Quit =>
         null;
      when E : others =>
         Ui.End_Ui;
         Ada.Text_IO.Put_Line ("Unexpected Exception:");
         Ada.Text_IO.Put (Ada.Exceptions.Exception_Information (E));
         return;
   end Run_Empire;

--  This provides a single place for collecting all startup routines

   procedure Emp_Start is
   begin
      Ui.Init_Ui;                   -- init tty, and info and status windows
      Ui.Init_Map;                  -- init map window
      Math.Rand_Init;               -- init random number generator
   end Emp_Start;

--  This provides a single place for collecting all cleanup routines */

   procedure Emp_End is
   begin
      Ui.End_Ui;
      raise User_Quit;
   end Emp_End;

--  execute a command

   procedure Do_Command (Orders : in Character) is
      E      : Character;
      Ncycle : Integer;
      Sec    : Sector_T;
   begin
      case Orders is
         when 'A' =>                    -- turn on auto-move mode
            Automove := True;
            Ui.Info ("Entering Auto-Mode");
            Game.Save_Game;             -- restart auto-save clock

         when 'C' =>                    -- give a city to the computer
            Commands.Give;

         when 'D' =>                    -- display round number
            Ui.Error ("Round " & Integer'Image (Date)); -- XXX spaces right?

         when 'E' =>                    -- examine enemy map
            if Resigned then
               Commands.Examine;
            else
               Ui.Huh;            -- would be better to tell user why not
            end if;

         when 'F' =>                    -- print map to file
            Commands.Map;

         when 'G' =>                 -- give one free enemy move
            Ai.Comp_Move;
            Ui.Display_Score;
            Ui.Display_Turn;

         when '?' =>                  -- help
            Ui.Help (Help_Cmd);

         when 'J' =>                    -- edit mode
            Sec := Ui.Cur_Sector;
            --  in C, this was only done if we were switching from view of
            --  non-user map, but it's cheap to always do it if sector is
            --  0. XXX may be better to add an out boolean to Cur_Sector
            if Sec = Sector_T'First then
               Ui.Print_Sector (USER, Sec);
            end if;
            Editing.Edit (Locations.Sector_Loc (Sec));

         when 'M' =>                    -- move
            User_Move.User_Move;
            Ai.Comp_Move;
            Game.Save_Game;

         when 'N' =>                    -- give enemy free moves
            Ncycle := Ui.Get_Int ("Number of free enemy moves: ", 1, 1000);
            for I in 1 .. Ncycle loop
               Ai.Comp_Move;
            end loop;
            Game.Save_Game;

         when 'P' =>                    -- print a sector
            Commands.Sector;

         when 'Q' =>              -- quit
            --  XXX XXX C code also does so for \026
            Commands.Quit;

         when 'R' =>                    -- restore saved game
            Ui.Clear;
            Commands.Restore;

         when 'S' =>                    -- save game (XXX error check)
            Game.Save_Game;

         when 'T' =>                    -- trace (toggle save_movie flag)
            Save_Movie := not Save_Movie;
            if Save_Movie then                        -- XXX name?
               Ui.Info ("Saving movie screens to '" & MOVIE_NAME & "'.");
            else
               Ui.Info ("No longer saving movie screens.");
            end if;

         when 'W' =>                    -- watch movie
            if Resigned or Debug then
               Game.Replay_Movie;
            else
               Ui.Error ("You cannot watch a movie until your foe resigns.");
            end if;

         when 'Z' =>                    -- print compressed map
            Ui.Print_Zoom (View (USER));
            Ui.Redraw;

         when Character'Val (12) =>      -- c-l (redraw screen)
            Ui.Redraw;

         when '+' =>                    -- change debug state
            E := Ui.Get_Chx;
            if E = '+' then
               Ui.Error ("Debug enabled");
               Debug := True;
            elsif E = '-' then
               Ui.Error ("Debug disabled");
               Debug := False;
            else
               Ui.Huh;
            end if;

         when others =>
            if Debug                    -- may be a debug command, try it
            then
               Commands.Debug (Orders);
            else
               Ui.Huh;
            end if;
      end case;
   end Do_Command;

end Empire;
