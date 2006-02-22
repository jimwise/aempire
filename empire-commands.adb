-- simple commands.

with Ada.Text_IO;

with Empire.Comp_Move;
with Empire.Game;
with Empire.Locations;
with Empire.Math;
with Empire.Objects;
with Empire.Ui;

--    procedure Map;
--    procedure Movie;
--    procedure Quit;
--    procedure Restore;
--    procedure Sector;

package body Empire.Commands is

  -- Debugging commands should be implemented here.
  -- The order cannot be any legal command.

  procedure Debug (Order : Character) is
     E : Character;
  begin
     case Order is
        when '#' =>
           Examine;
        when '%' =>
           Movie;
        when '@' =>                     -- change trace state
           E := Ui.Get_Chx;
           if E = '+'
           then
                Trace_Pmap := TRUE;
           elsif E = '-'
           then
              Trace_Pmap := FALSE;
           else
                Ui.Huh;
           end if;
        when '$' =>                     -- change print_debug state
           E := Ui.Get_Chx;
           if E = '+'
           then
              Print_Debug := TRUE;
           elsif E = '-'
           then
              Print_Debug := FALSE;
           else
              Ui.Huh;
           end if;
        when '&' =>                     -- change print_vmap state
           E := Ui.Get_Chx;
           case E is
              when 'A'|'I'|'L'|'S'|'U' =>
                 Print_Vmap := E;
              when others =>
                 Ui.Huh;
           end case;
        when others =>
           Ui.Huh;
     end case;
  end Debug;

  -- Allow user to examine the computer's map

  procedure Examine is
     Sec : Sector_T;
  begin
     Sec := Ui.Get_Int("Sector number? ", Sector_T'First, Sector_T'Last);
     Ui.Print_Sector_C (Sec);
  end Examine;

  -- Give an unowned city (if any) to the computer.  We make
  -- a list of unowned cities, choose one at random, and mark
  -- it as the computers.

  procedure Give is
     Unowned_Cities : array (City'Range) of Integer;
     City_Index : Integer;
     Count : Integer := 0;              -- nothing in list yet
  begin
     for I in City'Range
     loop
        if City(I).Owner = UNOWNED
        then
           Count := Count + 1;
           Unowned_Cities(Count) := I;
        end if;
     end loop;

     if Count = 0
     then
        Ui.Error("There are no unowned cities.");
        return;
     end if;

     City_Index := Unowned_Cities(Math.Rand_Long(Count));   -- pick a city, get its index
     City(City_Index).Owner := COMP;
     City(City_Index).Prod := NOPIECE;
     City(City_Index).Work := 0;
     Objects.Scan (Comp_Map, City(City_Index).Loc);

  end Give;


  -- The quit command.  Make sure the user really wants to quit
  procedure Quit is
  begin
     if Ui.Get_Yn("QUIT -- Are you sure? ")
     then
        Emp_End;
     end if;
  end Quit;


  -- Read the sector number from the user and print that sector

  procedure Sector is
     Sec : Sector_T;
  begin
     Sec := Ui.Get_Int("Sector number? ", Sector_T'First, Sector_T'Last);
     Ui.Print_Sector_U(Sec);
  end Sector;

-- Print the map to a file.
-- We print the map sideways to make it easier for the user to print
-- out the map.
-- XXX XXX For now, unlike C version, we always use the same file name
-- (which is symmetric with game and movie saves).  All three should change.

  procedure Map is
     F : Ada.Text_IO.File_Type;
  begin
     Ada.Text_IO.Create(F, Ada.Text_IO.Out_File, MAP_NAME);

     for I in Column_T'Range
     loop
        -- for each column (see above)
        for J in Row_T'Range
        loop
           -- for each row (see above)
           -- XXX we should do this line-at-a-time, as the C does.
           Content_IO.Put(F, User_Map(Locations.Row_Col_Loc(J, I)).Contents);
        end loop;
        Ada.Text_IO.Put_Line(F, "");
     end loop;
  exception
     when others =>                     -- XXX XXX much too general
        Ui.Error("Unable to write map to file " & MAP_NAME & ".");
  end Map;

-- We give the computer lots of free moves and
-- Print a "zoomed" version of the computer's map.

  procedure Movie is
  begin
     loop
        Comp_Move.Comp_Move;
        Ui.Print_Zoom(Comp_Map);
        Game.Save_Game;
     end loop;
  end Movie;

-- restore game with some error handling

  procedure Restore is
  begin
     Game.Restore_Game;
  exception
     when Game.No_Saved_Game =>
        Game.Init_Game;
     when Game.Corrupt_Saved_Game =>
        Ui.Error("Saved game is corrupted!");
        if Ui.Get_Yn("Overwrite with new game? ")
        then
           Game.Init_Game;
        else
           Emp_End;
        end if;
  end Restore;

end Empire.Commands;
