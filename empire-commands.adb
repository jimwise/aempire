-- empire.c -- this file contains initialization code, the main command
-- parser, and the simple commands.

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
              Huh;
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
     Unowned : array (1 .. NUM_CITY) of Integer;
     City_Index : Integer;
     Count : Integer := 0;              -- nothing in list yet
  begin
     for I in 1 .. NUM_CITY
     loop
        if City(I).Owner = UNOWNED
        then
           Count := Count + 1;
           Unowned(Count) := I;
        end if;
     end loop;

     if Count = 0
     then
        Ui.Error("There are no unowned cities.");
        return;
     end if;

     I := Rand_Long (Count);            -- pick a city, any city
     City_Index := Unowned(I);          -- get city index
     City(City_Index).Owner := COMP;
     City(City_Index).Prod := NOPIECE;
     City(City_Index).Work := 0;
     Objects.Scan (Comp_Map, City(City_Index).Loc);

  end Give;


  -- The quit command.  Make sure the user really wants to quit
  procedure Quit is
  begin
     if Get_Yn("QUIT -- Are you sure? ")
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

-- Print the map to a file.  We ask for a filename, attempt to open the
-- file, and if successful, print out the user's information to the file.
-- We print the map sideways to make it easier for the user to print
-- out the map.

static void
c_map (void)
{
        FILE *f;
        int i, j;
        char line[MAP_HEIGHT+2];

        prompt ("Filename? ");
        get_str (jnkbuf, STRSIZE);

        f = fopen (jnkbuf, "w");
        if (f == NULL)
        {
                error ("I can't open that file.");
                return;
        }

        for (i = 0; i < MAP_WIDTH; i++)
        {
                /* for each column */
                for (j = MAP_HEIGHT-1; j >= 0; j--)
                {
                        /* for each row */
                        line[MAP_HEIGHT-1-j] = user_map[row_col_loc(j,i)].contents;
                }
                j = MAP_HEIGHT-1;
                while (j >= 0 && line[j] == ' ') /* scan off trailing blanks */
                        j -= 1;

                line[++j] = '\n';
                line[++j] = 0; /* trailing null */
                fputs (line, f);
        }
        fclose (f);
}

/*
 * We give the computer lots of free moves and
 * Print a "zoomed" version of the computer's map.
 */

static void
c_movie (void)
{
        while (1)
        {
                comp_move();
                print_zoom(comp_map);
                save_game();
        }
}

/*This provides a single place for collecting all startup routines */

static void
emp_start (void)
{
        term_init();    /* init tty, and info and status windows */
        map_init();     /* init map window */
        rand_init();    /* init random number generator */
}

/* This provides a single place for collecting all cleanup routines */

static void
emp_end (void)
{
        term_end();
        exit(0);
}
