with Ada.Text_IO;
package Empire is
   MAP_WIDTH : constant Integer := 100;
   MAP_HEIGHT : constant Integer := 60;
   MAP_SIZE : constant Integer := MAP_WIDTH * MAP_HEIGHT;
   NUM_CITY : constant Integer := 70;

   -- cost to switch a city's production is the _new_ item's cost over RETOOLING_DENOMINATOR
   RETOOLING_DENOMINATOR : constant := 5;

   -- these are still from 0 so modulo, etc are easy to use.
   subtype Location_T is Integer range 0 .. MAP_SIZE-1; -- map location as index into map
   subtype Row_T is Integer range 0 .. MAP_HEIGHT-1;
   subtype Column_T is Integer range 0 .. MAP_WIDTH-1;

   type Location_Value_Array is array (Location_T) of Integer;

   LIST_SIZE : constant Integer := 5000; -- max number of pieces on board

   NUMTOPS : constant Integer := 4;      -- number of lines at top of screen for messages
   NUMINFO : constant Integer := NUMTOPS - 1;
   NUMSIDES : constant Integer := 6;     -- number of lines at side of screen

   type String_P is access constant String;
   type Help_Array is array (Natural range <>) of String_P;

   SAVE_NAME : constant String := "empsave.dat";
   MOVIE_NAME : constant String := "empmovie.dat";
   MAP_NAME : constant String := "empmap.txt";

   -- useful constants for accessing sectors

   SECTOR_ROWS : constant Integer := 5;  -- number of vertical sectors
   SECTOR_COLS : constant Integer := 2;  -- number of horizontal sectors
   NUM_SECTORS : constant Integer := SECTOR_ROWS * SECTOR_COLS; -- total sectors
   ROWS_PER_SECTOR : constant Integer := (MAP_HEIGHT + SECTOR_ROWS - 1) / SECTOR_ROWS;
   COLS_PER_SECTOR : constant Integer := (MAP_WIDTH + SECTOR_COLS - 1) / SECTOR_COLS;
   subtype Sector_T is Integer range 0 .. NUM_SECTORS - 1;

   INFINITY : constant Integer := Integer'Last;

   VERSION_STRING : constant String := "ADA EMPIRE, Version 1.4_ALPHA0, February 2006";

   type Owner_t is (UNOWNED, USER, COMP);

   type Piece_Type_T is
      (ARMY,
       FIGHTER,
       PATROL,
       DESTROYER,
       SUBMARINE,
       TRANSPORT,
       CARRIER,
       BATTLESHIP,
       SATELLITE,
       NOPIECE);

  type Piece_Value_Array is array (Piece_Type_T) of Integer;
  type Piece_Value_P is access all Piece_Value_Array;

  type Piece_Class_T is
     (GROUND,
      AIRCRAFT,
      SHIP,
      SPACECRAFT);

   -- in addition to terrain and user types, planning algorithms use following special symbols in view maps
   -- '$' represents loading tt must be first
   --  'x' represents tt producing city
   --  '0' .. '9' represent explorable territory

   type Content_Display_T is
      ('.', '+', '*', 'O', 'X', ' ',
       'A', 'a', 'F', 'f', 'P', 'p', 'D', 'd', 'S', 's', 'T', 't', 'C', 'c', 'B', 'b', 'Z', 'z',
       '$', 'x', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
   type Terrain_Display_T is ('.', '+', '*');

   type Acceptable_Content_Array is array (Content_Display_T) of Boolean;
   type Acceptable_Terrain_Array is array (Terrain_Display_T range '.' .. '*') of Boolean;
   type Content_Value_Array is array (Content_Display_T) of Integer;

   package Content_IO is new Ada.Text_IO.Enumeration_IO(Content_Display_T);
   package Terrain_IO is new Ada.Text_IO.Enumeration_IO(Terrain_Display_T);

   type Direction_T is
      (NORTH,
       NORTHEAST,
       EAST,
       SOUTHEAST,
       SOUTH,
       SOUTHWEST,
       WEST,
       NORTHWEST);

   -- in original, function_t was overloaded, with negative values meaning as indicated
   -- below, and positive values indicatign a location_t destination.
   -- we now use MOVE_TO_DEST, and the piece_info_t's (new) `Dest' field.
   type Function_T is
      (NOFUNC,                          -- no programmed function (-1)
       RANDOM,                          -- move randomly (-2)
       SENTRY,                          -- sleep (-3)
       FILL,                            -- load transport (-4)
       LAND,                            -- land fighter at city (-5)
       EXPLORE,                         -- explore (-6)
       ARMYLOAD,                        -- move toward and board a transport (-7)
       ARMYATTACK,                      -- army looks for city to attack (-8)
       TTLOAD,                          -- transport moves toward loading armies (-9)
       REPAIR,                          -- ship moves toward port (-10)
       WFTRANSPORT,                     -- army boards a transport (-11)
       MOVE_N,                          -- move north (-12)
       MOVE_NE,                         -- move northeast (-13)
       MOVE_E,                          -- move east (-14)
       MOVE_SE,                         -- move southeast (-15)
       MOVE_S,                          -- move south (-16)
       MOVE_SW,                         -- move southwest (-17)
       MOVE_W,                          -- move west (-18)
       MOVE_NW,                         -- move northwest (-19)
       MOVE_TO_DEST);                   -- move to Obj.Destination;

   -- if we determine how to represent T_PATH, this could be replaced with a set of constants of
   -- type Acceptable_Content_Array
   type Terrain_T is
      (T_UNKNOWN,
       T_PATH,
       T_LAND,
       T_WATER,
       T_AIR);

   for Terrain_T use
      (T_UNKNOWN => 0,
       T_PATH => 1,
       T_LAND => 2,
       T_WATER => 4,
       T_AIR => 6);                     -- T_LAND or T_WATER

   type Piece_Info_T;

   type Piece_Info_P is access all Piece_Info_T;

   type Link_T is
      record
         Next : Piece_Info_P;
         Prev : Piece_Info_P;
      end record;

   type Piece_Info_T is
      record
         Piece_Link : Link_T;           -- linked list of pieces of this type
         Loc_Link : Link_T;             -- linked list of pieces at a location
         Cargo_Link : Link_T;           -- linked list of cargo pieces
         Owner : Owner_t;               -- owner of piece
         Piece_Type : Piece_Type_T;     -- type of piece
         Loc : Location_T;              -- location of piece
         Func : Function_T;             -- programmed type of movement
         Dest : Location_T;             -- if Func = MOVE_TO_DEST
         Hits : Integer;                -- hits left
         Moved : Integer;               -- moves made
         Ship : Piece_Info_P;           -- pointer to containing ship
         Cargo : Piece_Info_P;          -- pointer to cargo list
         Count : Integer;               -- count of items on board
         Piece_Range : Integer;               -- current range if applicable
      end record;


   -- We maintain attributes for each piece.  Attributes are currently constant
   -- but the way has been paved to allow user's to change attributes at the
   -- beginning of a game.

   type Piece_Attr_T is
      record
         Sname : Content_Display_T;     -- eg 'C'
         Class : Piece_Class_T;         -- general type of piece
         Name : String_P;               -- eg "aircraft carrier"
         Nickname : String_P;           -- eg "carrier"
         Article : String_P;            -- eg "an aircraft carrier"
         Plural : String_P;             -- eg "aircraft carriers" (XXX not used)
         Terrain : Acceptable_Terrain_Array;    -- terrain piece can pass over
         Build_Time : Integer;          -- time needed to build unit
         Strength : Integer;            -- attack strength
         Max_Hits : integer;            -- number of hits when completely repaired
         Speed : Integer;               -- number of squares moved per turn
         Capacity : Integer;            -- max objects that can be held
         Piece_Range : Integer;         -- range of piece
      end record;

   type Function_Array is array (Piece_Type_T) of Function_T;
   type City_Info_T is
      record
         Loc : Location_T;                -- location of city
         Owner : Owner_T;
         func : Function_Array;             -- function for each object (XXX size?)
         Work : Integer;                -- units of work performed
         Prod : Piece_Type_T;           -- item being produced
      end record;

   type City_Info_P is access all City_Info_T;

   type Path_Map_T is
      record
         Cost : Integer;                -- total cost to get here
         Inc_Cost : Integer;            -- incremental cost to get here
         Terrain : Terrain_T;
      end record;
   type Path_Map is array (Location_T) of Path_Map_T;

   -- A record for counts we obtain when scanning a continent

   type Scan_Counts_T is
      record
         User_Cities : Integer;         -- number of user cities on continent
         User_Objects : Piece_Value_Array;
         Comp_Cities : Integer;
         Comp_Objects : Piece_Value_Array;
         Size : Integer;                -- size of continent in cells
         Unowned_Cities : Integer;      -- number of unowned cities
         Unexplored : Integer;          -- unexplored territory
      end record;

   -- Information we need for finding a path for moving a piece
   type Move_Info_T is
      record
         Owner : Owner_t;               -- applicable side
         Objective_Weights : Content_Value_Array; -- maps objectives to weights (0 = not an objective)
      end record;

   -- special cost for a city building a tt
   W_TT_BUILD : constant Integer := -1;

   -- List of cells in the perimeter of our searching for a path

   type Perimeter_T is
      record
         Len : Integer;                 -- number of items in list
         List : Location_Value_Array;   -- list of locations
      end record;

   -- There are 3 maps.  'map' describes the world as it actually
   -- exists; it tells whether each map cell is land, water or a city;
   -- it tells whether or not a square is on the board.
   --
   -- 'user_map' describes the user's view of the world.  'comp_map' describes
   -- the computer's view of the world.

   type Real_Map_T is
      record
         Contents : Terrain_Display_T;  -- '+', '.', or '*'
         On_Board : Boolean;            -- True IFF on board
         CityP : City_Info_P;           -- pointer to city at this location
         ObjP : Piece_Info_P;           -- list of objects at this location
      end record;
   type Real_Map is array (Location_T) of Real_Map_T;

   type View_Map_T is
      record
         Contents : Content_Display_T;  -- '+', '.', '*', 'A', 'a', etc
         Seen : Integer;                -- date when last updated
      end record;
   type View_Map is array (Location_T) of View_Map_T;

   type Continent_Map is array (Location_T) of Boolean;

   Map : Real_map;                      -- the way the world really looks
   Comp_Map : View_Map;                 -- computer's view of the world
   User_Map : View_Map;                 -- user's view of the world

   City : array (1..NUM_CITY) of aliased City_Info_T; -- city information

   -- There is one array to hold all allocated objects no matter who
   -- owns them.  Objects are allocated from the array and placed on
   -- a list corresponding to the type of object and its owner.

   Free_List : Piece_Info_P;            -- list of free items in object list
   User_Obj : array (Piece_Type_T) of Piece_Info_P; -- user object lists
   Comp_Obj : array (Piece_Type_T) of Piece_Info_P; -- computer object lists
   Object : array (1..LIST_SIZE) of aliased Piece_Info_T; -- global object list

   -- miscellaneous global variables

   Smooth : Integer;                    -- number of times to smooth map
   Water_Ratio : Integer;               -- percentage of map that is water
   Min_City_Dist : Integer;             -- minimum distance between two cities
   Save_Interval : Integer;             -- turns between autosaves
   Traditional : Integer := 0;          -- `traditional' movement keys

   Date : Integer;                      -- number of game turns played
   Automove : Boolean;                  -- true IFF user is in automove mode
   Resigned : Boolean;                  -- true IFF computer resigned
   Debug : Boolean;                     -- true IFF in debugging mode
   Print_Debug : Boolean;               -- true IFF we print debugging output
   subtype Vmap_Debug_Option is Character; -- XXX restrict to A, I, L, S, U
   Print_Vmap : Vmap_Debug_Option;      -- option for printing vmaps;
   Trace_Pmap : Boolean;                -- true IFF we are tracing pmaps
   Win : Boolean;                       -- true IFF games is over
   Save_Movie : Boolean;                -- true IFF we're saving movie screens
   User_Score : Integer;                -- user `score'
   Comp_Score : Integer;                -- computer `score'

   -- global display-related variables

   Color : Boolean := TRUE;             -- use color if available
   Lines : Integer;                     -- lines on screen
   Cols : Integer;                      -- columns on screen

   -- exceptions
   User_Quit : exception;

   -- constant data

   -- singleton types for deferred array constants
   type City_Char_Array is array (Owner_T) of Content_Display_T;
   type Piece_Attr_Array is array (Piece_Type_T range ARMY .. SATELLITE) of Piece_Attr_T;
   type Dir_Offset_Array is array (Direction_T) of Integer;
   type Func_Name_Array is array (Function_T) of String_P;
   type Move_Order_Array is array (Natural range <>) of Piece_Type_T;

   Piece_Attr : constant Piece_Attr_array;
   Dir_Offset : constant Dir_Offset_Array;
   Func_Name : constant Func_Name_Array;
   Move_Order : constant Move_Order_Array;
   Tt_Attack : constant Content_Value_Array;
   Army_Attack : constant Content_Value_Array;
   Fighter_Attack : constant Content_Value_Array;
   Ship_Attack : constant Content_Value_Array;
   City_char : constant City_Char_Array;

   Tt_Load : constant Move_Info_T;
   Tt_Explore : constant Move_Info_T;
   Tt_Unload : constant Move_Info_T;
   Army_Fight : constant Move_Info_T;
   Army_Load : constant Move_Info_T;
   Fighter_Fight : constant Move_Info_T;
   Ship_Fight : constant Move_Info_T;
   Ship_Repair : constant Move_Info_T;
   User_Army : constant Move_Info_T;
   User_Army_Attack : constant Move_Info_T;
   User_Fighter : constant Move_Info_T;
   User_Ship : constant Move_Info_T;
   User_Ship_Repair : constant Move_Info_T;

   Help_Cmd : constant Help_Array;
   Help_Edit : constant Help_Array;
   Help_User : constant Help_Array;

   -- global routines
   procedure Run_Empire;
   procedure Emp_End;

private

   procedure Emp_Start;
   procedure Do_Command (Orders : in Character);

-- Piece attributes.  Notice that the Transport is allowed only one hit.
-- In an earlier version of this game, the user could easily win simply
-- by building armies and troop transports.  We attempt to alleviate this
-- problem by making transports far more fragile.  We have also increased
-- the range of a fighter from 20 to 30 so that fighters will be somewhat
-- more useful.

   -- values for Piece_Attr.*.Terrain.  XXX We could dispatch based on
   -- Class now, but don't yet

   GROUND_TERRAIN : constant Acceptable_Terrain_Array := ('+'|'*' => TRUE, '.' => FALSE);
   AIRCRAFT_TERRAIN : constant Acceptable_Terrain_Array := (others => TRUE);
   SHIP_TERRAIN : constant Acceptable_Terrain_Array := ('.'|'*' => TRUE, '+' => FALSE);
   SPACECRAFT_TERRAIN : constant Acceptable_Terrain_Array := AIRCRAFT_TERRAIN;

   Piece_Attr : constant Piece_Attr_Array :=
     (ARMY =>
        (Sname => 'A',                 -- character for printing piece
         Class => GROUND,
         Name => new String'("army"),   -- name of piece
         Nickname => new String'("army"), -- nickname
         Article => new String'("an army"), -- name with preceding article
         Plural => new String'("armies"), -- plural
         Terrain => GROUND_TERRAIN,     -- terrain
         Build_Time => 5,               -- units to build
         Strength => 1,                 -- streng
         Max_Hits => 1,                 -- max hits
         Speed => 1,                    -- movement
         Capacity => 0,                 -- capacity
         Piece_Range => INFINITY),      -- range of piece

      -- For fighters, the range is set to an even multiple of the speed.
      -- This allows user to move fighter, say, two turns out and two
      -- turns back.

      FIGHTER =>
        (Sname => 'F', Class => AIRCRAFT, Name => new String'("fighter"), Nickname => new String'("fighter"),
         Article => new String'("a fighter"), Plural => new String'("fighters"),
         Terrain => AIRCRAFT_TERRAIN,
         Build_Time => 10, Strength => 1, Max_Hits => 1,
         Speed => 8, Capacity => 0, Piece_Range => 32),

      PATROL =>
        (Sname => 'P', Class => SHIP, Name => new String'("patrol boat"), Nickname => new String'("patrol"),
         Article => new String'("a patrol boat"), Plural => new String'("patrol boats"),
         Terrain => SHIP_TERRAIN,
         Build_Time => 15, Strength => 1, Max_Hits => 1,
         Speed => 4, Capacity => 0, Piece_Range => INFINITY),

      DESTROYER =>
        (Sname => 'D', Class => SHIP, Name => new String'("destroyer"), Nickname => new String'("destroyer"),
         Article => new String'("a destroyer"), Plural => new String'("destroyers"),
         Terrain => SHIP_TERRAIN,
         Build_Time => 20, Strength => 1, Max_Hits => 3,
         Speed => 4, Capacity => 0, Piece_Range => INFINITY),

      SUBMARINE =>
        (Sname => 'S', Class => SHIP, Name => new String'("submarine"), Nickname => new String'("submarine"),
         Article => new String'("a submarine"), Plural => new String'("submarines"),
         Terrain => SHIP_TERRAIN,
         Build_Time => 20, Strength => 3, Max_Hits => 2,
         Speed => 2, Capacity => 0, Piece_Range => INFINITY),

      TRANSPORT =>
        (Sname => 'T', Class => SHIP, Name => new String'("troop transport"), Nickname => new String'("transport"),
         Article => new String'("a troop transport"), Plural => new String'("troop transports"),
         Terrain => SHIP_TERRAIN,
         Build_Time => 30, Strength => 1, Max_Hits => 1,
         Speed => 2, Capacity => 6, Piece_Range => INFINITY),

      CARRIER =>
        (Sname => 'C', Class => SHIP, Name => new String'("aircraft carrier"), Nickname => new String'("carrier"),
         Article => new String'("an aircraft carrier"), Plural => new String'("aircraft carriers"),
         Terrain => SHIP_TERRAIN,
         Build_Time => 30, Strength => 1, Max_Hits => 8,
         Speed => 2, Capacity => 8, Piece_Range => INFINITY),

      BATTLESHIP =>
        (Sname => 'B', Class => SHIP, Name => new String'("battleship"), Nickname => new String'("battleship"),
         Article => new String'("a battleship"), Plural => new String'("battleships"),
         Terrain => SHIP_TERRAIN,
         Build_Time => 40, Strength => 2, Max_Hits => 10,
         Speed => 2, Capacity => 0, Piece_Range => INFINITY),

      SATELLITE =>
        (Sname => 'Z', Class => SPACECRAFT, Name => new String'("satellite"), Nickname => new String'("satellite"),
         Article => new String'("a satellite"), Plural => new String'("satellites"),
         Terrain => SPACECRAFT_TERRAIN,
         Build_Time => 50, Strength => 0, Max_Hits => 1,
         Speed => 10, Capacity => 0, Piece_Range => 500));

  -- direction offsets
   Dir_Offset : constant Dir_Offset_Array :=
     (NORTH => -MAP_WIDTH,
      NORTHEAST => -MAP_WIDTH + 1,
      EAST => 1,
      SOUTHEAST => MAP_WIDTH + 1,
      SOUTH => MAP_WIDTH,
      SOUTHWEST => MAP_WIDTH - 1,
      WEST => -1,
      NORTHWEST => -MAP_WIDTH - 1);

  -- names of movement functions
   Func_Name : constant Func_Name_Array :=
     (NOFUNC => new String'("none"),
      RANDOM => new String'("random"),
      SENTRY => new String'("sentry"),
      FILL => new String'("fill"),
      LAND => new String'("land"),
      EXPLORE => new String'("explore"),
      ARMYLOAD => new String'("load"),
      ARMYATTACK => new String'("attack"),
      TTLOAD => new String'("load"),
      REPAIR => new String'("repair"),
      WFTRANSPORT => new String'("transport"),
      MOVE_N => new String'("W"),
      MOVE_NE => new String'("E"),
      MOVE_E => new String'("D"),
      MOVE_SE => new String'("C"),
      MOVE_S => new String'("X"),
      MOVE_SW => new String'("Z"),
      MOVE_W => new String'("A"),
      MOVE_NW => new String'("Q"),
      MOVE_TO_DEST => new String'("destination"));

   -- the order in which pieces should be moved
   -- alternative (easy enough) would be to put piece_type_t in move order.
   -- we can do this because we don't rely on all ships being > fighter
   -- as the c code does.
   Move_Order : constant Move_Order_Array :=
     (SATELLITE, TRANSPORT, CARRIER, BATTLESHIP, PATROL, SUBMARINE, DESTROYER, ARMY, FIGHTER);

   -- types of cities
   City_char : constant City_Char_Array :=
     (UNOWNED => '*',
      USER => 'O',
      COMP => 'X');

   -- lists of attackable objects if object is adjacent to moving piece
   -- order matters! (these are in priority order, most..least)

   Tt_Attack : constant Content_Value_Array := ('T' => 1, others => 0);
   Army_Attack : constant Content_Value_Array :=
     ('O' => 10, '*' => 9, 'T' => 8, 'A' => 7, 'C' => 6, 'F' => 5,
      'B' => 4, 'S' => 3, 'D' => 2, 'P' => 1, others => 0);
   Fighter_Attack : constant Content_Value_Array :=
     ('T' => 8, 'C' => 7, 'F' => 6, 'B' => 5, 'S' => 4, 'D' => 3,
      'P' => 2, 'A' => 1, others => 0);
   Ship_Attack : constant Content_Value_Array :=
     ('T' => 6, 'C' => 5, 'B' => 4, 'S' => 3, 'D' => 2, 'P' => 1,
      others => 0);

   -- define various types of objectives

   Tt_Explore : constant Move_Info_T :=
     (Owner => COMP, Objective_Weights => (' ' => 1, others => 0));
   Tt_Load : constant Move_Info_T :=
     (Owner => COMP, Objective_Weights => ('$' => 1, others => 0));

   -- Rationale for 'tt_unload':
   --
   --     Any continent with four or more cities is extremely attractive,
   -- and we should grab it quickly.  A continent with three cities is
   -- fairly attractive, but we are willing to go slightly out of our
   -- way to find a better continent.  Similarily for two cities on a
   -- continent.  At one city on a continent, things are looking fairly
   -- unattractive, and we are willing to go quite a bit out of our way
   -- to find a better continent.
   --
   --      Cities marked with a '0' are on continents where we already
   -- have cities, and these cities will likely fall to our armies anyway,
   -- so we don't need to dump armies on them unless everything else is
   -- real far away.  We would prefer to use unloading transports for
   -- taking cities instead of exploring, but we are willing to explore
   -- if interesting cities are too far away.
   --
   --      It has been suggested that continents containing one city
   -- are not interesting.  Unfortunately, most of the time what the
   -- computer sees is a single city on a continent next to lots of
   -- unexplored territory.  So it dumps an army on the continent to
   -- explore the continent and the army ends up attacking the city
   -- anyway.  So, we have decided we might as well send the tt to
   -- the city in the first place and increase the speed with which
   -- the computer unloads its tts.

   Tt_Unload : constant Move_Info_T :=
     (Owner => COMP, Objective_Weights =>
        ('9'|'8'|'7'|'6'|'5'|'4' => 1, '3' => 11, '2' => 21, '1' => 41,
         '0' => 101, ' ' => 61, others => 0));

   Army_Fight : constant Move_Info_T :=
     (Owner => COMP, Objective_Weights => ('O'|'*'|'T'|'A' => 1, ' ' => 11, others => 0));
   Army_Load : constant Move_Info_T :=
     (Owner => COMP, Objective_Weights => ('$' => 1, 'x' => W_TT_BUILD, others => 0));
   Fighter_Fight : constant Move_Info_T :=
     (Owner => COMP, Objective_Weights => ('T'|'C' => 1, 'F'|'B'|'S'|'D'|'P'|'A' => 5,
                                           ' ' => 9, others => 0));
   Ship_Fight : constant Move_Info_T :=
     (Owner => COMP, Objective_Weights => ('T'|'C' => 1, 'B'|'S'|'D'|'P' => 3, ' ' => 21,
                                           others => 0));
   Ship_Repair : constant Move_Info_T := (Owner => COMP, Objective_Weights => ('X' => 1,
                                                                               others => 0));

   User_Army : constant Move_Info_T := (Owner => USER, Objective_Weights => (' ' => 1,
                                                                             others => 0));
   User_Army_Attack : constant Move_Info_T := (Owner => USER, Objective_Weights =>
                                                 ('*'|'X'|'a' => 1, ' ' => 12, others => 0));
   User_Fighter : constant Move_Info_T := (Owner => USER, Objective_Weights => (' ' => 1,
                                                                                others => 0));
   User_Ship : constant Move_Info_T := (Owner => USER, Objective_Weights => (' ' => 1,
                                                                             others => 0));
   User_Ship_Repair : constant Move_Info_T := (Owner => USER, Objective_Weights => ('O' => 1,
                                                                                    others => 0));

   -- Various help texts.
   Help_Cmd : constant Help_Array :=
     (new String'("COMMAND MODE"),
      new String'("a - enter Automove mode"),
      new String'("c - give City to computer"),
      new String'("d - print game Date (round)"),
      new String'("e - Examine enemy map"),
      new String'("f - print map to File"),
      new String'("g - Give move to computer"),
      new String'("h - display this Help text"),
      new String'("j - enter edit mode"),
      new String'("m - Move"),
      new String'("n - give N moves to computer"),
      new String'("p - Print a sector"),
      new String'("q - Quit game"),
      new String'("r - Restore game"),
      new String'("s - Save game"),
      new String'("t - save movie in " & MOVIE_NAME),
      new String'("w - Watch movie"),
      new String'("z - display Zoomed out map"),
      new String'("<ctrl-L> - redraw screen"));

   Help_User : constant Help_Array :=
     (new String'("USER MODE"),
      new String'("QWE"),
      new String'("A D - movement directions"),
      new String'("ZXC"),
      new String'("<space>:           skip"),
      new String'("b - change city production"),
      new String'("f - set func to Fill"),
      new String'("g - set func to explore"),
      new String'("h - display this Help text"),
      new String'("i <dir> - set func to dir"),
      new String'("j - enter edit mode"),
      new String'("k - set func to awake"),
      new String'("l - set func to Land"),
      new String'("o - get Out of automove mode"),
      new String'("p - redraw screen"),
      new String'("r - set func to Random"),
      new String'("s - set func to Sentry"),
      new String'("u - set func to repair"),
      new String'("v <piece> <func> - set city func"),
      new String'("y - set func to attack"),
      new String'("<ctrl-L> - redraw screen"),
      new String'("= - describe piece"));

   Help_Edit : constant Help_Array :=
     (new String'("EDIT MODE"),
      new String'("QWE"),
      new String'("A D - movement directions"),
      new String'("ZXC"),
      new String'("b - change city production"),
      new String'("f - set func to Fill"),
      new String'("g - set func to explore"),
      new String'("h - display this Help text"),
      new String'("i <dir> - set func to dir"),
      new String'("k - set func to awake"),
      new String'("l - set func to Land"),
      new String'("m - Mark piece"),
      new String'("n - set dest for marked piece"),
      new String'("o - get Out of automove mode"),
      new String'("p - redraw screen"),
      new String'("r - set func to Random"),
      new String'("s - set func to Sentry"),
      new String'("u - set func to repair"),
      new String'("v <piece> <func> - set city func"),
      new String'("y - set func to attack"),
      new String'("<ctrl-L> - redraw screen"),
      new String'("= - describe piece"));

end Empire;
