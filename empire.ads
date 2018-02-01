with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Bounded;
with Ada.Text_IO;

use type Ada.Containers.Count_Type;

package Empire is
   -- these include a `frame' of one cell all around the playable area.
   -- XXX maybe this should change, but it allows easy assumption that we can go
   -- XXX any direction from any playable cell, _then_ check if still on board.
   MAP_WIDTH : constant := 200;
   MAP_HEIGHT : constant := 100;
   MAP_SIZE : constant := MAP_WIDTH * MAP_HEIGHT;

   NUM_CITY : constant := 70;

   -- cost to switch a city's production is the _new_ item's cost over RETOOLING_DENOMINATOR
   RETOOLING_DENOMINATOR : constant := 5;

   -- these are still from 0 so modulo, etc are easy to use.
   subtype Location_T is Integer range 0 .. MAP_SIZE-1; -- map location as index into map
   subtype Row_T is Integer range 0 .. MAP_HEIGHT-1;
   subtype Column_T is Integer range 0 .. MAP_WIDTH-1;

   LIST_SIZE : constant Integer := 5000; -- max number of pieces on board

   package Location_Vectors is new Ada.Containers.Vectors
     (Element_Type => Location_T,
      Index_Type => Positive);
   use Location_Vectors;
   subtype Location_Vector is Location_Vectors.Vector;
   subtype Location_Count is Ada.Containers.Count_Type;

   STRING_MAX : constant := 256;
   package Strings is new Ada.Strings.Bounded.Generic_Bounded_Length(STRING_MAX);
   subtype Bstring is Strings.Bounded_String;

   type Help_Array is array (Natural range <>) of Bstring;

   SAVE_NAME : constant String := "empsave.dat";
   MOVIE_NAME : constant String := "empmovie.dat";
   MAP_NAME : constant String := "empmap.txt";

   -- useful constants for accessing sectors

   ROWS_PER_SECTOR : constant := 20; --  should divide evenly into MAP_HEIGHT, and be likely to fit on screen
   -- was      (MAP_HEIGHT + SECTOR_ROWS - 1) / SECTOR_ROWS;
   COLS_PER_SECTOR : constant := 50;    --  should divide evenly into MAP_WIDTH, and be likely to fit on screen
   -- was       (MAP_WIDTH + SECTOR_COLS - 1) / SECTOR_COLS;
   SECTOR_ROWS : constant := MAP_HEIGHT / ROWS_PER_SECTOR;  -- number of vertical sectors
   SECTOR_COLS : constant := MAP_WIDTH / COLS_PER_SECTOR;  -- number of horizontal sectors
   NUM_SECTORS : constant := SECTOR_ROWS * SECTOR_COLS; -- total sectors

   subtype Sector_T is Integer range 0 .. NUM_SECTORS - 1;

   -- XXX XXX XXX this is really broken.  INFINITY, as used here, has to be small enough that we can add a fair number of them together within the range
   -- XXX XXX XXX of an Integer, yet large enough that we wouldn't expect to see it.  Grrr....
   -- INFINITY : constant Integer := Integer'Last;
   INFINITY : constant := 1_000_000;

   VERSION_STRING : constant String := "ADA EMPIRE, Version 1.4_ALPHA0, February 2006";

   type Owner_T is (UNOWNED, USER, COMP);
   subtype Piece_Owner_T is Owner_T range USER .. COMP;
   type Acceptable_Owner_Array is array (Owner_T) of Boolean;

   -- when adding a new piece type, must add here, in piece_attr below,
   -- and at least also in switch in Empire.Mapping.Vmap_Cont_Scan
   -- XXX (undoubtedly others, too)
   -- XXX XXX note that piece types here need to be in preference order
   -- XXX XXX for Find_Obj_At_Loc (least -> most).  This can be replaced
   -- XXX XXX by a constant piece_value_array, making this tunable
   type Piece_Choice_T is
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
  subtype Piece_Type_T is Piece_Choice_T range ARMY .. SATELLITE;

  type Piece_Value_Array is array (Piece_Type_T) of Integer;
  type Acceptable_Piece_Array is array (Piece_Type_T) of Boolean;

  type Piece_Class_T is
     (GROUND,
      AIRCRAFT,
      SHIP,
      SPACECRAFT);

   -- in addition to terrain and user types, planning algorithms use following special symbols in view maps
   -- '$' represents loading tt must be first
   --  'x' represents transport-producing city
   --  '0' .. '9' represent explorable territory

   type Content_Display_T is
      ('.', '+', '*', 'O', 'X', ' ',
       'A', 'a', 'F', 'f', 'P', 'p', 'D', 'd', 'S', 's', 'T', 't', 'C', 'c', 'B', 'b', 'Z', 'z',
       '$', 'x', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '%');
   subtype Terrain_Display_T is Content_Display_T range '.' .. '*';

   type Acceptable_Content_Array is array (Content_Display_T) of Boolean;
   type Acceptable_Terrain_Array is array (Terrain_Display_T) of Boolean;
   type Content_Value_Array is array (Content_Display_T) of Integer;

   Comp_Content : Constant Acceptable_Content_Array :=
     (
      'a'|'f'|'p'|'d'|'s'|'t'|'c'|'b'|'z'|'X' => True,
      others => False
     );

   User_Content : Constant Acceptable_Content_Array :=
     (
      'A'|'F'|'P'|'D'|'S'|'T'|'C'|'B'|'Z'|'O' => True,
      others => False
     );

   package Content_IO is new Ada.Text_IO.Enumeration_IO(Content_Display_T);

   type Direction_Choice_T is
      (NORTH,
       NORTHEAST,
       EAST,
       SOUTHEAST,
       SOUTH,
       SOUTHWEST,
       WEST,
       NORTHWEST,
       NODIRECTION);
   subtype Direction_T is Direction_Choice_T range NORTH .. NORTHWEST;

   -- in original, function_t was overloaded, with negative values meaning as indicated
   -- below, and positive values indicatign a location_t destination.
   -- we now use MOVE_TO_DEST, and the piece_info_t's (new) `Dest' field.
   --
   -- also, despite defined values, the original computer move implementation (compmove.c)
   -- used two values, '0' and '1' (only), giving them the meaning 'LOADING' and 'UNLOADING'.
   -- for now, we include these in the type.
   --
   -- XXX XXX better would be to make Piece_Info_T a variant record, where `Func' took on
   -- a new User_Function_T or Comp_Function_T depending on Owner
   type Function_T is
      (NOFUNC,                          -- no programmed function (-1)
       RANDOM,                          -- move randomly (-2)
       SENTRY,                          -- sleep (-3)
       FILL,                            -- load transport (-4)
       LAND,                            -- land fighter at city (-5)
       EXPLORE,                         -- explore (-6)
       ARMYATTACK,                      -- army looks for city to attack (-8)
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
       MOVE_TO_DEST,                    -- move to Obj.Dest
       COMP_LOADING,                    -- Comp_Move only
       COMP_UNLOADING);                 -- Comp_Move only

   Move_Func_Directions : constant array (Function_T range MOVE_N .. MOVE_NW) of Direction_T :=
     (
      MOVE_N => NORTH,
      MOVE_NE => NORTHEAST,
      MOVE_E => EAST,
      MOVE_SE => SOUTHEAST,
      MOVE_S => SOUTH,
      MOVE_SW => SOUTHWEST,
      MOVE_W => WEST,
      MOVE_NW => NORTHWEST
     );

   Move_Dir_Functions : constant array (Direction_T range NORTH .. NORTHWEST) of Function_T :=
     (
      NORTH => MOVE_N,
      NORTHEAST => MOVE_NE,
      EAST => MOVE_E,
      SOUTHEAST => MOVE_SE,
      SOUTH => MOVE_S,
      SOUTHWEST => MOVE_SW,
      WEST => MOVE_W,
      NORTHWEST => MOVE_NW
     );

   -- if we determine how to represent T_PATH, this could be replaced with a set of constants of
   -- type Acceptable_Terrain_Array
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

   type Piece_Info_P is access Piece_Info_T;

   type Link_T is
      record
         Next : Piece_Info_P;
         Prev : Piece_Info_P;
      end record;

   type Link_Type_T is (Piece_Link, Loc_Link, Cargo_Link);
   type Link_Array is array (Link_Type_T'Range) of Link_T;

   type Piece_Info_T is
      record
         Links : Link_Array;    --  linked lists of pieces of this type, at this loc, and in this transport/carrier
         Owner : Piece_Owner_T;         -- owner of piece
         Piece_Type : Piece_Type_T;     -- type of piece
         Loc : Location_T;              -- location of piece
         Func : Function_T;             -- programmed type of movement
         Dest : Location_T;             -- if Func = MOVE_TO_DEST
         Hits : Integer;                -- hits left
         Moved : Integer;               -- moves made
         Ship : Piece_Info_P;           -- pointer to containing ship
         Cargo : Piece_Info_P;          -- pointer to cargo list
         Count : Integer;               -- count of items on board
         Piece_Range : Integer;         -- current range if applicable
      end record;


   -- We maintain attributes for each piece.  Attributes are currently constant
   -- but the way has been paved to allow user's to change attributes at the
   -- beginning of a game.

   type Piece_Attr_T is
      record
         U_Cont : Content_Display_T;     -- user version, eg 'A'
         C_Cont : Content_Display_T;     -- computer version, eg 'a'
         Sel_Char : Character;           -- shortcut char for menus
         Class : Piece_Class_T;         -- general type of piece
         Name : Bstring;                -- eg "aircraft carrier" XXX XXX wasteful -- should we have a "short_string" type?
         Nickname : Bstring;            -- eg "carrier"
         Article : Bstring;             -- eg "an aircraft carrier"
         Plural : Bstring;              -- eg "aircraft carriers" (XXX not used)
         Terrain : Acceptable_Content_Array; -- terrain piece can pass over
         Build_Time : Integer;          -- time needed to build unit
         Strength : Integer;            -- attack strength
         Max_Hits : integer;            -- number of hits when completely repaired
         Speed : Integer;               -- number of squares moved per turn
         Capacity : Integer;            -- max objects that can be held
         Piece_Range : Integer;         -- range of piece
      end record;

   type Dest_Array is array (Piece_Type_T) of Location_T;
   type Function_Array is array (Piece_Type_T) of Function_T;
   type City_Info_T is
      record
         Loc : Location_T;                -- location of city
         Owner : Owner_T;
         Func : Function_Array;         -- function for each object (XXX size?)
         Dest : Dest_Array;             -- destination if func is MOVE_TO_DEST
         work : Integer;                -- units of work performed
         Prod : Piece_Choice_T;           -- item being produced
      end record;

   type City_Info_P is access all City_Info_T;

   -- A record for counts we obtain when scanning a continent

   type Scan_Counts_T is
      record
         User_Cities : Integer := 0;    -- number of user cities on continent
         User_Objects : Piece_Value_Array := (others => 0);
         Comp_Cities : Integer := 0;
         Comp_Objects : Piece_Value_Array := (others => 0);
         Size : Integer := 0;           -- size of continent in cells
         Unowned_Cities : Integer := 0; -- number of unowned cities
         Unexplored : Integer := 0;     -- unexplored territory
      end record;

   -- Information we need for finding a path for moving a piece
   type Move_Info_T is
      record
         Owner : Owner_t;               -- applicable side
         Objective_Weights : Content_Value_Array; -- maps objectives to weights (0 = not an objective)
      end record;

   -- special cost for a city building a tt
   W_TT_BUILD : constant Integer := -1;

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
         Seen : Natural;                -- date when last updated
      end record;
   type View_Map is array (Location_T) of View_Map_T;

   type Continent_Map is array (Location_T) of Boolean;

   Map : Real_map;                      -- the way the world really looks
   View : array (Piece_Owner_T'Range) of View_Map; --  COMP and USER views of the world

   -- NOTA BENE the above replaced:
   -- Comp_Map : View_Map;                 -- computer's view of the world
   -- User_Map : View_Map;                 -- user's view of the world

   City : array (1..NUM_CITY) of aliased City_Info_T; -- city information

   -- There is one array to hold all allocated objects no matter who
   -- owns them.  Objects are allocated from the array and placed on
   -- a list corresponding to the type of object and its owner.

   Free_List : Piece_Info_P;            -- list of free items in object list
   User_Obj : array (Piece_Type_T) of Piece_Info_P; -- user object lists
   Comp_Obj : array (Piece_Type_T) of Piece_Info_P; -- computer object lists
   Object : array (1..LIST_SIZE) of Piece_Info_P := (others => new Piece_Info_T); -- global object list

   -- miscellaneous global variables

   Smooth : Integer;                    -- number of times to smooth map
   Water_Ratio : Natural;               -- percentage of map that is water
   Min_City_Dist : Natural;             -- minimum distance between two cities
   Save_Interval : Natural;             -- turns between autosaves
   Traditional : Boolean := FALSE;      -- `traditional' movement keys

   Date : Natural;                      -- number of game turns played
   Automove : Boolean;                  -- true IFF user is in automove mode
   Resigned : Boolean;                  -- true IFF computer resigned
   Debug : Boolean;                     -- true IFF in debugging mode
   Print_Debug : Boolean;               -- true IFF we print debugging output
   subtype Vmap_Debug_Option is Character; -- XXX Restrict to [AILSU0] (0 is none) XXX and perhaps allow multiple
   Print_Vmap : Vmap_Debug_Option;      -- option for printing vmaps - XXX should probably be a _set_ of options
   Trace_Pmap : Boolean;                -- true IFF we are tracing pmaps
   Win : Owner_T := UNOWNED;            -- true IFF games is over
   Save_Movie : Boolean;                -- true IFF we're saving movie screens
   User_Score : Integer;                -- user `score'
   Comp_Score : Integer;                -- computer `score'

   -- global display-related variables

   Color : Boolean := TRUE;             -- use color if available
   -- Lines : Integer;                     -- lines on screen
   -- Cols : Integer;                      -- columns on screen

   -- exceptions
   User_Quit : exception;

   -- constant data

   -- singleton types for deferred array constants
   type City_Char_Array is array (Owner_T) of Content_Display_T;
   type Piece_Attr_Array is array (Piece_Type_T range ARMY .. SATELLITE) of Piece_Attr_T;
   type Dir_Offset_Array is array (Direction_T) of Integer;
   type Function_Name_Array is array (Function_T) of Bstring;
   type Move_Order_Array is array (Natural range <>) of Piece_Type_T;

   Piece_Attr : constant Piece_Attr_array;
   Dir_Offset : constant Dir_Offset_Array;
   Function_Name : constant Function_Name_Array;
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

   GROUND_TERRAIN : constant Acceptable_Content_Array := ('+' => TRUE, others => FALSE);
   AIRCRAFT_TERRAIN : constant Acceptable_Content_Array := ('.'|'+' => TRUE, others => FALSE);
   SHIP_TERRAIN : constant Acceptable_Content_Array := ('.' => TRUE, others => FALSE);
   SPACECRAFT_TERRAIN : constant Acceptable_Content_Array := AIRCRAFT_TERRAIN;

   Piece_Attr : constant Piece_Attr_Array :=
     (ARMY =>
        (U_Cont => 'A', C_Cont => 'a',  -- character for printing piece
         Sel_Char => 'A',
         Class => GROUND,
         Name => Strings.To_Bounded_String("army"),   -- name of piece
         Nickname => Strings.To_Bounded_String("army"), -- nickname
         Article => Strings.To_Bounded_String("an army"), -- name with preceding article
         Plural => Strings.To_Bounded_String("armies"), -- plural
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
        (U_Cont => 'F', C_Cont => 'f', Sel_Char => 'F', Class => AIRCRAFT, Name => Strings.To_Bounded_String("fighter"), Nickname => Strings.To_Bounded_String("fighter"),
         Article => Strings.To_Bounded_String("a fighter"), Plural => Strings.To_Bounded_String("fighters"),
         Terrain => AIRCRAFT_TERRAIN,
         Build_Time => 10, Strength => 1, Max_Hits => 1,
         Speed => 8, Capacity => 0, Piece_Range => 32),

      PATROL =>
        (U_Cont => 'P', C_Cont => 'p', Sel_Char => 'P', Class => SHIP, Name => Strings.To_Bounded_String("patrol boat"), Nickname => Strings.To_Bounded_String("patrol"),
         Article => Strings.To_Bounded_String("a patrol boat"), Plural => Strings.To_Bounded_String("patrol boats"),
         Terrain => SHIP_TERRAIN,
         Build_Time => 15, Strength => 1, Max_Hits => 1,
         Speed => 4, Capacity => 0, Piece_Range => INFINITY),

      DESTROYER =>
        (U_Cont => 'D', C_Cont => 'd', Sel_Char => 'D', Class => SHIP, Name => Strings.To_Bounded_String("destroyer"), Nickname => Strings.To_Bounded_String("destroyer"),
         Article => Strings.To_Bounded_String("a destroyer"), Plural => Strings.To_Bounded_String("destroyers"),
         Terrain => SHIP_TERRAIN,
         Build_Time => 20, Strength => 1, Max_Hits => 3,
         Speed => 4, Capacity => 0, Piece_Range => INFINITY),

      SUBMARINE =>
        (U_Cont => 'S', C_Cont => 's', Sel_Char => 'S', Class => SHIP, Name => Strings.To_Bounded_String("submarine"), Nickname => Strings.To_Bounded_String("submarine"),
         Article => Strings.To_Bounded_String("a submarine"), Plural => Strings.To_Bounded_String("submarines"),
         Terrain => SHIP_TERRAIN,
         Build_Time => 20, Strength => 3, Max_Hits => 2,
         Speed => 2, Capacity => 0, Piece_Range => INFINITY),

      TRANSPORT =>
        (U_Cont => 'T', C_Cont => 't', Sel_Char => 'T', Class => SHIP, Name => Strings.To_Bounded_String("troop transport"), Nickname => Strings.To_Bounded_String("transport"),
         Article => Strings.To_Bounded_String("a troop transport"), Plural => Strings.To_Bounded_String("troop transports"),
         Terrain => SHIP_TERRAIN,
         Build_Time => 30, Strength => 1, Max_Hits => 1,
         Speed => 2, Capacity => 6, Piece_Range => INFINITY),

      CARRIER =>
        (U_Cont => 'C', C_Cont => 'c', Sel_Char => 'C', Class => SHIP, Name => Strings.To_Bounded_String("aircraft carrier"), Nickname => Strings.To_Bounded_String("carrier"),
         Article => Strings.To_Bounded_String("an aircraft carrier"), Plural => Strings.To_Bounded_String("aircraft carriers"),
         Terrain => SHIP_TERRAIN,
         Build_Time => 30, Strength => 1, Max_Hits => 8,
         Speed => 2, Capacity => 8, Piece_Range => INFINITY),

      BATTLESHIP =>
        (U_Cont => 'B', C_Cont => 'b', Sel_Char => 'B', Class => SHIP, Name => Strings.To_Bounded_String("battleship"), Nickname => Strings.To_Bounded_String("battleship"),
         Article => Strings.To_Bounded_String("a battleship"), Plural => Strings.To_Bounded_String("battleships"),
         Terrain => SHIP_TERRAIN,
         Build_Time => 40, Strength => 2, Max_Hits => 10,
         Speed => 2, Capacity => 0, Piece_Range => INFINITY),

      SATELLITE =>
        (U_Cont => 'Z', C_Cont => 'z', Sel_Char => 'Z', Class => SPACECRAFT, Name => Strings.To_Bounded_String("satellite"), Nickname => Strings.To_Bounded_String("satellite"),
         Article => Strings.To_Bounded_String("a satellite"), Plural => Strings.To_Bounded_String("satellites"),
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
   Function_Name : constant Function_Name_Array :=
     (NOFUNC => Strings.To_Bounded_String("none"),
      RANDOM => Strings.To_Bounded_String("random"),
      SENTRY => Strings.To_Bounded_String("sentry"),
      FILL => Strings.To_Bounded_String("fill"),
      LAND => Strings.To_Bounded_String("land"),
      EXPLORE => Strings.To_Bounded_String("explore"),
      ARMYATTACK => Strings.To_Bounded_String("attack"),
      REPAIR => Strings.To_Bounded_String("repair"),
      WFTRANSPORT => Strings.To_Bounded_String("transport"),
      MOVE_N => Strings.To_Bounded_String("W"),
      MOVE_NE => Strings.To_Bounded_String("E"),
      MOVE_E => Strings.To_Bounded_String("D"),
      MOVE_SE => Strings.To_Bounded_String("C"),
      MOVE_S => Strings.To_Bounded_String("X"),
      MOVE_SW => Strings.To_Bounded_String("Z"),
      MOVE_W => Strings.To_Bounded_String("A"),
      MOVE_NW => Strings.To_Bounded_String("Q"),
      MOVE_TO_DEST => Strings.To_Bounded_String("destination"),
      COMP_LOADING => Strings.To_Bounded_String("computer-internal-loading"), -- only used in debug output of Empire.Comp_Move
      COMP_UNLOADING => Strings.To_Bounded_String("computer-internal-unloading")); -- likewise

   -- the order in which pieces should be moved
   -- alternative (easy enough) would be to put piece_type_t in move order.
   -- we can do this because we don't rely on all ships being > fighter
   -- as the c code does.
   --
   -- NOTA BENE:  for user, move order is obeyed WITHIN EACH SECTOR
   --
   -- XXX XXX XXX IT IS CURRENTLY ENTIRELY THE DEVELOPER'S RESPONSIBILITY TO MAKE SURE EVERY PIECE IS HERE!!!
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
     (Owner => COMP,
      Objective_Weights => ('9'|'8'|'7'|'6'|'5'|'4' => 1, '3' => 11, '2' => 21, '1' => 41,
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

   Ship_Repair : constant Move_Info_T :=
     (Owner => COMP, Objective_Weights => ('X' => 1, others => 0));

   User_Army : constant Move_Info_T :=
     (Owner => USER, Objective_Weights => (' ' => 1, others => 0));

   User_Army_Attack : constant Move_Info_T :=
     (Owner => USER, Objective_Weights => ('*'|'X'|'a' => 1, ' ' => 12, others => 0));

   User_Fighter : constant Move_Info_T :=
     (Owner => USER, Objective_Weights => (' ' => 1, others => 0));

   User_Ship : constant Move_Info_T :=
     (Owner => USER, Objective_Weights => (' ' => 1, others => 0));

   User_Ship_Repair : constant Move_Info_T :=
     (Owner => USER, Objective_Weights => ('O' => 1, others => 0));

   -- Various help texts.
   Help_Cmd : constant Help_Array :=
     (Strings.To_Bounded_String("COMMAND MODE"),
      Strings.To_Bounded_String("a - enter Automove mode"),
      Strings.To_Bounded_String("c - give City to computer"),
      Strings.To_Bounded_String("d - print game Date (round)"),
      Strings.To_Bounded_String("e - Examine enemy map"),
      Strings.To_Bounded_String("f - print map to File"),
      Strings.To_Bounded_String("g - Give move to computer"),
      Strings.To_Bounded_String("h - display this Help text"),
      Strings.To_Bounded_String("j - enter edit mode"),
      Strings.To_Bounded_String("m - Move"),
      Strings.To_Bounded_String("n - give N moves to computer"),
      Strings.To_Bounded_String("p - Print a sector"),
      Strings.To_Bounded_String("q - Quit game"),
      Strings.To_Bounded_String("r - Restore game"),
      Strings.To_Bounded_String("s - Save game"),
      Strings.To_Bounded_String("t - save movie in " & MOVIE_NAME),
      Strings.To_Bounded_String("w - Watch movie"),
      Strings.To_Bounded_String("z - display Zoomed out map"),
      Strings.To_Bounded_String("<ctrl-L> - redraw screen"));

   Help_User : constant Help_Array :=
     (Strings.To_Bounded_String("USER MODE"),
      Strings.To_Bounded_String("QWE"),
      Strings.To_Bounded_String("A D - movement directions"),
      Strings.To_Bounded_String("ZXC"),
      Strings.To_Bounded_String("<space>:           skip"),
      Strings.To_Bounded_String("b - change city production"),
      Strings.To_Bounded_String("f - set func to Fill"),
      Strings.To_Bounded_String("g - set func to explore"),
      Strings.To_Bounded_String("h - display this Help text"),
      Strings.To_Bounded_String("i <dir> - set func to dir"),
      Strings.To_Bounded_String("j - enter edit mode"),
      Strings.To_Bounded_String("k - set func to awake"),
      Strings.To_Bounded_String("l - set func to Land"),
      Strings.To_Bounded_String("o - get Out of automove mode"),
      Strings.To_Bounded_String("p - redraw screen"),
      Strings.To_Bounded_String("r - set func to Random"),
      Strings.To_Bounded_String("s - set func to Sentry"),
      Strings.To_Bounded_String("u - set func to repair"),
      Strings.To_Bounded_String("v <piece> <func> - set city func"),
      Strings.To_Bounded_String("y - set func to attack"),
      Strings.To_Bounded_String("<ctrl-L> - redraw screen"),
      Strings.To_Bounded_String("= - describe piece"));

   Help_Edit : constant Help_Array :=
     (Strings.To_Bounded_String("EDIT MODE"),
      Strings.To_Bounded_String("QWE"),
      Strings.To_Bounded_String("A D - movement directions"),
      Strings.To_Bounded_String("ZXC"),
      Strings.To_Bounded_String("b - change city production"),
      Strings.To_Bounded_String("f - set func to Fill"),
      Strings.To_Bounded_String("g - set func to explore"),
      Strings.To_Bounded_String("h - display this Help text"),
      Strings.To_Bounded_String("i <dir> - set func to dir"),
      Strings.To_Bounded_String("k - set func to awake"),
      Strings.To_Bounded_String("l - set func to Land"),
      Strings.To_Bounded_String("m - Mark piece"),
      Strings.To_Bounded_String("n - set dest for marked piece"),
      Strings.To_Bounded_String("o - get Out of automove mode"),
      Strings.To_Bounded_String("p - redraw screen"),
      Strings.To_Bounded_String("r - set func to Random"),
      Strings.To_Bounded_String("s - set func to Sentry"),
      Strings.To_Bounded_String("u - set func to repair"),
      Strings.To_Bounded_String("v <piece> <func> - set city func"),
      Strings.To_Bounded_String("y - set func to attack"),
      Strings.To_Bounded_String("<ctrl-L> - redraw screen"),
      Strings.To_Bounded_String("= - describe piece"));
end Empire;
