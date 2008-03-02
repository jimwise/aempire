with Empire.Ui;
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
   procedure Regen_Land (Placed : in Natural; Num_Land : in out Natural);
   procedure Remove_Land (Loc : in Location_T; Num_Land : in out Natural);
   function Select_Cities return Boolean;
   procedure Stat_Display(Frame : in Ui.Movie_Screen; Round : in Natural);

   Max_Height : constant := 999;
   -- really two arrays, we smooth from one to the other, and then back
   Height : array (1..2, Map'range) of Integer;
   Height_Count : array (0..Max_Height) of Integer := (others => 0);

   Land : array (Map'Range) of Location_T;

   MAX_CONT : constant := 10;

   -- these arrays are actually too big -- unless all cities end up on one continent (!)
   subtype City_Index_T is Integer range 1 .. NUM_CITY;
   subtype Cont_Index_T is Integer range 1 .. MAX_CONT;
   type Cont_City_Array is array (City_Index_T) of City_Info_P;

   type Cont_T is
      record
         Value : Integer;
         Ncity : Natural;
         Cityp : Cont_City_Array;
      end record;

   type Pair_T is
      record
         Value : Integer;               --  value of pair for user
         User_Cont : City_Index_T;
         Comp_Cont : City_Index_T;
      end record;

   Marked : array (Location_T) of Boolean; --  cells we've examined
   ncont : Natural;
   Cont_Tab : array (Cont_Index_T) of Cont_T; --  good continents
   Rank_Tab : array (Cont_Index_T)  of Cont_Index_T; --  sorted indices into cont_tab, in rank order
   Pair_Tab : array (1 .. MAX_CONT**2) of Pair_T; --  ranked pairs of continents

   -- XXX XXX this cries out for a record type to return from mark_cont and pass around
   Ncity : Integer := 0;
   Nland : Integer := 0;
   Nshore : Integer := 0;

-- static int      xread (FILE *, void *, int);
-- static int      xwrite (FILE *, void *, int);

end Empire.Game;
