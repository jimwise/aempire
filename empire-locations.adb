package body Empire.Locations is

   function Loc_Row (Loc : in Location_T) return Row_T is
   begin
      return Loc / MAP_WIDTH;
   end Loc_Row;

   function Loc_Col (Loc : in Location_T) return Column_T is
   begin
      return Loc mod MAP_WIDTH;
   end Loc_Col;

   function Row_Col_Loc (Row : in Row_T; Col : in Column_T) return Location_T is
   begin
      return Row * Map_Width + Col;
   end Row_Col_Loc;

   function Sector_Row (Sec : in Sector_T) return Row_T is
   begin
      return Sec mod SECTOR_ROWS;
   end Sector_Row;

   function Sector_Col (Sec : in Sector_T) return Column_T is
   begin
      return Sec / SECTOR_ROWS;
   end Sector_Col;

   function Row_Col_Sector (Row : in Row_T; Col : in Column_T) return Sector_T is
   begin
      return Col * SECTOR_ROWS + Row;
   end;

   function Loc_Sector (Loc : in Location_T) return Sector_T is
   begin
      return Row_Col_Sector(Loc_Row(Loc)/ROWS_PER_SECTOR, Loc_Col(Loc)/COLS_PER_SECTOR);
   end Loc_Sector;

   function Sector_Loc (Sec : in Sector_T) return Location_T is
   begin
      return Row_Col_Loc(Sector_Row(Sec) * ROWS_PER_SECTOR + ROWS_PER_SECTOR / 2,
                         Sector_Col(Sec) * COLS_PER_SECTOR + COLS_PER_SECTOR / 2);
   end Sector_Loc;

end Empire.Locations;
