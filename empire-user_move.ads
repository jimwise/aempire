package Empire.User_Move is
   procedure User_Move;
private
   procedure Ask_User (Obj : in out Piece_Info_T);
   procedure Awake (Obj : in out Piece_Info_T; Awoke : out boolean);
   procedure Piece_Move (Obj : in out Piece_Info_T);
   procedure Fatal (Obj : in out Piece_Info_T; Loc : in Location_T; Msg : in String_P; Response : out Boolean);

   procedure Move_Armyattack (Obj : in out Piece_Info_T);
   procedure Move_Armyload (Obj : in out Pice_Info_T);
   procedure Move_Dir (Obj : in out Piece_Info_T);
   procedure Move_Explore (Obj : in out Piece_Info_T);
   procedure Move_Fill (Obj : in out Piece_Info_T);
   procedure Move_Land (Obj : in out Piece_Info_T);
   procedure Move_Path (Obj : in out Piece_Info_T);
   procedure Move_Random (Obj : in out Piece_Info_T);

--  void    move_repair (piece_info_t *obj);
--  void    move_to_dest (piece_info_t *, long);
--  void    move_transport (piece_info_t *);
--  void    move_ttload (piece_info_t *);

   --  void    reset_func (piece_info_t *);
--  void    user_armyattack (piece_info_t *);
--  void    user_build (piece_info_t *);
--  void    user_cancel_auto (void);
--  void    user_dir (piece_info_t *, direction_t);
--  void    user_dir_army (piece_info_t *, long);
--  void    user_dir_fighter (piece_info_t *, long);
--  void    user_dir_ship (piece_info_t *, long);
--  void    user_explore (piece_info_t *);
--  void    user_fill (piece_info_t *);
--  void    user_land (piece_info_t *);
--  void    user_random (piece_info_t *);
--  void    user_repair (piece_info_t *);
--  void    user_set_city_func (piece_info_t *);
--  void    user_set_dir (piece_info_t *);
--  void    user_sentry (piece_info_t *);
--  void    user_skip (piece_info_t *);
--  void    user_transport (piece_info_t *);
--  void    user_wake (piece_info_t *);

   procedure Move_Army_To_City (Obj : in out Piece_Info_T; Loc : in Location_T);
end Empire.User_Move;
