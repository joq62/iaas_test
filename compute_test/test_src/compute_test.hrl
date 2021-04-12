%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% 
%% 
%% --------------------------------------------------------------------
-define(B1_Name,"board1").
-define(B2_Name,"board2").
-define(B3_Name,"board3").
-define(B1,list_to_atom(?B1_Name++"@"++compute_unit_test:host())).
-define(B2,list_to_atom(?B2_Name++"@"++compute_unit_test:host())).
-define(B3,list_to_atom(?B3_Name++"@"++compute_unit_test:host())).

-define(A_B1_Name,"a_board1").
-define(B_B1_Name,"b_board1").
-define(C_B1_Name,"c_board1").
-define(A_B1,list_to_atom(?A_B1_Name++"@"++compute_unit_test:host())).
-define(B_B1,list_to_atom(?B_B1_Name++"@"++compute_unit_test:host())).
-define(C_B1,list_to_atom(?C_B1_Name++"@"++compute_unit_test:host())).

-define(A_B2_Name,"a_board2").
-define(B_B2_Name,"b_board2").
-define(C_B2_Name,"c_board2").
-define(A_B2,list_to_atom(?A_B2_Name++"@"++compute_unit_test:host())).
-define(B_B2,list_to_atom(?B_B2_Name++"@"++compute_unit_test:host())).
-define(C_B2,list_to_atom(?C_B2_Name++"@"++compute_unit_test:host())).

-define(A_B3_Name,"a_board3").
-define(B_B3_Name,"b_board3").
-define(C_B3_Name,"c_board3").
-define(A_B3,list_to_atom(?A_B3_Name++"@"++compute_unit_test:host())).
-define(B_B3,list_to_atom(?B_B3_Name++"@"++compute_unit_test:host())).
-define(C_B3,list_to_atom(?C_B3_Name++"@"++compute_unit_test:host())).
