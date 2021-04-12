%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc : represent a logical vm   
%%% 
%%% Supports the system with standard erlang vm functionality, load and start
%%% of an erlang application (downloaded from git hub) and "dns" support 
%%% 
%%% Make and start the board start SW.
%%%  boot_service initiates tcp_server and l0isten on port
%%%  Then it's standby and waits for controller to detect the board and start to load applications
%%% 
%%%     
%%% -------------------------------------------------------------------
-module(start_system_test). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("test_src/compute_test.hrl").
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-define(WAIT_FOR_TABLES,10000).	  
-define(MaxRandNum,5).
%% --------------------------------------------------------------------
-export([]).

%% ====================================================================
%% External functions
%% ====================================================================

initial_test()->
    ?assertMatch([{ok,_},
		  {ok,_},
		  {ok,_}],
		 [rpc:call(?B1,slave,start,[compute_unit_test:host(),VmId,"-setcookie abc"])||VmId<-[?A_B1_Name,
										   ?B_B1_Name,
										   ?C_B1_Name]]),
    
    
    ?assertMatch([{ok,_},
		  {ok,_},
		  {ok,_}],
		 [rpc:call(?B2,slave,start,[compute_unit_test:host(),VmId,"-setcookie abc"])||VmId<-[?A_B2_Name,
										   ?B_B2_Name,
										   ?C_B2_Name]]),
    ?assertMatch([{ok,_},
		  {ok,_},
		  {ok,_}],
		 [rpc:call(?B3,slave,start,[compute_unit_test:host(),VmId,"-setcookie abc"])||VmId<-[?A_B3_Name,
										   ?B_B3_Name,
										   ?C_B3_Name]]),
    % 14 = 3*board+9*slaves+compute+ ???
    ?assertEqual(14,lists:flatlength(nodes())),
    

    ok.

