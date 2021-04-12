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
-module(start_stop_apps_test). 

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
    AppId="common",
    GitCmd="git clone https://github.com/joq62/common_src.git common",
    DestDir=AppId,
    PathList=["ebin"],
    ok=git_load_start_app(?A_B1,AppId,GitCmd,DestDir,PathList),
    ok=stop_unload_app(?A_B1,AppId,DestDir),
    ok.

git_load_start_app(Slave,AppId,GitCmd,DestDir,PathList)->
    rpc:call(Slave,os,cmd,["rm -rf "++DestDir]),
    rpc:call(Slave,os,cmd,[GitCmd]),
    %% Add path to vm and start the application 
    {ok,DirParent}=rpc:call(Slave,file,get_cwd,[]),
    FullNamePathList=[filename:join([DirParent,DestDir,Path])||Path<-PathList],
    [rpc:call(Slave,code,add_patha,[FullNamePath])||FullNamePath<-FullNamePathList],
    ok=rpc:call(Slave,application,start,[list_to_atom(AppId)]),
    App=list_to_atom(AppId),
    {pong,Slave,App}=rpc:call(Slave,list_to_atom(AppId),ping,[]),
    ok.
  
stop_unload_app(Slave,AppId,DestDir)->
    ok=rpc:call(Slave,application,stop,[list_to_atom(AppId)]),
    {ok,DirParent}=rpc:call(Slave,file,get_cwd,[]),
    Path=filename:join([DirParent,AppId,"ebin"]),
    true=rpc:call(Slave,code,del_path,[Path]), 
    {badrpc,_}=rpc:call(Slave,list_to_atom(AppId),ping,[]),
    ok=rpc:call(Slave,application,unload,[list_to_atom(AppId)]),
    rpc:call(Slave,os,cmd,["rm -rf "++DestDir]),
    AppFile=AppId++".app",
    {error,{"no such file or directory",AppFile}}=rpc:call(Slave,application,start,[list_to_atom(AppId)]),
    ok.