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
-module(sd_test). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("test_src/compute_test.hrl").
%% --------------------------------------------------------------------
%-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-define(WAIT_FOR_TABLES,10000).	  
-define(MaxRandNum,5).
%% --------------------------------------------------------------------
-export([test/0,
	sd/1]).

%% ====================================================================
%% External functions
%% ====================================================================

test()->
    sd:start(),
    t1(),
   % t2(),

    ok.
t1()-> 
   App=common,
  %  R1=sd(App),
  %  time_test(10),
  %  io:format("R1 ~p~n",[R1]),
%  R1=['a_board1@joq62-X550CA','b_board2@joq62-X550CA','a_board2@joq62-X550CA',
%	'a_board3@joq62-X550CA'],

    rpc:call('a_board2@joq62-X550CA',application,stop,[App]),
    R2=sd:get(App),
 %   R2= ['a_board1@joq62-X550CA',
%	 'b_board2@joq62-X550CA',
%	 'a_board3@joq62-X550CA'],
    io:format("R2 ~p~n",[R2]),
   % R3=sd(all),
   % io:format("All = ~p~n",[R3]),
   ok.


t2()->
    {StartApps,StopApps}=check(),
     io:format("StartApps = ~p~n",[StartApps]),
     io:format("StopApps = ~p~n",[StopApps]),
    ok.


check()->
    % git clone deployment.config [{Node,app}]
    {ok,Deployment}=file:consult("deployment.config"),
    {ok,Catalog}=file:consult("apps.catalog"),
   % io:format("Deployment = ~p~n",[Deployment]),
 
    AllRunningApps=sd:get(all),  % All=[{Node,[{app,"app_info",Vsn}]
    check(AllRunningApps,Catalog,Deployment,[],[]).

check([],_,_,StartApps,StopApps)->
    {StartApps,StopApps};
check([{Node,AppList}|T],Catalog,Deployment,StartApps,StopApps)->
   % io:format("Node,AppList = ~p~n",[{Node,AppList}]),
  %  io:format("Deployment = ~p~n",[Deployment]),
    FilteredAppListOnNode=[{Node,App}||{App,_,_}<-AppList,
				       true==lists:keymember(App,1,Catalog)],

   % io:format("FilteredAppListOnNode = ~p~n",[FilteredAppListOnNode]),
    AppsToDeployOnNode=[{XNode,XApp}||{XNode,XApp}<-Deployment,
		     XNode==Node],
   % io:format("AppsToDeployOnNode = ~p~n",[AppsToDeployOnNode]),
    Start=[{ZNode,ZApp}||{ZNode,ZApp}<-AppsToDeployOnNode,
			 false==lists:member({ZNode,ZApp},FilteredAppListOnNode)],

    Stop=[{ZNode,ZApp}||{ZNode,ZApp}<-FilteredAppListOnNode,
			 false==lists:member({ZNode,ZApp},AppsToDeployOnNode)],
    
   % Stop=[{YNode,YApp}||{YNode,YApp}<-Deployment,
%				 false==lists:member({YNode,YApp},FilteredAppList)],
 %   Stop=[],
 %   io:format("Start = ~p~n",[Start]),
%    io:format("StopApps = ~p~n",[StopApps]),
    check(T,Catalog,Deployment,
	  lists:append(Start,StartApps),
	  lists:append(Stop,StopApps)).
				       
    
    
time_test(0)->
    ok;
time_test(N)->
    {T,_}=timer:tc(sd_test,sd,[common]),
    io:format("sd(common) exec time ~p~n",[T]),
    {T2,_}=timer:tc(rpc,call,['a_board1@joq62-X550CA',common,ping,[]]),
    io:format("common:ping exec time ~p~n",[T2]),
    time_test(N-1).

sd(all)->
    All=[{Node,rpc:call(Node,application,which_applications,[])}||Node<-nodes()],   
    All;
sd(App)-> % takes about 2,5 ms -> store in mnesia or each app keeps their needed apps in State?
    All=[{Node,rpc:call(Node,application,which_applications,[])}||Node<-nodes()],
    [Node||{Node,AppList}<-All,
	   true==lists:keymember(App,1,AppList)].

node_apps(Node)->
    rpc:call(Node,application,which_applications,[]).
    
