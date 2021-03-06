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
-module(compute_unit_test). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("test_src/db_passwd.hrl").
-include("test_src/db_shop.hrl").
-include("src/db_lock.hrl").
-include("test_src/compute_test.hrl").
%% --------------------------------------------------------------------
%%-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-define(WAIT_FOR_TABLES,10000).	  
-define(MaxRandNum,5).
%% --------------------------------------------------------------------
-export([test/0,
	 node_name/1,
	 monkey/0,
	 a_monkey/0,
	 a_sysinfo/0,
	 test_second_blood/0,
	 host/0
	%,
	% b_sysinfo/0,
	% c_sysinfo/0,
	% a_kill/0,
	% b_kill/0,
	% c_kill/0,
	% a_boot/0,
	% b_boot/0,
	% c_boot/0

	]).

%% ====================================================================
%% External functions
%% ====================================================================
node_name(Name)->
    {ok,Host}=inet:gethostname(),
    Node=list_to_atom(Name++"@"++Host),    
    Node.
host()->
    {ok,Host}=inet:gethostname(),
    Host.

vm_id(Vm)->
    [VmId,Host]=string:tokens(atom_to_list(Vm),"@"),
    VmId.

a_sysinfo()->
    {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    rpc:call(NodeA,mnesia,system_info,[]).

%%- Negative testing
a_monkey()->
    {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    rpc:call(NodeA,?MODULE,monkey,[]).
monkey()->
    {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    NodeB=list_to_atom("b@"++Host),
    NodeC=list_to_atom("c@"++Host),
    WorkerNodes=[NodeA,NodeB,NodeC],
    io:format("~n"),
    io:format("~p **************Start New session *****************~n",[time()]),
 %   io:format("~n"),
    Ping=[net_adm:ping(Node)||Node<-WorkerNodes],
    [pong,pong,pong]=Ping,
  %  io:format("Ping all nodes= ~p~n",[Ping]),

  %  io:format(" Check All workers that should be runnng *****************~n"),
  %  io:format("~n"),
    check_1(WorkerNodes,[no_node]),
  %  check_1(WorkerNodes,no_killed),
 
 %   io:format("AllNodes are running ~p~n",[{time(),":",?MODULE,?FUNCTION_NAME,?LINE}]),
 %   io:format("Node A = ~p~n",[{time(),":",rpc:call(NodeA,db_passwd,read,["Joq"])}]),
 %   io:format("Node B = ~p~n",[{time(),":",rpc:call(NodeB,db_passwd,read,["Joq"])}]),
 %   io:format("Node C = ~p~n",[{time(),":",rpc:call(NodeC,db_passwd,read,["Joq"])}]),
    timer:sleep(1000),
   % io:format("~n"),

    %% Kill one or two nodes 1=non, 2=NodeA,3=NodeB,4=NodeC, 5=non
    FirstBlood=rand:uniform(?MaxRandNum),    
    {FirstKilledNode,FirstKilledNodeId}=kill_node(FirstBlood),
  %  io:format("First blood ~p~n",[{{FirstKilledNode,FirstKilledNodeId},?MODULE,?FUNCTION_NAME,?LINE}]),

    SecondBlood=second_blood(FirstBlood,rand:uniform(?MaxRandNum)),
    {SecondKilledNode,SecondKilledNodeId}=kill_node(SecondBlood),
  %  io:format("Second blood ~p~n",[{{SecondKilledNode,SecondKilledNodeId},?MODULE,?FUNCTION_NAME,?LINE}]),
   
    io:format(" Killed Workers = ~p~n", [{FirstKilledNode,SecondKilledNode}]),
  %  io:format("~n"),
    check_1(WorkerNodes,[FirstKilledNode,SecondKilledNode]),
    %%check_1(WorkerNodes,SecondKilledNode),
    
    
  %  io:format("NodeB not running ~p~n",[{time(),":",?MODULE,?FUNCTION_NAME,?LINE}]),
  %  io:format("Node A = ~p~n",[{time(),":",rpc:call(NodeA,db_passwd,read,["Joq"])}]),
  %  io:format("Node B = ~p~n",[{time(),":",rpc:call(NodeB,db_passwd,read,["Joq"])}]),
  %  io:format("Node C = ~p~n",[{time(),":",rpc:call(NodeC,db_passwd,read,["Joq"])}]),

    %% Start killed nodes 
  %  io:format(" Start killed workers FirstKilledNodeId,SecondKilledNodeId = ~p~n", [{FirstKilledNodeId,SecondKilledNodeId}]),
  %  io:format("~n"),
    start_killed_node([FirstKilledNodeId,SecondKilledNodeId]),
    
   % {ok,NodeB}=slave:start(Host,b,"-pa ebin -pa test_ebin -pa gen_mnesia/ebin -setcookie abc -compute config_file nodes -run compute boot "),
    
    
  %  io:format("End session wait ********************************************~n"),
  %  io:format("~n"),
    timer:sleep(15000),
    monkey().

check_1([],_KilledNode)->
    ok;
check_1([Node|T],KilledNodes)->
  %  io:format("check_1 Node,KilledNodes = ~p~n",[{Node,KilledNodes,?MODULE,?FUNCTION_NAME,?LINE}]),
    case lists:member(Node,KilledNodes) of
	true->	   
	    {badrpc,_Err}=rpc:call(Node,db_passwd,read,["Joq"]);
%	    io:format("True Killed Node = ~p~n",[{Node,{badrpc,Err},?MODULE,?FUNCTION_NAME,?LINE}]);
	false->
	    case rpc:call(Node,db_passwd,read,["Joq"]) of
		[{"Joq",joq1}]->
		    ok;
		Err->
		    io:format("Error in running node = ~p~n",[{Node,Err,?MODULE,?FUNCTION_NAME,?LINE}])
	    end
    end,
    check_1(T,KilledNodes).


start_killed_node([])->
    ok;
start_killed_node([NodeId|T])->
 %   io:format("start_killed_node ~p~n",[{NodeId,?MODULE,?FUNCTION_NAME,?LINE}]),
    start_node(NodeId),
    start_killed_node(T).

start_node(no_id)->
  %  io:format("start_node ~p~n",[{no_id,?MODULE,?FUNCTION_NAME,?LINE}]),
    ok;
start_node(NodeId)->
  %  io:format("start_node ~p~n",[{NodeId,?MODULE,?FUNCTION_NAME,?LINE}]),
    {ok,Host}=inet:gethostname(),
    {ok,_Node}=slave:start(Host,NodeId,"-pa ebin -pa test_ebin -pa gen_mnesia/ebin -setcookie abc -compute config_file nodes -run compute boot "),
  %  io:format("Started Node = ~p~n",[{Node,?MODULE,?FUNCTION_NAME,?LINE}]),
    ok.
    
kill_node(Num)->
    {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    NodeB=list_to_atom("b@"++Host),
    NodeC=list_to_atom("c@"++Host),
    {KilledNode,NodeId}=case Num of
			    2->
				slave:stop(NodeA),
				{NodeA,a};
			    3->
				slave:stop(NodeB),
				{NodeB,b};
			    4->
				slave:stop(NodeC),
				{NodeC,c};
			    _->
				{no_node,no_id}
			end,
    {KilledNode,NodeId}.
    
	    
	    

test_second_blood()->
    FirstBlood=rand:uniform(?MaxRandNum),    
    SecondBlood=second_blood(FirstBlood,rand:uniform(?MaxRandNum)),
    io:format("~p~n",[{FirstBlood, SecondBlood,?MODULE,?FUNCTION_NAME,?LINE}]),
    timer:sleep(1000),
    test_second_blood().

second_blood(FirstBlood,FirstBlood)->
 %   io:format("~p~n",[{FirstBlood,?MODULE,?FUNCTION_NAME,?LINE}]),
    second_blood(FirstBlood,rand:uniform(?MaxRandNum));
second_blood(_FirstBlood,SecondBlood)->
    SecondBlood.
   
%% --------------------------------------------------------------------
%% 
%% 
%% --------------------------------------------------------------------
test()->
    io:format("Start test ~p~n",[?MODULE]),
    
    io:format("defines_test() ~n"),
    ok=defines_test(),
    io:format("clean_start_test() ~n"),
    ok=clean_start_test(),
    io:format("connect_boards_test() ~n"),
    ok=connect_boards_test(),
    io:format("start_slaves_test() ~n"),
    ok=start_slaves_test(),
    
     io:format("Successfully Stop test ~p~n",[?MODULE]),
    
    ok.
% Test defines
defines_test()->
    ['board1@joq62-X550CA',
     'board2@joq62-X550CA',
     'board3@joq62-X550CA']=[?B1,?B2,?B3],
    ['a_board1@joq62-X550CA',
     'b_board1@joq62-X550CA',
     'c_board1@joq62-X550CA']=[?A_B1,?B_B1,?C_B1],
    ['a_board2@joq62-X550CA',
     'b_board2@joq62-X550CA',
     'c_board2@joq62-X550CA']=[?A_B2,?B_B2,?C_B2],
    ['a_board3@joq62-X550CA',
     'b_board3@joq62-X550CA',
     'c_board3@joq62-X550CA']=[?A_B3,?B_B3,?C_B3],
    
    ok.
clean_start_test()->
   
    [rpc:call(Node,init,stop,[])||Node<-[?A_B1,?B_B1,?C_B1,
					 ?A_B2,?B_B2,?C_B2,
					 ?A_B3,?B_B3,?C_B3]],

    [rpc:call(?B1,slave,stop,[Node])||Node<-[?A_B1,
					     ?B_B1,
					     ?C_B1]],
    [rpc:call(?B2,slave,stop,[Node])||Node<-[?A_B2,
					     ?B_B2,
					     ?C_B2]],
    [rpc:call(?B3,slave,stop,[Node])||Node<-[?A_B3,
					     ?B_B3,
					     ?C_B3]],    
{ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    NodeB=list_to_atom("b@"++Host),
    NodeC=list_to_atom("c@"++Host),
    rpc:call(NodeA,init,stop,[]),
    rpc:call(NodeB,init,stop,[]),
    rpc:call(NodeC,init,stop,[]),
    timer:sleep(500),
    ok.


%% Connect to board nodes
connect_boards_test()->
    [pong,pong,pong]=[net_adm:ping(Node)||Node<-[?B1,?B2,?B3]],
    ok.
		 
%% Start slaves, a1,b1,c1,a2 ..
start_slaves_test()->
    ok=start_system_test:test(),
    ok=start_stop_apps_test:test(),
    ok=sd_test:test(),
    ok=orch_test:test(),
    ok.
    

