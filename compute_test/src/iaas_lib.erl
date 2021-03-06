%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(iaas_lib).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(Catalog,"iaas.catalog").

%% External exports
-export([get_catalog/0
	 
	]).

%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


get_catalog()->
    {ok,Catalog}=file:consult(?Catalog),
    {ok,Catalog}.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
% Catalog [{boardNode,[{slaveNodes,ErlCmd}]}
start_stop_nodes()->
    {ok,Catalog}=file:consult(?Catalog),
    RunningBoardNodes=[{BoardNode,SlaveList}||{BoardNode,SlaveList}<-Catalog,
				  pong==net_adm:ping(BoardNode)],
    MissingBoardNodes=[BoardNode||{BoardNode,_}<-Catalog,
				  false==lists:member(BoardNode,RunningBoardNodes)],
    AllRunningNodes=nodes(),
    {StartResult,StopResult}=slaves_to_start_stop([RunningBoardNodes,AllRunningNodes,[],[]),
    
    ok.


slaves_to_start_stop([{RunningBoardNode,Slaves}|T],AllRunningNodes,AccBoards,AccSlaves)->
    StartSlaves=[{RunningBoardNode,string:tokens(atom_to_list(SlaveNode),"@"),ERlCmd}||{SlaveNode,ERlCmd}<-Slaves,
						     nodes pang==net_adm:ping(SlaveNode)],

    StopSlaves=[SlaveNode||Node<-AllRunningNodes,
			   false==lists:keymember(Node,1,Slaves)],
    StartSlavesResult=[rpc:call(RunningBoardNode,slave,start,[Host,SlaveNodeId,ErlCmd])||
			  {RunningBoardNode,[SlaveNodeId,Host],ERlCmd}<-StartSlaves],

    rpc
    
    
