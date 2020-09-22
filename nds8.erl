-module(nds8). 							%The Current Module (Core Module)
-import(index, [index/1]).				%Import of the Index Module Provided
-export([peer/2, peerUser/2, setup/0]). %Exporting Primary Functions Only

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The peer function creates a peerUser and peerServer that interact
% with one another in order to fill its own catalog with the
% items contained by other peers that have been spawned.

peer(Index, DB) ->
    %% Starts the 'user' part of the peer
    spawn(?MODULE, peerUser, [Index, self()]),
    %% Starts the 'file server' part of the peer
    peerServer(DB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

-spec lookup(K, list({K, V})) -> none | {some, V}.

% The lookup function takes all items in a database and an ID and checks
% to see if the ID is contained within the database. It returns content
% associated with the ID if it is contained and returns none if it is not.

lookup(Id, [DB|DBs]) ->
    case DB of 
        {Id, V} -> {some, V};
        {_, _} -> lookup(Id, DBs)
    end;
lookup(Id, DB) ->
	case DB of 
        {Id, V} -> {some, V};
        {_, _} -> none
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
% The peerServer function waits for an interaction from another function
% and responds based on that task that was asked of it. Priority
% is given to tasks that add to the database rather than tasks
% that are only requests. The possible tasks are as follows:
% 		add 	- adds the ID (Id) and Content (C) to the server's database.
% 		request	- checks the ID (Id) against the server's database
%				  and if found, returns the ID and the Content (V) 
%				  and if notfound, returns only notfound
	
peerServer(DB) ->
	receive 
        {add,Id,Content} -> peerServer([{Id, Content}|DB])
		after 0 ->
			receive 
				{add, Id, C} -> 
					peerServer([{Id, C}|DB]);
				{request, Peer, Id} -> 
					L = lookup(Id, DB),
					case L of 
						{some, V} -> 
							Peer!{found, Id, V}, peerServer(DB);
						none -> 
							Peer!notfound, peerServer(DB)
					end,
					peerServer(DB)
			end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The checkOwnership function checks to see if a peer is in another list of peers.
% It returns true if the peer is within the list and false if it is not.
	
checkOwnership(_, []) -> false;
checkOwnership(Peer,[Peer|_])-> true;
checkOwnership(Peer, [_|Ps]) -> checkOwnership(Peer, Ps).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The peerUser Function waits 2 seconds and sends an interaction to index
% to attempt to receive the catalog (Cat) of all items. If no catalog is 
% recieved then the function restarts. Upon gaining the catalog, it will
% select a random item (Id) from the catalog along with who (Owner1|Owner2) 
% that item. It will check if it owns the item itself and if so will restart.
% If it does not have the item however, it will add the item to the
% peerServer's database, tell the index that it owns the item and will
% once again restart.

peerUser(Index, PeerServer) ->
	timer:sleep(2000),
	Index!{getCatalog, self()},
	receive
		{ok, []} ->
			peerUser(Index, PeerServer);
		{ok, Cat} -> 
			{Id, [Owner1|Owner2]} = lists:nth(rand:uniform(length(Cat)), Cat),
			case checkOwnership(PeerServer, [Owner1|Owner2]) of
			false ->
				Owner1!{request, self(), Id},
				receive
					{found,Id,C} -> 
						PeerServer!{add,Id,C}, 
						Index!{addOwner,Id,PeerServer},
						peerUser(Index, PeerServer)
				end;
			true -> peerUser(Index, PeerServer)
			end
	end,
	peerUser(Index, PeerServer).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
% The setup function spawns an index (IndexID) and 3 test peers (PeerA|PeerB|PeerC) 
% along with items they hold. These items and who owns them are then listed in the
% index.
  
setup() ->
	IndexID = spawn(index,index,[[]]),
    PeerA = spawn(?MODULE,peer,[IndexID, {1000,"ItemA"}]),
    PeerB = spawn(?MODULE,peer,[IndexID,{1001,"ItemB"}]),
    PeerC = spawn(?MODULE,peer,[IndexID, {1002,"ItemC"}]),
    IndexID!{addOwner,1000,PeerA},
    IndexID!{addOwner,1001,PeerB},
    IndexID!{addOwner,1002,PeerC}.