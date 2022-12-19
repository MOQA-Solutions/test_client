-module(test_client_worker).


-export([start_link/3]).
-export([init/1]).

-export([callback_mode/0]).

-export([not_connected/3]).
-export([connected/3]).
-export([terminate/3]).


-include("../include/test_client_worker_data.hrl").
-include("../include/test_client_timers.hrl").


-spec start_link(pid() , atom() | string() , pos_integer()) -> {ok , pid()}.
start_link(Parent , Host , Port) ->
	gen_statem:start_link(?MODULE , [Parent , Host , Port] , []).


init([Parent , Host , Port]) ->
	StringHost = case is_list(Host) of
			true ->
				Host;
			_ ->
				atom_to_list(Host)
		     end,
	StringPort = integer_to_list(Port),
	ConnectionData = {Parent , Host , Port}, 
	Data =#data{
			connection_data = ConnectionData,
			host = StringHost,
			port = StringPort
		},
	erlang:send(self() , start),
	{ok , not_connected , Data}.


callback_mode() ->
	state_functions.


%%================================================================================================%%
%%				gen_statem callback functions					  %%
%%================================================================================================%%

not_connected(info , start , Data =#data{
						connection_data = {Parent , Host , Port}
					}
		) ->
	case gen_tcp:connect(Host , Port , ?SOCKOPTS) of
		{ok , Socket} ->
			erlang:send(Parent , {self() , connected}),
			KeepAliveTimer = erlang:start_timer(?KEEP_ALIVE_TIMEOUT , self() , 'keep alive timeout'),
			DataTimer = erlang:start_timer(?DATA_TIMEOUT , self() , 'data timeout'),
			NewData = Data#data{
					socket = Socket,
					keep_alive_timer = KeepAliveTimer,
					data_timer = DataTimer
					},
			{next_state , connected , NewData};
		{error , Reason} ->
			{stop , Reason}
	end;


not_connected(info , _Info , Data) ->
	{keep_state , Data};


not_connected(cast , _Cast , Data) ->
	{keep_state , Data};


not_connected({call , _From} , _Call , Data) ->
	{keep_state , Data}.

%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++%%

connected(info , {timeout , KeepAliveTimer , 'keep alive timeout'},
	#data{
		keep_alive_timer = KeepAliveTimer
	}) ->
	{stop , 'keep alive timeout'};


connected(info , {timeout , DataTimer , 'data timeout'},
	Data =#data{
		host = Host,
		port = Port,
		data_timer = DataTimer
		}) -> 
	logger:debug(#{
			'1)Type' => 'STATE LOG',
			'2)Reason' => 'no received data for one minute',
			'3)Peer' => Host ++ ":" ++ Port,
			'4)Data' => null
		      }),
	NewDataTimer = erlang:start_timer(?DATA_TIMEOUT , self() , 'data timeout'),
	NewData = Data#data{
			data_timer = NewDataTimer
			},
	{keep_state , NewData};
 

connected(info , {tcp_closed , Socket}, 
	#data{
		socket = Socket
		}) ->
	{stop , 'connection closed from the server'};


connected(info , {tcp , Socket , BinData},
	Data =#data{
		host = Host,
		port = Port,
		socket = Socket,
		keep_alive_timer = KeepAliveTimer,
		data_timer = DataTimer
		}) ->
	_ = erlang:cancel_timer(KeepAliveTimer),
	_ = erlang:cancel_timer(DataTimer),
	logger:debug(#{
			'1)Type' => 'STATE LOG',
			'2)Reason' => 'received data',
			'3)Peer' => Host ++ ":" ++ Port,
			'4)Data' => BinData
		      }),
	NewKeepAliveTimer = erlang:start_timer(?KEEP_ALIVE_TIMEOUT , self() , 'keep alive timeout'),
	NewDataTimer = erlang:start_timer(?DATA_TIMEOUT , self() , 'data timeout'),
	NewData = Data#data{
			keep_alive_timer = NewKeepAliveTimer,
			data_timer = NewDataTimer
			},
	{keep_state , NewData};


connected(info , _Info , Data) ->
	{keep_state , Data};


connected(cast , _Cast , Data) ->
	{keep_state , Data};


connected({call , _From} , _Call , Data) ->
	{keep_state , Data}.

%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++%%

terminate(Reason , _State , _Data) ->
	{terminate , Reason}.

%%============================================================================================%%
%%			end of gen_statem callback functions				      %%
%%============================================================================================%%

		
