-module(test_client_supervisor).

	%%  This is a customized supervisor which will control the restarts rate of the gen_statem worker


-export([start_link/4]).
-export([init/1]).

-export([handle_info/2]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([code_change/3]).
-export([terminate/2]).


-include("../include/test_client_supervisor_data.hrl").
-include("../include/test_client_timers.hrl").


-spec start_link(pid() , atom() | string() , pos_integer() , boolean()) -> {ok , pid()}.
start_link(Parent , Host , Port , Monitor) ->
	gen_server:start_link(?MODULE , [Parent , Host , Port , Monitor] , []).


init([Parent , Host , Port , Monitor]) ->
	process_flag(trap_exit , true),
	ChildData = {self() , Host , Port},
	{ok , Child , SpamTimer} = start_worker(ChildData),
	StringHost = case is_list(Host) of
			true ->
				Host;
			_ ->
				atom_to_list(Host)
		     end,
	StringPort = integer_to_list(Port),
	State =#state{
			parent = Parent,
			monitor = Monitor,
			child = Child,
			child_data = ChildData,
			host = StringHost,
			port = StringPort,
			spam = on,
			spam_timer = SpamTimer
		},
	{ok , State}.

%%==================================================================================================%%
%%				gen_server callback functions					    %%
%%==================================================================================================%%

handle_info({'EXIT' , Parent , Reason},
	State =#state{
			parent = Parent
		}) ->
	{stop , {'parent exit' , Reason} , State};


handle_info({timeout , SpamTimer , 'spam timeout'},
	State =#state{
			spam_timer = SpamTimer
		}) ->
	NewState = State#state{
				spam = off,
				spam_timer = undefined
			},
	{noreply , NewState};


handle_info({'EXIT' , Child , Reason},
	State =#state{
			child = Child,
			child_data = ChildData,
			host = Host,
			port = Port,
			spam = Spam,
			spam_timer = SpamTimer
		}) ->
	logger:debug(#{
			'1)Type' => 'STATE LOG',
			'2)Reason' => {'connection failed' , Reason},
			'3)Peer' => Host ++ ":" ++ Port,
			'4)Data' => null
		      }),
	case Spam of
		on ->
			receive
				{timeout , SpamTimer , 'spam timeout'} ->
					ok
			end;
		_ ->
			ok
	end,
	{ok , NewChild , NewSpamTimer} = start_worker(ChildData),
	NewState = State#state{
				child = NewChild,
				spam = on,
				spam_timer = NewSpamTimer
			},
	{noreply , NewState};


handle_info({Child , connected} , State =#state{
						monitor = true,
						child = Child,
						child_data = {_Self , Host , Port}
						}
		) ->
	logger:debug(#{
			'1)Type' => 'MONITOR LOG',
			'2)Reason' => 'new connection after fail',
			'3)Peer' => Host ++ ":" ++ Port,
			'4)Data' => null
		      }),	      
	{noreply , State};
						

handle_info(_Info , State) ->
	{noreply , State}.

%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++%%

handle_cast(_Cast , State) ->
	{noreply , State}.

%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++%%

handle_call(_Call , _From , State) ->
	{noreply , State}.

%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++%%

code_change(_OldVsn , State , _) ->
	{ok , State}.

%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++%%

terminate(Reason , State) ->
	{terminate , Reason , State}.

%%=================================================================================================%%
%%				end of gen_server callback functions				   %%
%%=================================================================================================%%

-spec start_worker({pid() , atom() | string() , pos_integer()}) -> {ok , pid() , reference()}.
start_worker({Parent , Host , Port}) ->
	{ok , Pid} = test_client_worker:start_link(Parent , Host , Port),
	SpamTimer = erlang:start_timer(?SPAM_TIMEOUT , self() , 'spam timeout'),
	{ok , Pid , SpamTimer}.


 
