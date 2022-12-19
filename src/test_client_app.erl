-module(test_client_app).


-behaviour(application).


-export([start/2]).
-export([stop/1]).


-spec start(_Type , _Args) -> {ok , pid()}.
start(_Type , _Args) ->
	{ok , Host} = application:get_env(test_client , host),
	{ok , Port} = application:get_env(test_client , port),
	{ok , Monitor} = application:get_env(test_client , monitor),
	ok = test_client_logger:init(),
	test_client_sup:start_link(Host , Port , Monitor).


-spec stop(_) -> ok.
stop(_) ->
	ok.




