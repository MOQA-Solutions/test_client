-module(test_client_sup).


-export([start_link/3]).
-export([init/1]).


-spec start_link(atom() | string() , pos_integer() , boolean()) -> {ok , pid()}.
start_link(Host , Port , Monitor) ->
	supervisor:start_link(?MODULE , [Host , Port , Monitor]).


init([Host , Port , Monitor]) ->
	SupFlags =#{
		strategy => one_for_one,
		intensity => 1,
		period => 5
		},

	ChildSpecs =#{
		id => testclientsupervisor,
		start => {test_client_supervisor , start_link , [self() , Host , Port , Monitor]},
		restart => permanent,
		shutdown => brutal_kill,
		type => supervisor,
		modules => [test_client_supervisor]
		},

	Children = [ChildSpecs],

	{ok , {SupFlags , Children}}.



