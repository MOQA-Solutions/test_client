-module(test_client_logger).


-export([init/0]).


-spec init() -> ok.
init() ->
	logger:set_primary_config(level , debug),
	logger:set_handler_config(default , level , notice),
	Config1 = #{
		config => #{
			file => "logging/info_logging.log",
			max_no_bytes => 10 * 1024 * 1024,
			max_no_files => 10,
			sync_mode_qlen => 100,
			drop_mode_qlen => 1000,
			flush_qlen => 2000
			},
		level => info,
		filters => [
				{ info_logs , {fun logger_filters:level/2 , {stop , neq , info}} }
			]
		},

	Config2 = #{
		config => #{
			file => "logging/debug_state_logging.log",
			max_no_bytes => 10 * 1024 * 1024,
			max_no_files => 10,
			sync_mode_qlen => 100,
			drop_mode_qlen => 1000,
			flush_qlen => 2000
			},
		level => debug,
		filters => [
				{ debug_logs , {fun logger_filters:level/2 , {stop , neq , debug}} },
				{ state_logs , {fun state_logs/2 , []} }
			],
		formatter => {
				logger_formatter , #{
							legacy_header => true,
							single_line => false
						}
			}
		},

	Config3 = #{
		config => #{
			file => "logging/debug_monitor_logging.log",
			max_no_bytes => 10 * 1024 * 1024,
			max_no_files => 10,
			sync_mode_qlen => 100,
			drop_mode_qlen => 1000,
			flush_qlen => 2000
			},
		level => debug,
		filters => [
				{ debug_logs , {fun logger_filters:level/2 , {stop , neq , debug}} },
				{ monitor_logs , {fun monitor_logs/2 , []} }
			],
		formatter => {
				logger_formatter , #{
							legacy_header => true,
							single_line => false
						}
			}
		},

	logger:add_handler(info_logging_handler , logger_std_h , Config1),
	logger:add_handler(debug_state_logging_handler , logger_std_h , Config2),
	logger:add_handler(debug_monitor_logging_handler , logger_std_h , Config3),
	ok.


-spec state_logs(map() , []) -> map() | stop.
state_logs( #{msg := {report , Report}} = Log , []) ->
	case maps:get('1)Type' , Report , undefined) of
		'STATE LOG' ->
			Log;
		_ ->
			stop
	end.


-spec monitor_logs(map() , []) -> map() | stop.
monitor_logs( #{msg := {report , Report}} = Log , []) ->
	case maps:get('1)Type' , Report , undefined) of
		'MONITOR LOG' ->
			Log;
		_ ->
			stop
	end.



