-record(state , {

	parent :: pid(),

	monitor :: boolean(),

	child :: pid(),

	child_data :: {pid() , atom() | string() , pos_integer()},

	host :: string(),

	port :: string(),

	spam :: on | off,

	spam_timer :: reference()

	}

).



