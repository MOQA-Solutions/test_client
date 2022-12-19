-record (data , {

	connection_data :: {pid() , atom() | string() , pos_integer()},
	
	host :: string(),
	
	port :: string(),
	
	socket :: port() | undefined,

	keep_alive_timer :: reference() | undefined,

	data_timer :: reference() | undefined

	}

).

-define(SOCKOPTS , [
		
			binary, 

			{active , true},

			{packet , 4}

		]

).
 
