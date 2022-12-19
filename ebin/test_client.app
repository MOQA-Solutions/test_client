{application , 'test_client' , 

 [

	{description , "coding challenge client side"},

	{vsn , "0.1"},

	{modules , ['test_client_app' , 'test_client_sup' , 'test_client_logger' , 

		    'test_client_supervisor' , 'test_client_worker'

		   ]},

	{applications , [kernel , stdlib]},

	{mod , {test_client_app , []}},

	{env , [

		{host , "localhost"},

		{port , 10},

		{monitor , true}

	       ]

	}

 ]

}.

