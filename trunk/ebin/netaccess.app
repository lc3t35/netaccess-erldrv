{application, netaccess,
	[{description, "Netaccess Driver"},
		{vsn, "1.2"},
		{modules, [netaccess, netaccess_server, netaccess_fsm, pridrv, iisdn]},
		{registered, []},
		{applications, [kernel, stdlib]},
		{env, []}]}.
