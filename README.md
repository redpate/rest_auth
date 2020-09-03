rest_auth
=====

rest_auth application

Startup
-----

    $ rebar3 shell
    
or build release

Configure
-----
All configuration done with env.config file

    [{rest_auth, [
    {mysql, [
        {host, "localhost"},
        {user, "root"},
        {password, "foo"},
        {database, "auth"},
        {table, <<"userstable">>} 
    ]},
    {cowboy_dispatch, 
        [
            {'_', [
                {"/user/[:action]", rest_auth, []}
            ]}
        ]
    },
    {cowboy_opt, [{port, 8080}]}
    ]}].