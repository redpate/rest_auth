%%%-------------------------------------------------------------------
%%% File    : rest_auth.erl
%%% Author  : redpate
%%% Description : 
%%% @doc
%%% Rest handler for cowboy
%%% @end
%%% Created : 09/03/2020
%%%-------------------------------------------------------------------
-module(rest_auth).

%% cowboy defined callbacks
-export([init/2, allowed_methods/2, content_types_provided/2, content_types_accepted/2]).

-export([main/2, start_mysql/1, get_table_name/0, get_user_list/0, add_user/2, dirty_auth_user/2, dirty_update_user/3, init_table/2, q/2]).

%%base content provided
-define(CONTENT_PROVIDED, [
    {{<<"application">>, <<"json">>, '*'}, main}
]).

-type str() :: list() | binary().

%%====================================================================
%% Cowboy_rest callbacks
%%====================================================================
%%--------------------------------------------------------------------
%%% @doc
%%% Init handler. Sets Req binding 'action' as state.
%%% @end
%%--------------------------------------------------------------------
init(Req, _Opts) ->
	{cowboy_rest, Req, cowboy_req:binding(action, Req, <<>>)}.


%%--------------------------------------------------------------------
%%% @doc
%%% Describes alowed method for each state
%%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(map(), term())-> {list(), map(), binary()}.
allowed_methods(Req, <<"auth">>) ->
	{[<<"POST">>], Req, <<"auth">>};
allowed_methods(Req, <<"list">>) ->
	{[<<"GET">>], Req, <<"list">>};
allowed_methods(Req, <<"new">>) ->
	{[<<"POST">>], Req, <<"new">>};
allowed_methods(Req, <<"update">>) ->
	{[<<"POST">>,<<"PUT">>], Req, <<"update">>};
allowed_methods(Req, _) ->
	{[], Req, <<>>}.

%%--------------------------------------------------------------------
%%% @doc
%%% ct provided cowboy callback
%%% @end
%%--------------------------------------------------------------------
content_types_provided(Req, State) ->
	{?CONTENT_PROVIDED, Req, State}.
%%--------------------------------------------------------------------
%%% @doc
%%% ct accepted cowboy callback
%%% @end
%%--------------------------------------------------------------------
content_types_accepted(Req, State) ->
	{?CONTENT_PROVIDED, Req, State}.

-spec main(map(), binary())-> {list(), map(), binary()}.
%%--------------------------------------------------------------------
%%% @doc
%%% Main request handler, also works as router matching passed state
%%% @end
%%--------------------------------------------------------------------
%% authorization callback. actually combined with authentication because no sessions has been implemented.
main(Req, <<"auth">>) ->
	{ok, _Data, _} = cowboy_req:read_body(Req),
    case jsone:decode(_Data) of
        #{<<"username">> :=  Name, <<"password">> :=  Password}->
            Body = case dirty_auth_user(Name, Password) of
                true->
                    %% base authorization - roles are hardcoded as empty list. role managment was not requested
                    #{<<"status">> => <<"ok">>, <<"name">> => Name, <<"roles">> => []}; 
                false->
                    #{<<"status">> => <<"error">>, <<"reason">> => <<"invalid username or password">>}
            end,
            Req2 = cowboy_req:set_resp_body(jsone:encode(Body), Req),
	        {true, Req2, <<"new">>};
        _-> %% wrong request. drop it
            {stop, Req, <<"new">>}
    end;
%% all users list callback.
main(Req, <<"list">>) ->
	{jsone:encode(get_user_list()), Req, <<"list">>};  %% no cache or pagination requested
%% new users creation callback.
main(Req, <<"new">>) ->
     error_logger:error_msg("new ", [Req]),
    {ok, _Data, _} = cowboy_req:read_body(Req),
    case jsone:decode(_Data) of
        #{<<"username">> :=  Name, <<"password">> :=  Password}->
            Body = case add_user(Name, Password) of
                ok->
                    #{<<"status">> => <<"ok">>, <<"name">> => Name};
                {error, already_exist}->
                    #{<<"status">> => <<"error">>, <<"reason">> => <<"User already exist">>}
            end,
            Req2 = cowboy_req:set_resp_body(jsone:encode(Body), Req),
	        {true, Req2, <<"new">>};
        _-> %% wrong request. drop it
            {stop, Req, <<"new">>}
    end;
%% change user password callback.
main(Req, <<"update">>) ->
	{ok, _Data, _} = cowboy_req:read_body(Req),
    case jsone:decode(_Data) of
        #{<<"username">> :=  Name, <<"password">> :=  Password, <<"new_password">> :=  NewPassword}->
            Body = case dirty_update_user(Name, Password, NewPassword) of
                true->
                    #{<<"status">> => <<"ok">>, <<"name">> => Name}; 
                false->
                    #{<<"status">> => <<"error">>, <<"reason">> => <<"invalid username or password">>}
            end,
            Req2 = cowboy_req:set_resp_body(jsone:encode(Body), Req),
	        {true, Req2, <<"new">>};
        _-> %% wrong request. drop it
            {stop, Req, <<"new">>}
    end;
%% error handling for unknown request passed to 'main/2'.
main(Req, State) ->
    error_logger:error_msg("main rest_auth hanled was called with badarg: main(~p, ~p)", [Req, State]),
	{stop, Req, State}.

%%====================================================================
%% Database functions
%%====================================================================
%%--------------------------------------------------------------------
%%% @doc
%%% Basic query to mysql database.
%%% @end
%%--------------------------------------------------------------------
-spec q(list() | binary(), [term()])-> tuple() | no_return().
q(Format, Data)->
    case rest_auth_sup:get_child_pid(mysql) of
        false -> throw(badarg);
        ConnectionPid->  mysql:query(ConnectionPid, Format, Data)
    end.
%%--------------------------------------------------------------------
%%% @doc
%%% Returns table name from application environment.
%%% @end
%%--------------------------------------------------------------------
-spec get_table_name()-> binary().
get_table_name()->
    Options = application:get_env(rest_auth, mysql, []),
    proplists:get_value(table, Options, <<"auth">>).

%%--------------------------------------------------------------------
%%% @doc
%%% Returns lists of all existing usernames.
%%% @end
%%--------------------------------------------------------------------
-spec get_user_list()-> list().
get_user_list()->
    get_user_list(get_table_name()).
-spec get_user_list(binary())-> list().
get_user_list(Table)->
    case rest_auth:q(<<"select username from ", Table/binary, ";">>, []) of
        {ok,[<<"username">>],UserList}->
            lists:flatten(UserList);
        _-> 
            []
    end.

%%--------------------------------------------------------------------
%%% @doc
%%% Create new user, if username does not exist. Return {error, Reason} if failed.
%%% @end
%%--------------------------------------------------------------------
-spec add_user(str(), str())-> ok | {error, atom()}.
add_user(UserName, Password)->
    add_user(get_table_name(), UserName, Password).
-spec add_user(binary(), str(), str())-> ok | {error, atom()}.
add_user(Table, UserName, Password)->
    case rest_auth:q(<<"insert into ", Table/binary, " (username, sha) values (?, sha(?));">>, [UserName, Password]) of
        ok->
            ok;
        {error,{1062,<<"23000">>, _}}-> 
            {error, already_exist}
    end.

%%--------------------------------------------------------------------
%%% @doc
%%% User authentication.
%%% @end
%%--------------------------------------------------------------------
-spec dirty_auth_user(str(), str())-> boolean().
dirty_auth_user(UserName, Password)->
    dirty_auth_user(get_table_name(), UserName, Password).
-spec dirty_auth_user(binary(), str(), str())-> boolean().
dirty_auth_user(Table, UserName, Password) when is_list(UserName)->
    dirty_auth_user(Table, list_to_binary(UserName), Password);
dirty_auth_user(Table, UserName, Password) when is_binary(UserName)->
    case rest_auth:q(<<"select username from ", Table/binary, " where username= ? and sha=sha(?);">>, [UserName, Password]) of
        {ok,[<<"username">>],[[UserName]]} -> true;
        _-> false
    end.

%%--------------------------------------------------------------------
%%% @doc
%%% User password update.
%%% @end
%%--------------------------------------------------------------------
-spec dirty_update_user(str(), str(), str())-> boolean().
dirty_update_user(UserName, Password, NewPassword)->
    dirty_update_user(get_table_name(), UserName, Password, NewPassword).
-spec dirty_update_user(binary(), str(), str(), str())-> boolean().
dirty_update_user(Table, UserName, Password, NewPassword)->
    case rest_auth_sup:get_child_pid(mysql) of
        false -> throw(badarg);
        ConnectionPid->  
            ok = mysql:query(ConnectionPid, <<"update ", Table/binary, " set sha=sha(?) where username= ? and sha=sha(?);">>, [NewPassword, UserName, Password]),
            case mysql:affected_rows(ConnectionPid) of
                0->
                   false;
                1->
                    true;
                _N-> %% modified too much records log as error
                    error_logger:error_msg("~p rows were updated with dirty_update_user(~p,~p,~p,~p,~p) call.",[_N, Table, UserName, Password, NewPassword]),
                    true
            end
    end.

%%--------------------------------------------------------------------
%%% @doc
%%% MySql client start_link. Called by application suprvisor
%%% @end
%%--------------------------------------------------------------------
-spec start_mysql(list())-> {ok, pid()}.
start_mysql(Options)->
    {ok, Pid} = mysql:start_link(Options),
    ok = init_table(Pid, proplists:get_value(table, Options, <<"default">>)),
    {ok, Pid}.

%%--------------------------------------------------------------------
%%% @doc
%%% MySql table initialization.
%%% @end
%%--------------------------------------------------------------------
-spec init_table(pid(), binary()|list())-> ok | no_return().
init_table(Pid, TableName) when is_list(TableName)->
    init_table(Pid, list_to_binary(TableName));
init_table(ConnectionPid, TableName) when is_binary(TableName)->
    case mysql:query(ConnectionPid, <<"DESCRIBE ", TableName/binary, ";">>, []) of
        {error,{_,<<"42S02">>, _}}->
            %% create table
            ok = mysql:query(ConnectionPid, <<"CREATE TABLE ", TableName/binary, " (username VARCHAR(20), sha VARCHAR(40), PRIMARY KEY (username));">>,[]);
        {ok, _,_}->
            %% table already exist
            ok
    end.

