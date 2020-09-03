%%%-------------------------------------------------------------------
%%% File    : rest_auth_app.erl
%%% Author  : redpate
%%% Description : 
%%% @doc
%%% Rest_auth application top level supervisor
%%% @end
%%% Created : 09/03/2020
%%%-------------------------------------------------------------------

-module(rest_auth_sup).

-behaviour(supervisor).

-export([start_link/0, get_child_pid/1]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    MysqlConnetionOptions = application:get_env(rest_auth, mysql, []),
    Dispatch = cowboy_router:compile(application:get_env(rest_auth, cowboy_dispatch, [])),
	CowboyStartOptions = [http, application:get_env(rest_auth, cowboy_opt, [{port,8080}]), #{env => #{dispatch => Dispatch}}],
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
                #{id => mysql,
                    start => {rest_auth, start_mysql, [MysqlConnetionOptions]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [mysql]},
                #{id => cowboy,
                    start => {cowboy, start_clear, CowboyStartOptions},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => dynamic}
                ],
    {ok, {SupFlags, ChildSpecs}}.

%%--------------------------------------------------------------------
%%% @doc
%%% Returns 'Pid' of child with defined 'ID'
%%% @end
%%--------------------------------------------------------------------
-spec get_child_pid(atom())-> pid() | boolean().
get_child_pid(ID)->
    ChildrenList = supervisor:which_children(?MODULE),
    case lists:keyfind(ID, 1, ChildrenList) of
        {ID,Pid,_,_}->Pid;
        _-> false
    end.
