%%%-------------------------------------------------------------------
%%% File    : rest_auth_app.erl
%%% Author  : redpate
%%% Description : 
%%% @doc
%%% Rest_auth application module
%%% @end
%%% Created : 09/03/2020
%%%-------------------------------------------------------------------

-module(rest_auth_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    rest_auth_sup:start_link().

stop(_State) ->
    ok.
