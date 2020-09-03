-module(basic_SUITE).

%% tests for mysql interface

-include_lib("mixer/include/mixer.hrl").
-include_lib("stdlib/include/assert.hrl").
-mixin([{ktn_meta_SUITE, [dialyzer/1]}]).

-compile(export_all).

all()->
    [
        new_test,
        auth_test,
        mod_test
    ].

-type config() :: [{atom(), term()}].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    application:ensure_all_started(rest_auth),
    [{application, rest_auth}| Config].

-spec end_per_suite(config()) -> term() | {save_config, config()}.
end_per_suite(Config) ->
    Table = rest_auth:get_table_name(),
    rest_auth:q(<<"DROP TABLE ", Table/binary, ";">>, []),
    application:stop(rest_auth),
    Config.

-define(NEW_RECORD, 100).
new_test(_Config)->
    UserList1 = rest_auth:get_user_list(),
    ?assertEqual(UserList1, []),
    lists:foreach(fun(_X)-> rest_auth:add_user(base64:encode(crypto:strong_rand_bytes(10)), "Password") end, lists:seq(1,?NEW_RECORD)),
    UserList2 = rest_auth:get_user_list(),
    ?assertNotEqual(UserList1, UserList2),
    ?assertEqual(length(UserList2), ?NEW_RECORD).

auth_test(_Config)->
    UserList1 = rest_auth:get_user_list(),
    ?assertNotEqual(UserList1, []),
    TrueList = lists:usort(lists:map(fun(X)-> rest_auth:dirty_auth_user(X, "Password") end, UserList1)),
    ?assertEqual([true], TrueList),
    FalseList = lists:usort(lists:map(fun(X)-> rest_auth:dirty_auth_user(X, "Password'; 1=1") end, UserList1)),
    ?assertEqual([false], FalseList).

mod_test(_Config)->
    UserList1 = rest_auth:get_user_list(),
    ?assertNotEqual(UserList1, []),
    TrueList = lists:usort(lists:map(fun(X)-> rest_auth:dirty_update_user(X, "Password", "foobar") end, UserList1)),
    ?assertEqual([true], TrueList),
    FalseList = lists:usort(lists:map(fun(X)-> rest_auth:dirty_update_user(X, "Password", "deadbeaf") end, UserList1)),
    ?assertEqual([false], FalseList).