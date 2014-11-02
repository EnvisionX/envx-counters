%%% @doc
%%% Non-persistent counters storage interface - main interface module.

%%% @author Aleksey Morarash <aleksey.morarash@envisionx.co>
%%% @since 29 Aug 2014
%%% @copyright 2014, EnvisionX <info@envisionx.co>

-module(envx_counters).

%% API exports
-export(
   [increment/1,
    increment/2,
    set/2,
    get/1,
    list/0,
    reset/0
   ]).

-include("envx_counters.hrl").
-include("envx_counters_private.hrl").

%% --------------------------------------------------------------------
%% Type definitions
%% --------------------------------------------------------------------

-export_type(
   [name/0,
    canonic_name/0,
    value/0,
    delta/0
   ]).

-type name() ::
        canonic_name() |
        [atom() | binary(), ...]. %% nonempty list of atoms or binaries

-type canonic_name() ::
        [atom(), ...]. %% nonempty list of atoms

-type value() :: integer().

-type delta() :: integer().

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

%% @doc Increment a counter, creating it if it is not exists yet.
%% @equiv increment(CounterName, 1)
-spec increment(CounterName :: name()) -> ok.
increment(CounterName) ->
    increment(CounterName, 1).

%% @doc Increment a counter with delta,
%% creating it if it is not exists yet.
-spec increment(CounterName :: name(), Delta :: delta()) -> ok.
increment(CounterName, Delta) ->
    envx_counters_srv:increment(CounterName, Delta).

%% @doc Set a new value of the counter.
-spec set(CounterName :: name(), Value :: value()) -> ok.
set(CounterName, Value) ->
    envx_counters_srv:set(CounterName, Value).

%% @doc Fetch counter value.
-spec get(CounterName :: name()) -> Value :: value().
get(CounterName) ->
    envx_counters_srv:get(CounterName).

%% @doc Return list of all counters available.
-spec list() -> [name()].
list() ->
    envx_counters_srv:list().

%% @doc Reset all existing counters to zero.
-spec reset() -> ok.
reset() ->
    envx_counters_srv:reset().

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% ----------------------------------------------------------------------
%% eunit tests
%% ----------------------------------------------------------------------

-ifdef(TEST).

-define(slave, 'slave@127.1').
-define(slave_apply(M, F, A), rpc:call(?slave, M, F, A)).

-define(c1, [a, b, c]).
-define(c2, [d, e, f]).
-define(c3, [g, h, j]).

-define(CLI, "clients/cli/envx-counters-cli.py").
-define(CLI_UDP, ?CLI " --udp").

main_test_() ->
    envx_lib_eunit:fixture_nodes(
      [{?slave, "-pa ebin"}],
      {inorder,
       [?_assertMatch(
           ok, ?slave_apply(envx_lib_application, start, [?MODULE])),
        {"Erlang interface test",
         [?_assertMatch([], ?slave_apply(?MODULE, list, [])),
          ?_assertMatch(0,  ?slave_apply(?MODULE, get, [?c1])),
          ?_assertMatch(ok, ?slave_apply(?MODULE, increment, [?c1])),
          ?_assertMatch(1,  ?slave_apply(?MODULE, get, [?c1])),
          ?_assertMatch(ok, ?slave_apply(?MODULE, increment, [?c1, 2])),
          ?_assertMatch(3,  ?slave_apply(?MODULE, get, [?c1])),
          ?_assertMatch(ok, ?slave_apply(?MODULE, increment, [?c1, -4])),
          ?_assertMatch(-1, ?slave_apply(?MODULE, get, [?c1])),
          ?_assertMatch([?c1], ?slave_apply(?MODULE, list, [])),
          ?_assertMatch(ok, ?slave_apply(?MODULE, increment, [?c2, -100])),
          ?_assertMatch(-100, ?slave_apply(?MODULE, get, [?c2])),
          ?_assertMatch(
             [?c1, ?c2], lists:sort(?slave_apply(?MODULE, list, []))),
          ?_assertMatch(ok, ?slave_apply(?MODULE, set, [?c3, 1])),
          ?_assertMatch(1, ?slave_apply(?MODULE, get, [?c3])),
          ?_assertMatch(ok, ?slave_apply(?MODULE, set, [?c3, 5])),
          ?_assertMatch(5, ?slave_apply(?MODULE, get, [?c3])),
          ?_assertMatch(ok, ?slave_apply(?MODULE, increment, [?c3, 5])),
          ?_assertMatch(10, ?slave_apply(?MODULE, get, [?c3]))
         ]},
        {"CLI tool TCP test",
         [?_assertMatch(
             "a.b.c\nd.e.f\ng.h.j\n",
             os:cmd(?CLI " list | sort")),
          ?_assertMatch(
             "a.b.c -1\n",
             os:cmd(?CLI " get a.b.c")),
          ?_assertMatch(
             "d.e.f -100\n",
             os:cmd(?CLI " get d.e.f")),
          ?_assertMatch(
             "a.b.c -1\nd.e.f -100\n",
             os:cmd(?CLI " get a.b.c d.e.f"))
         ]},
        {"CLI tool UDP test",
         [?_assertMatch(
             "a.b.c\nd.e.f\ng.h.j\n",
             os:cmd(?CLI_UDP " list | sort")),
          ?_assertMatch(
             "a.b.c -1\n",
             os:cmd(?CLI_UDP " get a.b.c")),
          ?_assertMatch(
             "d.e.f -100\n",
             os:cmd(?CLI_UDP " get d.e.f")),
          ?_assertMatch(
             "a.b.c -1\nd.e.f -100\n",
             os:cmd(?CLI_UDP " get a.b.c d.e.f"))
         ]}
       ]}).

-endif.
