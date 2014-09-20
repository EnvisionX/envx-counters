%%% @doc
%%% Non-persistent counters storage interface - main interface module.

%%% @author Aleksey Morarash <aleksey.morarash@proffero.com>
%%% @since 29 Aug 2014
%%% @copyright 2014, Proffero <info@proffero.com>

-module(proffero_counters).

%% API exports
-export(
   [increment/1,
    increment/2,
    get/1,
    list/0
   ]).

-include("proffero_counters.hrl").
-include("proffero_counters_private.hrl").

%% --------------------------------------------------------------------
%% Type definitions
%% --------------------------------------------------------------------

-export_type(
   [name/0,
    value/0,
    delta/0
   ]).

-type name() ::
        [atom() | binary(), ...]. %% nonempty list of atoms or binaries

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
    proffero_counters_srv:increment(CounterName, Delta).

%% @doc Fetch counter value.
-spec get(CounterName :: name()) -> Value :: value().
get(CounterName) ->
    proffero_counters_srv:get(CounterName).

%% @doc Return list of all counters available.
-spec list() -> [name()].
list() ->
    proffero_counters_srv:list().

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

-define(CLI, "clients/cli/proffero-counters-cli.py").
-define(CLI_UDP, ?CLI " --udp").

main_test_() ->
    proffero_lib_eunit:fixture_nodes(
      [{?slave, "-pa ebin"}],
      {inorder,
       [?_assertMatch(
           ok, ?slave_apply(proffero_lib_application, start, [?MODULE])),
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
             [?c1, ?c2], lists:sort(?slave_apply(?MODULE, list, [])))]},
        {"CLI tool TCP test",
         [?_assertMatch(
             "a.b.c\nd.e.f\n",
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
             "a.b.c\nd.e.f\n",
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
