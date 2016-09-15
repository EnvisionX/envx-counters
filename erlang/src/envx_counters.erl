%%% @doc
%%% Non-persistent counters storage interface - main interface module.

%%% @author Aleksey Morarash <aleksey.morarash@envisionx.co>
%%% @since 29 Aug 2014
%%% @copyright 2014, ENVISIONX <info@envisionx.co>

-module(envx_counters).

%% API exports
-export(
   [increment/1,
    increment/2,
    set/2,
    get/1,
    list/0,
    dump/0,
    dump_rates/0,
    print/0,
    print_rates/0,
    drop/0,
    drop/1,
    reset/0,
    is_enabled/0,
    set_enabled/1
   ]).

-include("envx_counters.hrl").
-include("envx_counters_private.hrl").

-define(is_enabled, (whereis(?MODULE) == undefined)).

%% --------------------------------------------------------------------
%% Type definitions
%% --------------------------------------------------------------------

-export_type(
   [name/0,
    canonic_name/0,
    value/0,
    value_getter/0,
    delta/0
   ]).

-type name() ::
        canonic_name() |
        [atom() | binary() | integer(), ...].

-type canonic_name() ::
        [atom(), ...]. %% nonempty list of atoms

-type value() :: integer().

-type value_getter() :: fun(() -> value()).

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
    case ?is_enabled of
        true ->
            envx_counters_srv:increment(CounterName, Delta);
        false ->
            ok
    end.

%% @doc Set a new value of the counter.
-spec set(CounterName :: name(), Value :: value() | value_getter()) -> ok.
set(CounterName, Value) ->
    case ?is_enabled of
        true ->
            envx_counters_srv:set(CounterName, Value);
        false ->
            ok
    end.

%% @doc Fetch counter value.
-spec get(CounterName :: name()) -> Value :: value().
get(CounterName) ->
    case ?is_enabled of
        true ->
            envx_counters_srv:get(CounterName);
        false ->
            0
    end.

%% @doc Return list of all counters available.
-spec list() -> [name()].
list() ->
    case ?is_enabled of
        true ->
            envx_counters_srv:list();
        false ->
            []
    end.

%% @doc Return sorted list of all counters with their values.
-spec dump() -> [{name(), value()}].
dump() ->
    case ?is_enabled of
        true ->
            envx_counters_srv:dump();
        false ->
            []
    end.

%% @doc Make two dumps and calculate rate of each counter.
-spec dump_rates() -> [{name(), integer()}].
dump_rates() ->
    Dump1 = dump(),
    ok = timer:sleep(1000),
    Dump2 = dump(),
    Counters = lists:usort([K || {K, _} <- Dump1] ++ [K || {K, _} <- Dump2]),
    lists:map(
      fun(Counter) ->
              V1 = proplists:get_value(Counter, Dump1, 0),
              V2 = proplists:get_value(Counter, Dump2, 0),
              Delta = V2 - V1,
              {Counter, Delta}
      end, Counters).

%% @doc Print counters dump to stdout.
-spec print() -> ok.
print() ->
    io:format("~p~n", [lists:sort(dump())]).

%% @doc Print rates for each counter (changes per second).
-spec print_rates() -> ok.
print_rates() ->
    io:format("~p~n", [dump_rates()]).

%% @doc Remove all existing counters.
-spec drop() -> ok.
drop() ->
    case ?is_enabled of
        true ->
            envx_counters_srv:drop();
        false ->
            ok
    end.

%% @doc Remove counter.
-spec drop(CounterName :: name()) -> ok.
drop(CounterName) ->
    case ?is_enabled of
        true ->
            envx_counters_srv:drop(CounterName);
        false ->
            ok
    end.

%% @doc Reset all existing counters to zero.
-spec reset() -> ok.
reset() ->
    case ?is_enabled of
        true ->
            envx_counters_srv:reset();
        false ->
            ok
    end.

%% @doc Return 'true' if library is in enabled mode (collecting and storing
%% counters and gauges) and 'false' when all calls like increment/1, set/2
%% are ignored.
-spec is_enabled() -> boolean().
is_enabled() ->
    %% the most efficient way to maintain such a global flag is to
    %% create registered process. When process is alive (registered
    %% name maps to a PID), then flag is ON.
    %% Here we use opposite meaning: in normal mode (envx_counters
    %% library is enabled) the process does not exists.
    ?is_enabled.

%% @doc Change library mode from enabled to disabled and vice versa.
%% See description for is_enabled/0 for more details.
%% This function intended only for testing purposes and can be used to
%% bypass efforts for counter saving.
-spec set_enabled(IsEnabled :: boolean()) -> ok.
set_enabled(false) ->
    _Pid =
        spawn(
          fun() ->
                  %% entry point of flag process
                  try
                      true = register(?MODULE, self()),
                      %% name registered, waiting
                      %% for a signal to terminate
                      receive
                          stop ->
                              ok
                      end
                  catch
                      error:badarg ->
                          %% race: process with such
                          %% name already registered.
                          %% just terminate
                          ok
                  end
          end),
    ok;
set_enabled(true) ->
    %% killing flag process
    case whereis(?MODULE) of
        undefined ->
            ok;
        PID ->
            stop = PID ! stop,
            ok
    end.

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
-define(c4, [g, <<"h">>, 1]).

-define(CLI, "../bin/envx-counters-cli").
-define(CLI_UDP, ?CLI " --udp").

main_test_() ->
    envx_lib_eunit:fixture_nodes(
      [{?slave, "-pa ebin"}],
      {inorder,
       [?_assertMatch(
           {ok, _}, ?slave_apply(application, ensure_all_started, [?MODULE])),
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
          ?_assertMatch(10, ?slave_apply(?MODULE, get, [?c3])),
          ?_assertMatch(ok, ?slave_apply(?MODULE, increment, [?c4, 10])),
          ?_assertMatch(10, ?slave_apply(?MODULE, get, [?c4]))
         ]},
        {"CLI tool TCP test",
         [?_assertMatch(
             "a.b.c\nd.e.f\ng.h.1\ng.h.j\n",
             os:cmd(?CLI " list | sort")),
          ?_assertMatch(
             "a.b.c -1\n",
             os:cmd(?CLI " get a.b.c")),
          ?_assertMatch(
             "d.e.f -100\n",
             os:cmd(?CLI " get d.e.f")),
          ?_assertMatch(
             "a.b.c -1\nd.e.f -100\n",
             os:cmd(?CLI " get a.b.c d.e.f")),
          ?_assertMatch(
             "g.h.1 10\n",
             os:cmd(?CLI " get g.h.1"))
         ]},
        {"CLI tool UDP test",
         [?_assertMatch(
             "a.b.c\nd.e.f\ng.h.1\ng.h.j\n",
             os:cmd(?CLI_UDP " list | sort")),
          ?_assertMatch(
             "a.b.c -1\n",
             os:cmd(?CLI_UDP " get a.b.c")),
          ?_assertMatch(
             "d.e.f -100\n",
             os:cmd(?CLI_UDP " get d.e.f")),
          ?_assertMatch(
             "a.b.c -1\nd.e.f -100\n",
             os:cmd(?CLI_UDP " get a.b.c d.e.f")),
          ?_assertMatch(
             "g.h.1 10\n",
             os:cmd(?CLI_UDP " get g.h.1"))
         ]}
       ]}).

getter_test_() ->
    {setup,
     _StartUp =
         fun() ->
                 {ok, _Apps} = application:ensure_all_started(?MODULE)
         end,
     _CleanUp =
         fun({ok, Apps}) ->
                 lists:foreach(
                   fun(App) ->
                           ok = application:stop(App)
                   end, Apps)
         end,
     {inorder,
      [
       ?_assertMatch(0, ?MODULE:get(?c1)),
       ?_assertMatch("a.b.c 0\n", os:cmd(?CLI " get a.b.c")),
       ?_assertMatch([], dump()),
       ?_assertMatch(ok, set(?c1, fun() -> 1234 end)),
       ?_assertMatch(1234, ?MODULE:get(?c1)),
       ?_assertMatch("a.b.c 1234\n", os:cmd(?CLI " get a.b.c")),
       ?_assertMatch([{[a,b,c], 1234}], dump()),
       ?_assertMatch(ok, set(?c1, fun() -> 12345 end)),
       ?_assertMatch(12345, ?MODULE:get(?c1)),
       ?_assertMatch([{[a,b,c], 12345}], dump()),
       ?_assertMatch("a.b.c 12345\n", os:cmd(?CLI " get a.b.c")),
       ?_assertMatch("a.b.c 12345\n", os:cmd(?CLI " dump"))
      ]}}.

disabled_mode_test_() ->
    {setup,
     _StartUp =
         fun() ->
                 {ok, _Apps} = application:ensure_all_started(?MODULE)
         end,
     _CleanUp =
         fun({ok, Apps}) ->
                 lists:foreach(
                   fun(App) ->
                           ok = application:stop(App)
                   end, Apps)
         end,
     {inorder,
      [{"Normal mode",
        [?_assert(is_enabled()),
         ?_assertMatch(0, ?MODULE:get(?c1)),
         ?_assertMatch(ok, increment(?c1)),
         ?_assertMatch(ok, increment(?c1)),
         ?_assertMatch(2, ?MODULE:get(?c1))]},
       {"Disabled mode",
        [?_assertMatch(ok, set_enabled(false)),
         ?_assertMatch(ok, timer:sleep(100)),
         ?_assertNot(is_enabled()),
         ?_assertMatch(0, ?MODULE:get(?c1)),
         ?_assertMatch(ok, increment(?c1)),
         ?_assertMatch(ok, increment(?c1)),
         ?_assertMatch(0, ?MODULE:get(?c1))]},
       {"Restore normal mode",
        [?_assertMatch(ok, set_enabled(true)),
         ?_assertMatch(ok, timer:sleep(100)),
         ?_assert(is_enabled()),
         ?_assertMatch(2, ?MODULE:get(?c1)),
         ?_assertMatch(ok, increment(?c1)),
         ?_assertMatch(ok, increment(?c1)),
         ?_assertMatch(4, ?MODULE:get(?c1))]}
      ]}}.

-endif.
