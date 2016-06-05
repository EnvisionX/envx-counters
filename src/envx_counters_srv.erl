%%% @doc
%%% Main application activity module.

%%% @author Aleksey Morarash <aleksey.morarash@envisionx.co>
%%% @since 29 Aug 2014
%%% @copyright 2014, EnvisionX <info@envisionx.co>

-module(envx_counters_srv).

-behaviour(gen_server).

%% API exports
-export(
   [start_link/0,
    increment/2,
    set/2,
    get/1,
    list/0,
    dump/0,
    drop/0,
    drop/1,
    reset/0
   ]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("envx_counters.hrl").

%% ----------------------------------------------------------------------
%% Internal signals
%% ----------------------------------------------------------------------

-define(INCREMENT(CounterName, Delta), {'*increment*', CounterName, Delta}).
-define(SET(CounterName, Value), {'*set*', CounterName, Value}).
-define(RESET, '*reset*').
-define(DROP, '*drop*').
-define(DROP_ONE(CounterName), {'*drop*', CounterName}).

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Start the process as part of the supervision tree.
-spec start_link() -> {ok, pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    gen_server:start_link(
      {local, ?MODULE}, ?MODULE, _Args = undefined, _Options = []).

%% @doc Increment a counter with delta, creating it if it is not exists yet.
%% The implementation is based on direct message send instead of
%% gen_server:cast/2 because it is faster: no monitors are created
%% and removed during the operation.
-spec increment(CounterName :: envx_counters:name(),
                Delta :: envx_counters:delta()) -> ok.
increment(CounterName, Delta) ->
    CanonicName = canonicalize(CounterName),
    _Sent = ?MODULE ! ?INCREMENT(CanonicName, Delta),
    ok.

%% @doc Set a new value of the counter.
-spec set(CounterName :: envx_counters:name(),
          Value :: envx_counters:value()) -> ok.
set(CounterName, Value) ->
    CanonicName = canonicalize(CounterName),
    _Sent = ?MODULE ! ?SET(CanonicName, Value),
    ok.

%% @doc Fetch counter value.
-spec get(CounterName :: envx_counters:name()) ->
                 Value :: envx_counters:value().
get(CounterName) ->
    CanonicName = canonicalize(CounterName),
    case ets:lookup(?MODULE, CanonicName) of
        [{CanonicName, Value}] ->
            Value;
        [] ->
            0
    end.

%% @doc Return list of all counters available.
-spec list() -> [envx_counters:name()].
list() ->
    [Name || {Name, _Value} <- ets:tab2list(?MODULE)].

%% @doc Return sorted list of all counters with their values.
-spec dump() -> [{envx_counters:name(), envx_counters:value()}].
dump() ->
    lists:sort(ets:tab2list(?MODULE)).

%% @doc Remove all existing counters.
-spec drop() -> ok.
drop() ->
    _Sent = ?MODULE ! ?DROP,
    ok.

%% @doc Remove all existing counters.
-spec drop(envx_counters:name()) -> ok.
drop(CounterName) ->
    _Sent = ?MODULE ! ?DROP_ONE(CounterName),
    ok.

%% @doc Reset all existing counters to zero.
-spec reset() -> ok.
reset() ->
    _Sent = ?MODULE ! ?RESET,
    ok.

%% ----------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------

-record(state, {}).

%% @hidden
-spec init(Args :: any()) -> {ok, InitialState :: #state{}}.
init(_Args) ->
    ?MODULE = ets:new(?MODULE, [named_table]),
    {ok, _State = #state{}}.

%% @hidden
-spec handle_cast(Request :: any(), State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
-spec handle_info(Info :: any(), State :: #state{}) ->
                         {noreply, State :: #state{}}.
handle_info(?INCREMENT(CounterName, Delta), State) ->
    _Ignored =
        try
            ets:update_counter(?MODULE, CounterName, Delta)
        catch
            error:badarg ->
                %% no such counter present. Create it
                ets:insert(?MODULE, {CounterName, Delta})
        end,
    {noreply, State};
handle_info(?SET(CounterName, Value), State) ->
    true = ets:insert(?MODULE, {CounterName, Value}),
    {noreply, State};
handle_info(?RESET, State) ->
    _Ignored =
        ets:foldl(
          fun({Counter, _Value}, Accum) ->
                  true = ets:insert(?MODULE, {Counter, 0}),
                  Accum
          end, _Accum0 = undefined, ?MODULE),
    {noreply, State};
handle_info(?DROP, State) ->
    true = ets:delete_all_objects(?MODULE),
    {noreply, State};
handle_info(?DROP_ONE(CounterName), State) ->
    true = ets:delete(?MODULE, CounterName),
    {noreply, State};
handle_info(_Request, State) ->
    {noreply, State}.

%% @hidden
-spec handle_call(Request :: any(), From :: any(), State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @hidden
-spec terminate(Reason :: any(), State :: #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @hidden
-spec code_change(OldVersion :: any(), State :: #state{}, Extra :: any()) ->
                         {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc Canonicalize counter name (convert binaries to atoms).
-spec canonicalize(envx_counters:name()) -> envx_counters:canonic_name().
canonicalize(CounterName) ->
    [if is_binary(Elem) ->
             list_to_atom(binary_to_list(Elem));
        is_integer(Elem) ->
             list_to_atom(integer_to_list(Elem));
        true ->
             Elem
     end || Elem <- CounterName].
