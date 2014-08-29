%%% @doc
%%% Main application activity module.

%%% @author Aleksey Morarash <aleksey.morarash@proffero.com>
%%% @since 29 Aug 2014
%%% @copyright 2014, Proffero <info@proffero.com>

-module(proffero_counters_srv).

-behaviour(gen_server).

%% API exports
-export(
   [start_link/0,
    increment/2,
    get/1,
    list/0
   ]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("proffero_counters.hrl").

%% ----------------------------------------------------------------------
%% Internal signals
%% ----------------------------------------------------------------------

-define(INCREMENT(CounterName, Delta), {'*increment*', CounterName, Delta}).

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
-spec increment(CounterName :: proffero_counters:name(),
                Delta :: proffero_counters:delta()) -> ok.
increment(CounterName, Delta) ->
    _Sent = ?MODULE ! ?INCREMENT(CounterName, Delta),
    ok.

%% @doc Fetch counter value.
-spec get(CounterName :: proffero_counters:name()) ->
                 Value :: proffero_counters:value().
get(CounterName) ->
    case ets:lookup(?MODULE, CounterName) of
        [{CounterName, Value}] ->
            Value;
        [] ->
            0
    end.

%% @doc Return list of all counters available.
-spec list() -> [proffero_counters:name()].
list() ->
    [Name || {Name, _Value} <- ets:tab2list(?MODULE)].

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
