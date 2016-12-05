%%% @doc
%%% API for external processes to fetch current values of the counters.
%%% This process listens two sockets:
%%% - TCP server socket listens for incoming TCP connections;
%%% - UDP socket.
%%%
%%% For each new incoming TCP connection the process spawns a separate
%%% worker process.
%%% Each incoming UDP request the process handles itself.

%%% @author Aleksey Morarash <aleksey.morarash@envisionx.co>
%%% @since 29 Aug 2014
%%% @copyright 2014, ENVISIONX <info@envisionx.co>

-module(envx_counters_ext_api).

-behaviour(gen_server).

%% API exports
-export(
   [start_link/0,
    decode_request/1,
    process/1,
    encode_reply/1
   ]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("envx_counters.hrl").
-include("envx_counters_private.hrl").

-define(CMD_LIST, list).
-define(CMD_GET, get).
-define(CMD_DUMP, dump).
-define(CMD_RESET, reset).

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-export_type(
   [request/0,
    reply/0
   ]).

-type request() ::
        ?CMD_LIST | ?CMD_DUMP | ?CMD_RESET | {?CMD_GET, envx_counters:name()}.

-type reply() ::
        (CounterList :: [envx_counters:name()]) |
        (Dump :: binary()) |
        (CounterValue :: envx_counters:value()).

%% ----------------------------------------------------------------------
%% Internal signals and other defs
%% ----------------------------------------------------------------------

-define(CHECK_SOCKETS, '*check_sockets*').
-define(ACCEPT_CONNECTION, '*accept_connection*').

-define(SOCKET_REOPEN_PERIOD, 30 * 1000). %% 30 seconds
-define(CONNECTION_ACCEPT_PAUSE, 500). %% half a second

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Start the process as part of the supervision tree.
-spec start_link() -> {ok, pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    gen_server:start_link(
      {local, ?MODULE}, ?MODULE, _Args = undefined, _Options = []).

%% @doc Decode external request.
-spec decode_request(EncodedRequest :: nonempty_string()) ->
                            {ok, request()} | error.
decode_request(EncodedRequest) ->
    case string:tokens(EncodedRequest, " \t\r\n") of
        [RawCommand | Args] ->
            case string_to_atom(string:to_lower(RawCommand),
                                [?CMD_LIST, ?CMD_DUMP, ?CMD_RESET, ?CMD_GET]) of
                {ok, ?CMD_LIST = Command} when Args == [] ->
                    {ok, Command};
                {ok, ?CMD_DUMP = Command} when Args == [] ->
                    {ok, Command};
                {ok, ?CMD_RESET = Command} when Args == [] ->
                    {ok, Command};
                {ok, ?CMD_GET = Command} when length(Args) == 1 ->
                    [RawCounterName] = Args,
                    case decode_name(RawCounterName) of
                        {ok, CounterName} ->
                            {ok, {Command, CounterName}};
                        error ->
                            error
                    end;
                error ->
                    error
            end;
        _ ->
            error
    end.

%% @doc Process the request.
-spec process(?CMD_LIST) -> [envx_counters:name()];
             (?CMD_DUMP) -> binary();
             ({?CMD_GET, envx_counters:name()}) ->
                     {envx_counters:name(),
                      envx_counters:value()}.
process(?CMD_LIST) ->
    envx_counters_srv:list();
process(?CMD_DUMP) ->
    case [io_lib:format("~s ~w\n", [encode_name(N), V]) ||
             {N, V} <- envx_counters_srv:dump()] of
        [] ->
            <<$\n>>;
        Dump ->
            iolist_to_binary(Dump)
    end;
process(?CMD_RESET) ->
    ok = envx_counters:reset(),
    <<"\n">>;
process({?CMD_GET, CounterName}) ->
    {CounterName, envx_counters_srv:get(CounterName)}.

%% @doc Encode the reply.
-spec encode_reply(Reply :: reply()) -> iolist().
encode_reply(Binary) when is_binary(Binary) ->
    Binary;
encode_reply(Reply) when is_list(Reply) ->
    string:join([encode_name(CounterName) || CounterName <- Reply], " ");
encode_reply({CounterName, CounterValue}) ->
    io_lib:format("~s ~w", [encode_name(CounterName), CounterValue]).

%% ----------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------

-record(
   state,
   {udp_socket :: port() | undefined,
    tcp_socket :: port() | undefined
   }).

%% @hidden
-spec init(Args :: any()) -> {ok, InitialState :: #state{}}.
init(_Args) ->
    ok = schedule_check_sockets(0),
    {ok, _State = #state{}}.

%% @hidden
-spec handle_cast(Request :: any(), State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
-spec handle_info(Info :: any(), State :: #state{}) ->
                         {noreply, State :: #state{}}.
handle_info(?CHECK_SOCKETS, State) ->
    {noreply, check_sockets(State)};
handle_info(?ACCEPT_CONNECTION, State) ->
    case gen_tcp:accept(State#state.tcp_socket, 0) of
        {ok, Socket} ->
            %% spawn
            ok = envx_counters_ext_api_tcp_connection:start_link(Socket),
            ok = schedule_connection_accept(0),
            {noreply, State};
        {error, timeout} ->
            ok = schedule_connection_accept(?CONNECTION_ACCEPT_PAUSE),
            {noreply, State};
        {error, _Reason} ->
            catch gen_tcp:close(State#state.tcp_socket),
            {noreply, check_sockets(State#state{tcp_socket = undefined})}
    end;
handle_info({udp, Socket, FromIP, FromPort, EncodedRequest}, State)
  when State#state.udp_socket == Socket ->
    case decode_request(EncodedRequest) of
        {ok, Request} ->
            Reply = process(Request),
            EncodedReply = encode_reply(Reply),
            case gen_udp:send(
                   Socket, FromIP, FromPort, EncodedReply) of
                ok ->
                    {noreply, State};
                {error, _Reason} ->
                    catch gen_tcp:close(Socket),
                    {noreply,
                     check_sockets(
                       State#state{udp_socket = undefined})}
            end;
        error ->
            {noreply, State}
    end;
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

%% @doc Decode counter name.
-spec decode_name(String :: nonempty_string()) ->
                         {ok, envx_counters:name()} | error.
decode_name(String) ->
    try lists:map(fun list_to_atom/1, string:tokens(String, ".")) of
        [_ | _] = NonEmpty ->
            {ok, NonEmpty};
        [] = _Empty ->
            error
    catch
        _:_ ->
            error
    end.

%% @doc Encode counter name to external form.
-spec encode_name(CounterName :: envx_counters:name()) ->
                         EncodedCounterName :: iolist().
encode_name(CounterName) ->
    string:join(lists:map(fun encode_name_elem/1, CounterName), ".").

%% @doc Encode counter name element.
-spec encode_name_elem(atom() | binary() | integer()) -> string().
encode_name_elem(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
encode_name_elem(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
encode_name_elem(Atom) when is_atom(Atom) ->
    atom_to_list(Atom).

%% @doc Schedule sockets check.
-spec schedule_check_sockets(TimeMillis :: non_neg_integer()) -> ok.
schedule_check_sockets(0 = _Immediately) ->
    _Sent = ?MODULE ! ?CHECK_SOCKETS,
    ok;
schedule_check_sockets(TimeMillis) ->
    {ok, _TimerRef} = timer:send_after(TimeMillis, ?CHECK_SOCKETS),
    ok.

%% @doc Reopen sockets if needed.
-spec check_sockets(State :: #state{}) -> NewState :: #state{}.
check_sockets(State) ->
    check_tcp_socket(check_udp_socket(State)).

%% @doc Reopen UDP socket if needed.
-spec check_udp_socket(State :: #state{}) -> NewState :: #state{}.
check_udp_socket(State) when is_port(State#state.udp_socket) ->
    State;
check_udp_socket(State) ->
    %% need to reopen
    BindPort =
        envx_config:get(?CFG_UDP_BIND_PORT, ?DEFAULT_UDP_BIND_PORT),
    SocketOpts =
        [{active, true}, {reuseaddr, true}, list],
    case gen_udp:open(BindPort, SocketOpts) of
        {ok, Socket} ->
            State#state{udp_socket = Socket};
        {error, _Reason} ->
            ok = schedule_check_sockets(?SOCKET_REOPEN_PERIOD),
            State
    end.

%% @doc Reopen TCP socket if needed.
-spec check_tcp_socket(State :: #state{}) -> NewState :: #state{}.
check_tcp_socket(State) when is_port(State#state.tcp_socket) ->
    State;
check_tcp_socket(State) ->
    %% need to reopen
    BindPort =
        envx_config:get(?CFG_TCP_BIND_PORT, ?DEFAULT_TCP_BIND_PORT),
    SocketOpts =
        [{active, false}, {reuseaddr, true}, list,
         {send_timeout_close, true}, {packet, line}],
    case gen_tcp:listen(BindPort, SocketOpts) of
        {ok, Socket} ->
            ok = schedule_connection_accept(0),
            State#state{tcp_socket = Socket};
        {error, _Reason} ->
            ok = schedule_check_sockets(?SOCKET_REOPEN_PERIOD),
            State
    end.

%% @doc Schedule accept of a new TCP connection.
-spec schedule_connection_accept(TimeMillis :: non_neg_integer()) -> ok.
schedule_connection_accept(0 = _Immediately) ->
    _Sent = ?MODULE ! ?ACCEPT_CONNECTION,
    ok;
schedule_connection_accept(TimeMillis) ->
    {ok, _TimerRef} = timer:send_after(TimeMillis, ?ACCEPT_CONNECTION),
    ok.

%% @doc Safely convert a string to an atom().
%% The implementation is not fast like erlang:list_to_atom/1,
%% but prevents of creation of new atoms, which is not garbage
%% collected.
%% erlang:list_to_existing_atom/1 is not used because our goal
%% is not ANY existing atom, but only one of the given.
-spec string_to_atom(String :: string(),
                     Atoms :: [atom()]) ->
                            {ok, atom()} | error.
string_to_atom(_String, []) ->
    error;
string_to_atom(String, [Atom | Tail]) ->
    case atom_to_list(Atom) of
        String ->
            {ok, Atom};
        _Other ->
            string_to_atom(String, Tail)
    end.
