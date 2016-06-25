%%% @doc
%%% API for external processes to fetch current values of the counters.

%%% @author Aleksey Morarash <aleksey.morarash@envisionx.co>
%%% @since 29 Aug 2014
%%% @copyright 2014, ENVISIONX <info@envisionx.co>

-module(envx_counters_ext_api_tcp_connection).

%% API exports
-export([start_link/1]).

-include("envx_counters.hrl").
-include("envx_counters_private.hrl").
-include_lib("envx_logger/include/envx_logger.hrl").

-record(
   state,
   {log_id :: nonempty_string(),
    socket :: port()
   }).

-define(MAX_READ_TIMEOUT, 60 * 1000). %% one minute

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Start a new linked process.
-spec start_link(Socket :: port()) -> ok.
start_link(Socket) ->
    {ok, {Addr, Port}} = inet:peername(Socket),
    LogID = envx_lib_inet:socket_to_list(Addr, Port) ++ ">>",
    ReadySignal = make_ref(),
    Pid =
        spawn_link(
          fun() ->
                  try
                      %% wait until an owner of the socket
                      %% will be changed
                      receive
                          ReadySignal ->
                              ok
                      after 5000 ->
                              %% something went wrong
                              throw(ready_signal_missing)
                      end,
                      State =
                          #state{log_id = LogID,
                                 socket = Socket},
                      ?info("~s started", [LogID]),
                      loop(State),
                      ?info("~s finished", [LogID])
                  catch
                      ExcType:ExcReason ->
                          ?error(
                             "crashed: log_id=~w; type=~w; reason=~9999p;"
                             " stacktrace=~9999p",
                             [LogID, ExcType, ExcReason,
                              erlang:get_stacktrace()])
                  end
          end),
    ok = gen_tcp:controlling_process(Socket, Pid),
    _Sent = Pid ! ReadySignal,
    ok.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc
-spec loop(State :: #state{}) -> ok.
loop(State) ->
    case gen_tcp:recv(State#state.socket, 0, ?MAX_READ_TIMEOUT) of
        {error, closed} ->
            %% connection is closed from the other side
            ok;
        {error, timeout} ->
            %% max read timeout elapsed - closing the connection
            ok;
        {ok, EncodedRequest} ->
            case envx_counters_ext_api:decode_request(EncodedRequest) of
                {ok, Request} ->
                    Reply = envx_counters_ext_api:process(Request),
                    EncodedReply =
                        envx_counters_ext_api:encode_reply(Reply),
                    case gen_tcp:send(
                           State#state.socket, [EncodedReply, $\n]) of
                        ok ->
                            loop(State);
                        {error, Reason} ->
                            ?error(
                               "failed to send reply over TCP: ~9999p",
                               [Reason])
                    end;
                error ->
                    ?error("unable to decode request: ~s", [EncodedRequest])
            end
    end.
