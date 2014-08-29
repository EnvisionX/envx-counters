%%% @doc
%%% Application main supervisor.

%%% @author Aleksey Morarash <aleksey.morarash@proffero.com>
%%% @since 29 Aug 2014
%%% @copyright 2014, Proffero <info@proffero.com>

-module(proffero_counters_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("proffero_counters.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Starts supervisor as part of a supervision tree.
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, undefined).

%% @doc Calls initialisation procedures and return child workers spec.
%% @hidden
-spec init(Args :: any()) ->
                  {ok,
                   {{RestartStrategy :: supervisor:strategy(),
                     MaxR :: non_neg_integer(),
                     MaxT :: non_neg_integer()},
                    [ChildSpec :: supervisor:child_spec()]}}.
init(_Args) ->
    {ok, {
       {one_for_one, 5, 1},
       [
        %% Main application activity
        {proffero_counters_srv, {proffero_counters_srv, start_link, []},
         permanent, 100, worker, [proffero_counters_srv]},
        %% external API
        {proffero_counters_ext_api,
         {proffero_counters_ext_api, start_link, []},
         permanent, 100, worker,
         [proffero_counters_ext_api,
          proffero_counters_ext_api_tcp_connection]}
       ]}}.
