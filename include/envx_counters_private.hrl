%%%----------------------------------------------------------------------
%%% File        : envx_counters_private.hrl
%%% Author      : Aleksey Morarash <aleksey.morarash@envisionx.co>
%%% Description : envx_counters private definitions file
%%% Created     : 29 Aug 2014
%%%----------------------------------------------------------------------

-ifndef(_PROFFERO_COUNTERS_PRIVATE).
-define(_PROFFERO_COUNTERS_PRIVATE, true).

%% ----------------------------------------------------------------------
%% Configuration keys

-define(CFG_TCP_BIND_PORT, [envx_counters, tcp_bind_port]).
-define(CFG_UDP_BIND_PORT, [envx_counters, udp_bind_port]).

%% ----------------------------------------------------------------------
%% Default values for configuration keys

-define(DEFAULT_TCP_BIND_PORT, 8907).
-define(DEFAULT_UDP_BIND_PORT, 8907).

%% ----------------------------------------------------------------------
%% eunit

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-endif.
