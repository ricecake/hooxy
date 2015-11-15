%%%-------------------------------------------------------------------
%% @doc hooxy public API
%% @end
%%%-------------------------------------------------------------------

-module(hooxy_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	{ok, Port} = application:get_env(hooxy, proxy_port),
	case hooxy_sup:start_link() of
		{ok, Pid} ->
			{ok, _} = vegur:start_http(Port, hooxy_proxy, [
				{middlewares, vegur:default_middlewares()}
			]),
			{ok, Pid}
	end.

%%--------------------------------------------------------------------
stop(_State) ->
	ok.
