%% vegur router template for this module: https://github.com/heroku/vegur/blob/master/demo/toy_router.erl
-module(hooxy_proxy).
-behaviour(vegur_interface).
-export([
	init/2,
	terminate/3,
	lookup_domain_name/3,
	checkout_service/3,
	checkin_service/6,
	service_backend/3,
	feature/2,
	additional_headers/4,
	error_page/4
]).

init(ReqTime, Upstream) ->
	{ok, Upstream, #{ initiated => ReqTime, tried => [] }}.

lookup_domain_name(Domain, Upstream, State) ->
	{ok, IP} = hooxy_config:lookup_domain(Domain),
	Servers  = [{IP, 80}],
	{ok, Servers, Upstream, State}.

checkout_service([], Upstream, State) ->
	{error, unhandled_domain, Upstream, State};
checkout_service(Servers, Upstream, #{ tried := Failed } = State) when is_list(Servers) ->
	case Servers -- Failed of
		[Server |_Rest] -> {service, Server, Upstream, State#{ tried := [Server |Failed]}};
		[]              -> {error, {downstream, unavailable}, Upstream, State}
	end.

service_backend({IP, Port}, Upstream, State) ->
	%% extract the IP:PORT from the chosen server.
	{{IP, Port}, Upstream, State}.

checkin_service(_Servers, _Pick, _Phase, _ServState, Upstream, State) ->
	%% if we tracked total connections, we would decrement the counters here
	{ok, Upstream, State}.

feature(_WhoCares, State) ->
	{disabled, State}.

additional_headers(_Direction, _Log, _Upstream, State) ->
	{[], State}.

error_page(unhandled_domain, _DomainGroup, Upstream, HandlerState) ->
	{{404, [], <<>>}, Upstream, HandlerState};
%% Vegur-returned errors that should be handled no matter what. Full list in vegur_stub.erl
error_page({upstream, _Reason}, _DomainGroup, Upstream, HandlerState) ->
	%% Blame the caller
	{{400, [], <<>>}, Upstream, HandlerState};
error_page({downstream, _Reason}, _DomainGroup, Upstream, HandlerState) ->
	%% Blame the server
	{{500, [], <<>>}, Upstream, HandlerState};
error_page({undefined, _Reason}, _DomainGroup, Upstream, HandlerState) ->
	%% Who knows who was to blame!
	{{500, [], <<>>}, Upstream, HandlerState};
%% Specific error codes from middleware
error_page(empty_host, _DomainGroup, Upstream, HandlerState) ->
	{{400, [], <<>>}, Upstream, HandlerState};
error_page(bad_request, _DomainGroup, Upstream, HandlerState) ->
	{{400, [], <<>>}, Upstream, HandlerState};
error_page(expectation_failed, _DomainGroup, Upstream, HandlerState) ->
	{{417, [], <<>>}, Upstream, HandlerState};
%% Catch-all
error_page(_, _DomainGroup, Upstream, HandlerState) ->
	{{500, [], <<>>}, Upstream, HandlerState}.

terminate(_, _, _) ->
	ok.
