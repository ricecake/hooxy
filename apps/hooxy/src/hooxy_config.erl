-module(hooxy_config).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([init_tables/0, lookup_domain/1, flush_domain_cache/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

init_tables() ->
	%% initialize our ets table for memory storing our domain cache
	ets:new(?MODULE, [bag, public, named_table, {read_concurrency, true}]),
	ok.

lookup_domain(Domain) when is_list(Domain) ->
	%% always store the domain in binary
	lookup_domain(erlang:list_to_binary(Domain));
lookup_domain(Domain) when is_binary(Domain) ->
	%% do a domain lookup and if we don't have the IP cached in ets, go cache it and return it
	case ets:lookup(?MODULE, Domain) of
		[{Domain, IP, _}] -> {ok, IP};
		[]                -> cache_domain(Domain)
	end.

flush_domain_cache() ->
	%% this syntax is some weirdo stuff that has to be done with ets to do the internal timestamp comparison,
	%% there is a function that will generate this syntax for you in ets:fun2ms (http://www.erlang.org/doc/man/ets.html#fun2ms-1)
	TotalFlushed = ets:select_delete(?MODULE, [{{'$1', '_', '$2'}, [{'>', {const, erlang:timestamp()}, '$2'}],[true]}]),
	{ok, TotalFlushed}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

cache_domain(Domain) ->
	%% decide whether the user wanted to use an Erlang module or an external program to
	%% do the domain to IP translation
	{ok, TTL} = application:get_env(hooxy, ttl),
	case application:get_env(hooxy, hook) of
		{ok, {erlang_hook, Module}}  -> cache_domain(Domain, {erlang_hook, Module, TTL});
		{ok, {extern_hook, Program}} -> cache_domain(Domain, {extern_hook, Program, TTL});
		_ -> throw({error, "hook not properly defined in config"})
	end.
%% the erlang hook must export a (lookup, Domain) function that returns {ok, IP}
cache_domain(Domain, {erlang_hook, Module, TTL}) ->
	{ok, IP} = erlang:apply(Module, lookup, [Domain]),
	%% ensure that we store the IP address as binary
	BinaryIP = case erlang:is_binary(IP) of
		true  -> IP;
		false -> erlang:list_to_binary(IP)
	end,
	%% get a timestamp to store when the cache was taken
	{Mega, Sec, Micro} = erlang:timestamp(),
	%% add the default system TTL to the timestamp to create a timestamp of the time when the cache is invalid
	true = ets:insert(?MODULE, {Domain, BinaryIP, {Mega, Sec + TTL, Micro}}),
	{ok, BinaryIP};
%% the external program must return the IP address with no other formatting
cache_domain(_Domain, {extern_hook, _Program, _TTL}) ->
	%% TODO implement me, this is where a user should be able to define a shell program
	%% or script that will return the IP given the domain
	ok.

%% ------------------------------------------------------------------
%% Unit Tests
%% ------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% run before all other tests to setup mocking
setup_test() ->
	meck:new(application, [unstick]),
	meck:expect(application, get_env, fun(hooxy, Param) ->
		case Param of
			hook -> {ok, {erlang_hook, test_hook}};
			ttl  -> {ok, 1} %% don't increase, we actually wait for this in tests
		end
	end),
	meck:new(test_hook, [non_strict]),
	meck:expect(test_hook, lookup, fun(_) -> {ok, "127.0.0.1"} end),
	ok.

%% function tests
init_tables_test() ->
	ok = init_tables().

lookup_domain_test() ->
	ok = before_each(),
	{ok, <<"127.0.0.1">>} = lookup_domain("example.com"),
	{ok, <<"127.0.0.1">>} = lookup_domain(<<"example.com">>).

flush_domain_cache_test() ->
	ok = before_each(),
	{ok, <<"127.0.0.1">>} = lookup_domain("example.com"),
	{ok, 0}   = flush_domain_cache(),
	{ok, TTL} = application:get_env(hooxy, ttl),
	timer:send_after(timer:seconds(TTL), ready),
	{ok, 1} = receive ready -> flush_domain_cache() end.

%% run at the beginning of each test to clean up the test environment
before_each() ->
	true = ets:delete(?MODULE),
	ok   = hooxy_config:init_tables().

-endif.
