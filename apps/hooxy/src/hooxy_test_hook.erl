-module(hooxy_test_hook).
-export([lookup/1]).

lookup(_Domain) ->
	{ok, "127.0.0.1"}.
