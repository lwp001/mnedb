-module(mnedb_app).

-behaviour(application).

-export([
		 start/2,
		 stop/1
		]).

-export([
		 start/0
		]).



start(_Type, _StartArgs) ->
	mnedb_sup:start_link().
	
stop(_State) ->
	ok.
	
	
start() ->
	ok = application:start(mnesia),
	ok = application:start(mnedb),
	ok.