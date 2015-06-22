-module(mnedb_sup).

-behaviour(supervisor).

-export([
		 init/1
		]).

-export([
		 start_link/0
		]).

-define(CHILD(Id, Type),	{Id, {Id, start_link, []}, permanent, 2000, Type, [Id]}).



start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, null).
	

init(_Arg) ->
	MnedbChild = ?CHILD(mnedb_srv, worker),
	{ok, {{one_for_one, 3, 10}, [MnedbChild]}}.