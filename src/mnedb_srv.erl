-module(mnedb_srv).

-behaviour(gen_server).

-export([
		 start_link/0
		]).

-export([
		 init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 code_change/3,
		 terminate/2
		]).

-export([
		 seq_call/1,
		 seq_cast/1
		]).



start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	
	
init(_Arg) ->
	{ok, null}.
	

handle_call({read, Oid}, _From, State) ->
	{reply, mnedb_api:read_dirty(Oid), State};
handle_call({index_read, Table, Key, Index}, _From, State) ->
	{reply, mnedb_api:read_dirty_index(Table, Key, Index), State};
handle_call(_Msg, _From, State) ->
	{reply, error, State}.
	
handle_cast({write, Rec}, State) ->
	mnedb_api:write_dirty(Rec),
	{noreply, State};
handle_cast({delete, Oid}, State) ->
	mnedb_api:delete_dirty(Oid),
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.
	
handle_info(_Msg, State) ->
	{noreply, State}.
	
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
	
terminate(_Reason, _State) ->
	ok.


seq_call(Msg) ->
	gen_server:call(?MODULE, Msg).
	
seq_cast(Msg) ->
	gen_server:cast(?MODULE, Msg).