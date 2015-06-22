-module(mnedb_api).

-include_lib("stdlib/include/qlc.hrl").

-export([
		 read_dirty/1,
		 read_dirty_row/1,
		 read_dirty_index/3,
		 read_dirty_index_row/3,
		 
		 read_seq/1,
		 read_seq_row/1,
		 read_seq_index/3,
		 read_seq_index_row/3,
		 
		 read_trans/1,
		 read_trans_row/1,
		 read_trans_index/3,
		 read_trans_index_row/3 
		]).

-export([
		 write_dirty/1,
		 write_seq/1,
		 write_trans/1
		]).

-export([
		 delete_dirty/1,
		 delete_seq/1,
		 delete_trans/1 
		]).

-export([
		 select/1
		]).

-export([
		 batch_operate/3
		]).

-export([
		 add_table_index/2,
		 del_table_index/2
		]).



%%%%%%%%%%%%%%%%%%%% read %%%%%%%%%%%%%%%%%%%%
%% 脏读数据
%% Oid: {table, key}	
read_dirty(Oid) ->
	mnesia:dirty_read(Oid).

%% 脏读一行数据
%% Oid: {table, key}	
read_dirty_row(Oid) ->
	case read_dirty(Oid) of
		[Row|_] ->
			Row;
		[] ->
			null
	end.

%% 脏索引读数据
%% arg: table, key, index	
read_dirty_index(Table, Key, Index) ->
	mnesia:dirty_index_read(Table, Key, Index).

%% 脏索引读取一行数据
%% arg: table, key, index
read_dirty_index_row(Table, Key, Index) ->
	case read_dirty_index(Table, Key, Index) of
		[Row|_] ->
			Row;
		[] ->
			null
	end.


%% 顺序读数据
%% Oid: {table, key}	
read_seq(Oid) ->
	mnedb_srv:seq_call({read, Oid}).

%% 顺序读取一行数据
%% Oid: {table, key}	
read_seq_row(Oid) ->
	case read_seq(Oid) of
		[Row|_] ->
			Row;
		[] ->
			null
	end.

%% 顺序索引读数据
%% arg: table, key, index	
read_seq_index(Table, Key, Index) ->
	mnedb_srv:seq_call({index_read, Table, Key, Index}).

%% 顺序索引读取一行数据
%% arg: table, key, index	
read_seq_index_row(Table, Key, Index) ->
	case read_seq_index(Table, Key, Index) of
		[Row|_] ->
			Row;
		[] ->
			null
	end.


%% 事务读数据
%% Oid: {table, key}	
read_trans(Oid) ->
	Fun = fun() ->
				mnesia:read(Oid)
		  end,
	{atomic, Res} = mnesia:transaction(Fun),
	Res.

%% 事务读一行数据
%% Oid: {table, key}	
read_trans_row(Oid) ->
	case read_trans(Oid) of
		[Row|_] ->
			Row;
		_ ->
			null
	end.

%% 事务索引读数据
%% arg: table, key, index	
read_trans_index(Table, Key, Index) ->
	Fun = fun() ->
				mnesia:index_read(Table, Key, Index)
		  end,
	{atomic, Res} = mnesia:transaction(Fun),
	Res.

%% 事务索引读一行数据
%% arg: table, key, index	
read_trans_index_row(Table, Key, Index) ->
	case read_trans_index(Table, Key, Index) of
		[Row|_] ->
			Row;
		_ ->
			null
	end.
%%%%%%%%%%%%%%%%%%%% read %%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%% write %%%%%%%%%%%%%%%%%%%%
%% 脏写数据
%% Rec: table tuple
write_dirty(Rec) ->
	mnesia:dirty_write(Rec).

%% 顺序写数据
%% Rec: table tuple
write_seq(Rec) ->
	mnedb_srv:seq_cast({write, Rec}).

%% 事务写数据
%% Rec: table tuple
write_trans(Rec) ->
	Fun = fun() ->
				mnesia:write(Rec)
		  end,
	{atomic, Res} = mnesia:transaction(Fun),
	Res.
%%%%%%%%%%%%%%%%%%%% write %%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%% delete %%%%%%%%%%%%%%%%%%%%
%% 脏删除数据
%% Oid: {table, key}
delete_dirty(Oid) ->
	mnesia:dirty_delete(Oid).

%% 顺序删除数据
%% Oid: {table, key}
delete_seq(Oid) ->
	mnedb_srv:seq_cast({delete, Oid}).

%% 事务删除数据
%% Oid: {table, key}
delete_trans(Oid) ->
	Fun = fun() ->
				mnesia:delete(Oid)
		  end,
	{atomic, Res} = mnesia:tranaction(Fun),
	Res.
%%%%%%%%%%%%%%%%%%%% delete %%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%% select %%%%%%%%%%%%%%%%%%%%
%% Qlcq: qlc:q() result
select(Qlcq) ->
	Fun = fun() ->
				qlc:e(Qlcq)
		  end,
	{atomic, Res} = mnesia:transaction(Fun),
	Res.
%%%%%%%%%%%%%%%%%%%% select %%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%% batch operate %%%%%%%%%%%%%%%%%%%%
%% arg: table, fun, state
batch_operate(Table, FoldFun, State) when is_function(FoldFun) ->
	Fun = fun() ->
				mnesia:foldl(FoldFun, State, Table)
		  end,
	{atomic, Res} = mnesia:transaction(Fun),
	Res;
batch_operate(_Table, _FoldFun, _State) ->
	{error,arg_2_not_a_function}.
%%%%%%%%%%%%%%%%%%%% batch operate %%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%% table index %%%%%%%%%%%%%%%%%%%%
%% arg: table, index
add_table_index(Table, Index) ->
	mnesia:add_table_index(Table, Index).
	
%% arg: table, index
del_table_index(Table, Index) ->
	mneisa:del_table_index(Table, Index).
%%%%%%%%%%%%%%%%%%%% table index %%%%%%%%%%%%%%%%%%%%