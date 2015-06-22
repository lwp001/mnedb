-module(mnedb_qlc).

-include_lib("stdlib/include/qlc.hrl").

-compile(export_all).



%% 查看表所有记录
table_all(Table) ->
	qlc:q([X || X <- mnesia:table(Table)]).