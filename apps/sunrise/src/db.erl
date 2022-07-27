-module(db).

-export([start/3, start/4, insert/2, insert/3, lookup/3, lookup/2, update/4, update/3, match/2, all/1, all/2, delete/2, delete/3]).

start(mem, Name, Options) ->
    ets:new(Name, Options).
start(disk, Name, _, Path) ->
    dets:open_file(Name, [{file, io_lib:format("~s/~s.data", [Path, Name])}]);
start(both, Name, Options, Path) ->
    start(mem, Name, Options),
    start(disk, Name, Options, Path),
    dets:to_ets(Name, Name).

insert(mem, Table, Fields) ->
    ets:insert(Table, Fields);
insert(disk, Table, Fields) ->
    dets:insert(Table, Fields);
insert(save, Table, Fields) -> 
    insert(mem, Table, Fields),
    insert(disk, Table, Fields).

insert(Table, Fields) ->
    insert(mem, Table, Fields).

lookup(mem, Table, Key) ->
    ets:lookup(Table, Key);
lookup(disk, Table, Key) ->
    dets:lookup(Table, Key).

lookup(Table, Key) ->
    lookup(mem, Table, Key).

update(mem, Table, Key, PosValue) ->
    ets:update_element(Table, Key, PosValue).

update(mem, Table, Fields) ->
    ets:insert(Table, Fields);
update(disk, Table, Fields) ->
    dets:insert(Table, Fields);
update(save, Table, Fields) ->
    update(mem, Table, Fields),
    update(disk, Table, Fields).

%% only supports ets for now
match(Table, Pattern) -> 
    ets:match(Table, Pattern).

all(mem, Table) ->
    ets:tab2list(Table);
all(disk, Table) ->
    dets:tab2list(Table).
all(Table) ->
    all(mem, Table).

delete(mem, Table, Key) ->
    ets:delete(Table, Key);
delete(disk, Table, Key) ->
    dets:delete(Table, Key).
delete(Table, Key) ->
    delete(mem, Table, Key).
