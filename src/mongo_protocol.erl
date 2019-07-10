%@doc MongoDB wire protocol
-module(mongo_protocol).

-export_type ([db/0]).
-export_type ([notice/0, request/0, reply/0]).
-export_type ([message/0]).
-export_type ([requestid/0]).

-export ([bit/1, bool/1, dbcoll/2, put_message/3, get_reply/1]).
%-define(DEBUG(F, A), io:format(F++"\n", A)).
-define(DEBUG(_F, _A), "").

-include ("mongo_protocol.hrl").
-include_lib ("bson/include/bson_binary.hrl").
-import (bson_binary, [put_cstring/1, put_document/1, get_document/1]).

-type notice() :: #insert{} | #update{} | #delete{} | #killcursor{}.
% A notice is an asynchronous message sent to the server (no reply expected)

-type request() :: #'query'{} | #getmore{} | #'op_msg'{}.
% A request is a syncronous message sent to the server (reply expected)

-type reply() :: #reply{}.
% A reply to a request

-type requestid() :: integer(). % message id

-define (put_header (Opcode), ?put_int32 (RequestId), ?put_int32 (0), ?put_int32 (Opcode)).
% RequestId expected to be in scope at call site

-define (get_header (Opcode, ResponseTo), ?get_int32 (_RequestId), ?get_int32 (ResponseTo), ?get_int32 (Opcode)).

-define (ReplyOpcode, 1).
-define (UpdateOpcode, 2001).
-define (InsertOpcode, 2002).
-define (QueryOpcode, 2004).
-define (OpMsg, 2013).
-define (GetmoreOpcode, 2005).
-define (DeleteOpcode, 2006).
-define (KillcursorOpcode, 2007).

-spec bit (boolean()) -> 0 | 1.
bit (false) -> 0;
bit (true) -> 1.

-spec bool (0 | 1) -> boolean().
bool (0) -> false;
bool (1) -> true.

-spec dbcoll (db(), collection()) -> bson:utf8().
%@doc Concat db and collection name with period (.) in between
dbcoll (Db, Coll) -> <<(atom_to_binary (Db, utf8)) /binary, $., (atom_to_binary (Coll, utf8)) /binary>>.

-type message() :: notice() | request().

-spec put_message (db(), message(), requestid()) -> binary().
put_message (Db, Message, RequestId) -> case Message of
	#insert {collection = Coll, documents = Docs} -> <<
		?put_header (?InsertOpcode),
		?put_int32 (0),
		(put_cstring (dbcoll (Db, Coll))) /binary,
		<< <<(put_document (Doc)) /binary>> || Doc <- Docs>> /binary >>;
	#update {collection = Coll, upsert = U, multiupdate = M, selector = Sel, updater = Up} -> <<
		?put_header (?UpdateOpcode),
		?put_int32 (0),
		(put_cstring (dbcoll (Db, Coll))) /binary,
		?put_bits32 (0,0,0,0,0,0, bit(M), bit(U)),
		(put_document (Sel)) /binary,
		(put_document (Up)) /binary >>;
	#delete {collection = Coll, singleremove = R, selector = Sel} -> <<
		?put_header (?DeleteOpcode),
		?put_int32 (0),
		(put_cstring (dbcoll (Db, Coll))) /binary,
		?put_bits32 (0,0,0,0,0,0,0, bit(R)),
		(put_document (Sel)) /binary >>;
	#killcursor {cursorids = Cids} -> <<
		?put_header (?KillcursorOpcode),
		?put_int32 (0),
		?put_int32 (length (Cids)),
		<< <<?put_int32 (Cid)>> || Cid <- Cids>> /binary >>;
	#'query' {tailablecursor = TC, slaveok = SOK, nocursortimeout = NCT, awaitdata = AD,
	 collection = Coll, skip = Skip, batchsize = Batch, selector = Sel, projector = Proj} ->
		<<
		?put_header (?QueryOpcode),
		?put_bits32 (0, 0, bit(AD), bit(NCT), 0, bit(SOK), bit(TC), 0),
		(put_cstring (dbcoll (Db, Coll))) /binary,
		?put_int32 (Skip),
		?put_int32 (Batch),
		(put_document (Sel)) /binary,
		(case Proj of [] -> <<>>; _ -> put_document (Proj) end) /binary >>;
	#'op_msg' {tailablecursor = TC, slaveok = SOK, nocursortimeout = NCT, awaitdata = AD,
	collection = Coll, skip = Skip, batchsize = Batch, selector = Sel, projector = _Proj} ->
		Mode = mongodb_util:read_preference(SOK),
		Select = mongodb_util:query_parse(Sel),
		OtherData= ['$db', atom_to_binary(Db, utf8), '$readPreference', {'mode', Mode}],
		Limit = ['limit', Batch],
		SkipNum = ['skip', Skip],
		Data = ['find', atom_to_binary(Coll, utf8)] ++  Select ++ SkipNum ++ Limit ++ OtherData,
		<<
			?put_header (?OpMsg),
			?put_int32 (0),
			?put_int8 (0), (put_document (list_to_tuple(Data))) /binary >>;
	#getmore {collection = Coll, batchsize = Batch, cursorid = Cid, slaveok = SOK} ->
		Mode = mongodb_util:read_preference(SOK),
		Data = {'getMore', Cid, 'collection', atom_to_binary(Coll, utf8), '$db', atom_to_binary(Db, utf8),
			'$readPreference', {'mode', Mode}},
		<<
			?put_header (?OpMsg),
			?put_int32 (0),?put_int8 (0),
			(put_document (Data)) /binary>>
	end.

%%-spec get_reply (binary()) -> {requestid(), reply(), binary()}.
get_reply (<<
	?get_header (?ReplyOpcode, ResponseTo),
	?get_bits32 (_,_,_,_, AwaitCapable, _, QueryError, CursorNotFound),
	?get_int64 (CursorId),
	?get_int32 (StartingFrom),
	?get_int32 (NumDocs),
	Bin /binary >>) ->
		{Docs, BinRest} = get_docs (NumDocs, Bin),
		Reply = #reply {
			cursornotfound = bool (CursorNotFound), queryerror = bool (QueryError), awaitcapable = bool (AwaitCapable),
			cursorid = CursorId, startingfrom = StartingFrom, documents = Docs },
		{ResponseTo, Reply, BinRest};

get_reply (<<
	?get_header (?OpMsg, ResponseTo),
	?get_int32 (StartingFrom),
	?get_int8 (_CursorId),
	Bin /binary >>) ->
	{Docs, BinRest} = get_docs (1, Bin),
	?DEBUG("~p:~p BinRest = ~p, Docs = ~p~n", [?MODULE, ?LINE, BinRest, Docs]),
	{ResponseTo, Docs, <<>>}.
		
get_docs (0, Bin) -> {[], Bin};
get_docs (NumDocs, Bin) when NumDocs > 0 ->
	{Doc, Bin1} = get_document (Bin),
	{Docs, Bin2} = get_docs (NumDocs - 1, Bin1),
	{[Doc | Docs], Bin2}.
