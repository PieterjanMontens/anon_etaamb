-module(server).

%% Simple server module, to provide a REST API to the old
%% anonymisation components.

-export([start/1, dispatch/1, stop/0]).

start(Options) ->
    mochiweb_http:start([{name, ?MODULE}, {loop, {?MODULE, dispatch}} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).

dispatch(Req) ->
    case mochiweb_request:get(method, Req) of
        'GET' ->
            get_resource(Req);
        'POST' ->
            post_resource(Req);
        _ -> 
            io:format("Received unsupported query~n"),
            Headers = [{"Allow", "GET"}],
            mochiweb_request:respond({405, Headers,
                "405 Method Not Allowed\r\n"},
               Req)
    end.

get_resource(Req) ->
    "/" ++ Path = mochiweb_request:get(path, Req),
    Qs = get_qs(Req),
    case Path of
        "status" -> 
            mochiweb_request:ok({"text/html", "Nominal"}, Req);
        "check" ->
            Lang = case proplists:get_value("lang", Qs) of
                    "dutch" -> dutch;
                     _ -> french
                   end,
            Word = proplists:get_value("word", Qs),
            Score = io_lib:format("~B", [anoner:check(Lang,Word)]),
            mochiweb_request:ok({"text/html", Score}, Req);
        _ -> 
            mochiweb_request:respond({404, [], "404 Not Found\r\n"}, Req)
    end.

post_resource(Req) ->
    "/" ++ Path = mochiweb_request:get(path, Req),
    case Path of
        "sequence_check" ->
            Body = mochiweb_request:recv_body(Req),
            {struct, Data} = mochijson2:decode(Body),
            Lang = case proplists:get_value(<<"lang">>, Data) of
                    <<"dutch">> -> dutch;
                     _ -> french
                   end,
            BinStr = proplists:get_value(<<"string">>, Data),
            Scores = anoner:sequence_check(Lang, binary_to_list(BinStr)),
            mochiweb_request:ok({"text/html", Scores}, Req);
        _ -> 
            mochiweb_request:respond({404, [], "404 Not Found\r\n"}, Req)
    end.
            
get_qs(Req) ->
    RawPath = mochiweb_request:get(raw_path, Req),
    {_,QueryString,_} = mochiweb_util:urlsplit_path(RawPath),                                                                                                       
    mochiweb_util:parse_qs(QueryString).
