-module(rebar3_gen).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, gen).
-define(DEPS, [default, app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 gen"},
        {opts, []},
        {short_desc, "A rebar plugin"},
        {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
        undefined ->
            rebar_state:project_apps(State);
        AppInfo ->
            [AppInfo]
    end,
    Generators = rebar_state:get(gen, State, []),
    lists:foreach(fun generate/1, Generators),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%
generate({Output, Generators}) when is_list(Generators) ->
    rebar_api:debug("Generating ~ts", [Output]),
    {ok, File} =  file:open(Output, [write]),
    try 
        lists:foreach(fun(Gen) ->
            file:write(File, process(Gen))
        end, Generators),
    after 
        ok = file:close(File)
    end.
generate({Output, Gen}) ->
    generate({Output, [Gen]}).

process({Uri, Transform, Template}) ->
    Data0 = fetch(Uri),
    Data1 = transform(Data0, Transform),
    template(Data1, Template);
process({Uri, Template}) ->
    process({Uri, [], Template}).

fetch(Url) ->
    case httpc:request(get, {Url, [{"User-Agent", rebar_utils:user_agent()}]},
                       [{ssl, rebar_api:ssl_opts(Url)}, {relaxed, true} | rebar_utils:get_proxy_auth()],
                       [{body_format, binary}],
                       rebar) of
        {ok, {{_Version, 200, _Reason}, Headers, Body}} ->
            rebar_api:debug("Successfully downloaded ~ts", [Url]),
            {ok, Body};
        {ok, {{_Version, Code, _Reason}, _Headers, _Body}} ->
            rebar_api:debug("Request to ~p failed: status code ~p", [Url, Code]),
            erlang:error({http, Code});
        {error, Reason} ->
            rebar_api:debug("Request to ~p failed: ~p", [Url, Reason]),
            erlang:error({http, Code});
    end.

transform(Data, {M, F, A}) -> 
    erlang:apply(M, F, A ++ [Data]);
transform(Data, List) when is_list(List) -> 
    lists:foldl(fun({M, F, A}, Acc) ->
        erlang:apply(M, F, A ++ Acc)
    end, Data, List);
transform(Data, []) ->
    Data;
transform(_, _) -> 
    error({badarg, transform}).

template(Data, {src, File})->
    Priv = code:priv_dir(cryptoea),
    Template = bbmustache:parse_file(File),
    bbmustache:compile(Template, Data, [{key_type, binary}]);
template(Data, {src, File})->
    Template = bbmustache:parse_file(File),
    compile(Data, Template);
template(Data, {file, File})->
    Template = bbmustache:parse_file(File),
    compile(Data, Template).

compile(Data, Template) ->
    bbmustache:compile(Template, Data, [{key_type, binary}]).
% path({priv, Filename}) ->
%     Priv = code:priv_dir(cryptoea),
%     filename:join([Priv, Filename]);
% path(Filename) when is_list(Filename); is_atom(Filename); is_binary(Filename) ->
%     Filename;
% path(Other) ->
%     error({badarg, Other}).

