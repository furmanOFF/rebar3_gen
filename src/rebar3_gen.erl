-module(rebar3_gen).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, gen).
-define(DEPS, [app_discovery]).

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
        {short_desc, "Source file generator"},
        {desc, "Convert .gen {{mustache}} files from given URL"}
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
    [begin
        Opts = rebar_app_info:opts(AppInfo),
        SourceDir = filename:join(rebar_app_info:dir(AppInfo), "src"),
        FoundFiles = rebar_utils:find_files(SourceDir, ".*\\.gen\$"),

        CompileFun = fun(Source, Opts1) ->
            gen_compile(Opts1, Source, SourceDir)
        end,
        rebar_base_compiler:run(Opts, [], FoundFiles, CompileFun)
    end || AppInfo <- Apps],
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%
gen_compile(_Opts, Source, OutDir) ->
    Data0 = try_fetch(Source),
    Script = filename:rootname(Source, ".gen") ++ ".script",

    case try_eval(Script, Data0) of
        {ok, Data1} -> 
            Template = bbmustache:parse_file(Source),
            Binary = bbmustache:compile(Template, Data1, [{key_type, binary}]),

            OutFile = filename:join(OutDir, filename:basename(Source, ".gen")),
            filelib:ensure_dir(OutFile),
            rebar_api:info("Writing out ~s", [OutFile]),
            file:write_file(OutFile, Binary);
        Error ->
            Error
    end.

try_fetch(Source) ->
    {ok, F} = file:open(Source, [read]),
    try file:read_line(F) of
        {ok, Line} -> 
            case re:run(Line, <<"^\s*{{!(.+)}}\s*$">>, [{capture, [1], list}]) of
                {match, [Url]} ->
                    download(Url);
                _ ->
                    []
            end;
        _ ->
            []
    after
        ok = file:close(F)
    end.

try_eval(Script, Data) ->
    case filelib:is_regular(Script) of
        true ->
            eval(Script, Data);
        false ->
            {ok, Data}
    end.
%%
download(Url) ->
    rebar_api:info("Downloading ~ts", [Url]),
    case httpc:request(get, {Url, [{"User-Agent", rebar_utils:user_agent()}]},
                       [{ssl, rebar_api:ssl_opts(Url)}, {relaxed, true} | rebar_utils:get_proxy_auth()],
                       [{body_format, binary}],
                       rebar) of
        {ok, {{_Version, 200, _Reason}, Headers, Body}} ->
            rebar_api:debug("Successfully downloaded ~ts", [Url]),
            case lists:keyfind("content-type", 1, Headers) of
                {_, "application/json"} ->
                    jsx:decode(Body, [return_maps]);
                _ ->
                    Body
            end;
        {ok, {{_Version, Code, _Reason}, _Headers, _Body}} ->
            rebar_api:debug("Request to ~ts failed: status code ~p", [Url, Code]),
            erlang:error({http, Code});
        {error, Reason} ->
            rebar_api:debug("Request to ~ts failed: ~p", [Url, Reason]),
            erlang:error({download, Reason})
    end.

eval(Script, Data) ->
    Bindings = erl_eval:add_binding('Data', Data, erl_eval:new_bindings()),
    case file:script(Script, Bindings) of
        {ok, Terms} ->
            {ok, Terms};
        {error, Reason} ->
            rebar_base_compiler:error_tuple(Script, [{Script, [Reason]}], [], [])
    end.
