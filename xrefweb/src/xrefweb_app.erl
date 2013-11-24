%%%------------------------------------------------------------------------
%%% @doc Defines start point for application, also starts dependencies if
%%% started from the console, and, sort of, prepared to be started as a
%%% release, but its not tested and not sure why you would want that?
%%% Created: 2013-11-13
%%% @author Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(xrefweb_app).

-behaviour(application).

%% Application callbacks
-export([ start/0
        , start/2
        , stop/1
        , start_web/0
        , change_offline_mode/1
        ]).

-define(XREFWEB_LISTENER, xrefweb_http_listener).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
  xrefweb:ensure_started(sasl),
  xrefweb:ensure_started(gproc),
  lager:start(),
  application:start(xrefweb).

start(_StartType, _StartArgs) ->
  start_web(),
  xrefweb_sup:start_link().

stop(_State) ->
  ok.

start_web() ->
  ok = xrefweb:ensure_started(crypto),
  ok = xrefweb:ensure_started(ranch),
  ok = xrefweb:ensure_started(cowboy),

  Disp = cowboy_compile_dispatch(),
  cowboy_start_listener(Disp).

%%%-----------------------------------------------------------------------------
cowboy_start_listener(Disp) ->
  {ok, HttpPort} = xrefweb_conf:get_or_fatal([<<"html">>, <<"listen_port">>]),
  {ok, Listeners} = xrefweb_conf:get_or_fatal([<<"html">>, <<"listeners">>]),
  cowboy:start_http(?XREFWEB_LISTENER, Listeners,
                    [{port, HttpPort}, {ip, {0,0,0,0}}],
                    [{env, [{dispatch, Disp}]}]
                   ).

%%%-----------------------------------------------------------------------------
cowboy_compile_dispatch() ->
  Priv = code:priv_dir(xrefweb),
  CSSPath = filename:join([Priv, "css"]),
  JSPath  = filename:join([Priv, "js"]),
  ImgPath = filename:join([Priv, "img"]),

  Mime = {mimetypes, [ {<<".css">>, [<<"text/css">>]}
                     , {<<".js">>,  [<<"application/javascript">>]}
                     , {<<".jpg">>,  [<<"image/jpeg">>]}
                     , {<<".png">>,  [<<"image/png">>]}
                     ]},

  %%--- Static resources ---
  SMod = cowboy_static,
  St1 = {"/css/[...]", SMod, [{directory, CSSPath}, Mime]},
  St2 = {"/js/[...]",  SMod, [{directory, JSPath},  Mime]},
  St3 = {"/img/[...]", SMod, [{directory, ImgPath}, Mime]},

  %%--- anonymous/public resources ---
  HMod = xrefweb_html_public,
  Index    = {"/", HMod, [index]},

  {Resources, CatchAll} = %% board is online, bring everything up!
                          { json_api_routing_table() ++ [Index]
                          , []},
  cowboy_router:compile(
    [ {'_',
       %% static always works in online and offline
       [ St1, St2, St3 ] ++ Resources ++ CatchAll
      } ]).

%%%-----------------------------------------------------------------------------
%% @doc Build routing table piece for REST resources
json_api_routing_table() ->
  [ {"/json_dump", xrefweb_html_public, [json_dump]} ].
