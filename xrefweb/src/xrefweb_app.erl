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
  mcweb_sup:start_link().

stop(_State) ->
  ok.

%% @doc TODO: move this out of macaba_app
start_web() ->
  ok = xrefweb:ensure_started(crypto),
  ok = xrefweb:ensure_started(ranch),
  ok = xrefweb:ensure_started(cowboy),

  Site = macaba_board:get_site_config(),
  Disp = cowboy_compile_dispatch(Site#mcb_site_config.offline),
  cowboy_start_listener(Disp).

%%%-----------------------------------------------------------------------------
cowboy_start_listener(Disp) ->
  {ok, HttpPort} = macaba_conf:get_or_fatal([<<"html">>, <<"listen_port">>]),
  {ok, Listeners} = macaba_conf:get_or_fatal([<<"html">>, <<"listeners">>]),
  cowboy:start_http(?XREFWEB_LISTENER, Listeners,
                    [{port, HttpPort}, {ip, {0,0,0,0}}],
                    [{env, [{dispatch, Disp}]}]
                   ).

%%%-----------------------------------------------------------------------------
change_offline_mode(Offline) ->
  Disp = cowboy_compile_dispatch(Offline),
  cowboy:set_env(?XREFWEB_LISTENER, dispatch, Disp).

%%%-----------------------------------------------------------------------------
cowboy_compile_dispatch(Offline) ->
  Priv = code:priv_dir(mcweb),
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
  HMod = mcweb_html_public,
  Index    = {"/", HMod, [index]},
  TNew     = {"/board/:mcb_board/thread/new", HMod, [thread_new]},
  TManage  = {"/board/:mcb_board/thread/:mcb_thread/manage", HMod,
              [thread_manage]},
  TShow    = {"/board/:mcb_board/thread/:mcb_thread", HMod, [thread]},
  TRepl    = {"/board/:mcb_board/post/new", HMod, [post_new]},
  BShow1   = {"/board/:mcb_board/page/:mcb_page", HMod, [board]},
  BShow2   = {"/board/:mcb_board", HMod, [board]},
  AttThumb = {"/attach/:mcb_attach/thumb", HMod, [attach_thumb]},
  Attach   = {"/attach/:mcb_attach", HMod, [attach]},
  UPvw     = {"/util/preview", HMod, [util_preview]},

  %%--- admin resources ---
  AMod = mcweb_html_admin,
  ASiteB  = {"/admin/site/boards", AMod, [admin_site_boards]},
  ASiteO  = {"/admin/site/offline", AMod, [admin_site_offline]},
  ASite   = {"/admin/site", AMod, [admin_site]},
  ALogin  = {"/admin/login", AMod, [admin_login]},
  ALogout = {"/admin/logout", AMod, [admin_logout]},
  ALanding= {"/admin", AMod, [admin]},

  case Offline of
    false ->
      %% board is online, bring everything up!
      BoardResources = [ AttThumb, Attach ]
        ++ rest_routing_table()
        ++ [ TNew, TManage, TShow, TRepl
           , BShow1, BShow2
           , UPvw, Index
           ],
      CatchAll = [];
    true ->
      %% board is offline, shut everything down except admin pages!
      BoardResources = [],
      CatchAll = [{'_', AMod, [offline]}]
  end,
  cowboy_router:compile(
    [ {'_',
       %% static always works in online and offline
       [ St1, St2, St3 ]
       ++ BoardResources
       ++ [ ASiteB, ASiteO, ASite, ALogin, ALogout, ALanding ]
       ++ CatchAll
      } ]).

%%%-----------------------------------------------------------------------------
%% @doc Build routing table piece for REST resources
rest_routing_table() ->
  [ {"/rest/board/:mcb_board/thread/:mcb_thread", mcweb_rest_thread, []}
  , {"/rest/board/:mcb_board/post/:mcb_post", mcweb_rest_post, []}
  , {"/rest/post/preview", mcweb_rest_post, []}
  ].
