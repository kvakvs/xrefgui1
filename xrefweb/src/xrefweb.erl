%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <kvakvs@yandex.ru>
%%% @doc Generic functions and helpers
%%% @end
%%% Created: 2013-11-13
%%%-------------------------------------------------------------------
-module(xrefweb).

%% API
-export([ ensure_started/1
        , fatal/2]).


ensure_started(App) ->
  ensure_started_1(App, 25).

ensure_started_1(_, 0) -> erlang:error({macaba, ensure_started, retries_count});
ensure_started_1(App, Retries) ->
  case application:start(App) of
    {error, {not_started, Dependency}} ->
      ensure_started_1(Dependency, Retries-1),
      ensure_started_1(App, Retries-1);
    _ ->
      ok
  end.

%% @doc ANSI ESCape color codes: reset font color/weight
endfont() -> [27 | "[0m"].
%% @doc ANSI ESCape color codes: font weight
font(bold) -> [27 | "[1;00m"];
font(thin) -> [27 | "[1;01m"].
%% @doc ANSI ESCape color codes: font color
color(black) -> [27 | "[1;30m"];
color(red) -> [27 | "[1;31m"];
color(green) -> [27 | "[1;32m"];
color(yellow) -> [27 | "[1;33m"];
color(blue) -> [27 | "[1;34m"];
color(magenta) -> [27 | "[1;35m"];
color(cyan) -> [27 | "[1;36m"];
color(white) -> [27 | "[1;37m"].

%% @doc Some error has happened, further execution has no sense - suicide
fatal(Msg, Err) ->
  io:format(standard_error, "~s===================================~n"
  "~s~s: ~p~s~n"
  "===================================~s~n",
    [color(white), color(red), Msg, Err, color(white), endfont()]),
  init:stop(),
  error(Err).
