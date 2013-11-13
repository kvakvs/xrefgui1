%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <kvakvs@yandex.ru>
%%% @doc Generic functions and helpers
%%% @end
%%% Created: 2013-11-13
%%%-------------------------------------------------------------------
-module(xrefweb).

%% API
-export([ensure_started/1]).


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
