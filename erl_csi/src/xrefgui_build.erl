-module(xrefgui_build).
-export([start/1]).

%%-----------------------------------------------------------------------------
start( [CfgPath] ) ->
  io:format("*** input from cfg ~p~n", [CfgPath]),
  Config = otp_analysis:start(atom_to_list(CfgPath)),
  build_edges(Config),
  ok.

%%-----------------------------------------------------------------------------
%% @doc Extracts app list, and module list for each app, filters out modules
%% which we don't want (eunit and ct suites, Klarna upgrade files), writes
%% edges.json with per-module callee list.
build_edges(Config) ->
  Apps = otp_analysis:apps(),
  io:format("*** apps found: ~p~n", [Apps]),
  Mods0 = lists:flatten([otp_analysis:app_modules(A) || A <- Apps]),
  AppModMap = [{A, otp_analysis:app_modules(A)} || A <- Apps],

  %% filter out tests and UP's
  Mods = lists:filter(fun(M) ->
                          B = atom_to_binary(M, latin1),
                          case binary:match(B, [ <<"_UP">>, <<"_SUITE">>
                                               , <<"_test">>
                                               ]) of
                            nomatch -> true;
                            _ -> false
                          end
                      end, Mods0),
  io:format("*** mods found: ~p~n", [Apps]),

  ModCalls = mod_level_calls(Config, Apps, Mods),
  %%FunCalls = fun_level_calls(Apps, orddict:new()),
  FunCalls = {struct, npmap:npmap(8, fun fun_level_calls/1, Apps)},
  Calls = {struct, [ {<<"connections">>, ModCalls}
                   , {<<"applications">>, AppModMap}
                   , {<<"calls">>, FunCalls}
                   ]
          },
  %%io:format("*** ~p calls found~n", [length(lists:flatten(ModCalls))]),
  J = mochijson2:encode(Calls),

  file:write_file("input.json", J),
  io:format("*** build_edges done.~n", []).

%%-----------------------------------------------------------------------------
mod_level_calls(Config, _Apps, Mods) ->
  Calls0 = [otp_analysis:module_to_module(Config, M) || M <- Mods],
  Calls1 = {struct, [
                     {atom_to_binary(K, latin1),
                      V}
                     || {K, V} <- Calls0
                    ]
           },
  Calls1.

%%-----------------------------------------------------------------------------
mfa_to_key({M, F, A}) ->
  iolist_to_binary(io_lib:format("~s:~s/~B", [M, F, A])).

%%-----------------------------------------------------------------------------
%% TODO: paralellize me!
%fun_level_calls([], Accum) -> Accum;
%fun_level_calls([App|Apps], Accum) ->
fun_level_calls(App) ->
  Accum = orddict:new(),
  {ok, Calls0} = xref:q(otp_analysis, "E ||| " ++ atom_to_list(App)),
  %% for each pair of MFA in Calls0, create dict of lists defining call graph
  FoldF = fun({MFA1, MFA2}, IAccum) ->
                  K1 = mfa_to_key(MFA1),
                  K2 = mfa_to_key(MFA2),
                  case orddict:find(K1, IAccum) of
                    {ok, Value} ->
                      orddict:store(K1, [K2|Value], IAccum);
                    error ->
                      orddict:store(K1, [K2], IAccum)
                  end
          end,
  Accum2 = lists:foldl(FoldF, Accum, Calls0),
  io:format("...output ~B fun calls for app '~s'~n", [length(Calls0), App]),
  {App, {struct, Accum2}}.
  %fun_level_calls(Apps, Accum2).

%%-----------------------------------------------------------------------------
