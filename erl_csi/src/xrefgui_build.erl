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
  %% Mods = filter_out_tests_and_UPs(Mods0),
  Mods = Mods0,
  
  io:format("*** ~B mods found...~n", [length(Mods)]),

  ModCalls = mod_level_calls(Config, Apps, Mods),

  ets:new(fun_level_calls, [ named_table
                           , bag % not duplicate_bag
                           , {write_concurrency, true}
                           , {read_concurrency, false}
                           ]),
  %%npmap:npmap(8, fun fun_level_calls/1, Apps),
  lists:map(fun fun_level_calls/1, Apps),
  io:format("*** fun map generation complete, dumping ETS table..."),
  FunCalls = load_fun_level_calls_table(ets:first(fun_level_calls), []),

  Calls = {struct, [ {<<"connections">>, ModCalls}
                   , {<<"applications">>, AppModMap}
                   , {<<"calls">>, FunCalls}
                   ]
          },

  io:format("*** encoding JSON...~n"),
  J = mochijson2:encode(Calls),
  
  io:format("*** writing JSON...~n"),
  file:write_file("input.json", J),
  
  io:format("*** build_edges done. Hit ^C,^C to stop erlang shell~n", []).

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
%% @private
%% @doc Removes modules from list whose name ends with _test, _UP or _SUITE
filter_out_tests_and_UPs(Mods0) ->
  lists:filter(fun(M) ->
                       B = atom_to_binary(M, latin1),
                       case binary:match(B, [ <<"_UP">>, <<"_SUITE">>
                                              , <<"_test">>
                                            ]) of
                         nomatch -> true;
                         _ -> false
                       end
               end, Mods0).

%%-----------------------------------------------------------------------------
mfa_to_key({M, F, A}) ->
  iolist_to_binary(io_lib:format("~s:~s/~B", [M, F, A])).

%%-----------------------------------------------------------------------------
%% @private
%% @doc For application reads calls in it from xref and dumps to ets table
fun_level_calls(App) ->
  {ok, Calls0} = xref:q(otp_analysis, "E ||| " ++ atom_to_list(App)),
  io:format("...output ~B fun calls for app '~s'~n", [length(Calls0), App]),
  
  %% for each pair of MFA in Calls0, create dict of lists defining call graph
  FoldF = fun({MFA1, MFA2}) ->
                  K1 = mfa_to_key(MFA1),
                  K2 = mfa_to_key(MFA2),
                  ets:insert_new(fun_level_calls, {K1, K2})
          end,
  lists:map(FoldF, Calls0),  
  ok.

%% @private
%% @doc Reads contents of ets bag table and outputs as unsorted
%% proplist with fun identifiers as keys, and fun identifier lists as values
load_fun_level_calls_table('$end_of_table', A) -> A;
load_fun_level_calls_table(K, A) ->
  Values = ets:lookup(fun_level_calls, K),
  A2 = [{K, Values} | A],
  load_fun_level_calls_table(ets:next(fun_level_calls, K), A2).

%%-----------------------------------------------------------------------------
