-module(learnkred).
-export([start/0]).

start() ->
  otp_analysis:start("otp_analysis.cfg"),

  build_edges(),
  ok.

%% @doc Extracts app list, and module list for each app, filters out modules
%% which we don't want (eunit and ct suites, Klarna upgrade files), writes
%% edges.json with per-module callee list.
build_edges() ->
  Apps = otp_analysis:apps(),
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

  Calls0 = [otp_analysis:module_to_module(M) || M <- Mods],
  Calls1 = {struct, [
                     {atom_to_binary(K, latin1),
                      V}
                      %%lists:map(fun(V0) ->
                      %%              erlang:atom_to_binary(V0, latin1)
                      %%          end, V)}
                     || {K, V} <- Calls0
                    ]
           },
  Calls = {struct, [ {<<"connections">>, Calls1}
                   , {<<"applications">>, AppModMap}
                   ]
          },
  J = mochijson2:encode(Calls),

  file:write_file("input.json", J),
  io:format("*** build_edges done.~n", []).
