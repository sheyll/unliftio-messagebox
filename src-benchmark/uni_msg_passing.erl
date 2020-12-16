#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname uni_msg_passing -mnesia debug verbose

-module(uni_msg_passing).
-export([main/1,bench/0, bench/3]).

main([]) ->
  bench();
main([Nps, Nms, Ncs]) ->
  Np = list_to_integer(Nps),
  Nm = list_to_integer(Nms),
  Nc = list_to_integer(Ncs),
  bench(Np, Nm, Nc).

mk_msg(I) ->
 { "The not so very very very very very very very very very very very very very very very very very very very very very very very very " ++ integer_to_list(I),
      "large",
      "meeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeessssssssssssssssssssssssssssssssss" ++ integer_to_list(I),
      { "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        "ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
        I,
        123423421111111111111111111123234 * I
      }
    }.
    
consumer(0) ->
  ok;
consumer(WorkLeft) ->
  receive
    _Msg ->
      consumer(WorkLeft - 1)
  end.
  
consumers (Nm, Nc) ->  
 [ spawn (fun() -> consumer(Nm div Nc) end)
 || _ <- lists:seq(1, Nc)].

producer(Cs, Nm) ->
  [C ! mk_msg(I) 
    || I <- lists:seq(1, Nm), C <- Cs ].

producers(Cs, Np, Nm, Nc) ->
  [ monitor(process, 
            spawn(fun() -> 
              producer(Cs, Nm div (Nc * Np)) 
            end))
  || _ <- lists:seq(1, Np)].
  
bench() ->
  bench(1000, 100000, 1).

bench(Np, Nm, Nc) ->
  Before = erlang:system_time(microsecond), 
  Cs = consumers(Nm, Nc),
  MCs = [monitor(process, C) || C <- Cs],
  MPs = producers(Cs, Np, Nm, Nc),
  await_monitors(MCs),
  await_monitors(MPs),
  After = erlang:system_time(microsecond), 
  Dt = After - Before,
  io:format("Passing ~w messages from ~w producers to ~w consumers, took ~ws.~n", 
            [Nm, Np, Nc, Dt / 1000000]).

await_monitors([]) -> ok;
await_monitors([P|Ps]) ->
  receive
    {'DOWN', P, _,_, _} -> ok
  end,
  await_monitors(Ps).
