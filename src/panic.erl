-module(panic).

-export([initial_call/1]).
-export([find_big_mboxes/1]).
-export([find_large_memory_consumers/1]).


find_big_mboxes(N) ->
    FilterBig =
        fun(Pid) ->
                case process_info(Pid, message_queue_len) of
                    {_, Size} = All when Size > N ->
                        {true, All};
                    _ ->
                        false
                end
        end,
    lists:filtermap(FilterBig, processes()).

find_large_memory_consumers(N) ->
    Pinfo = [{P, recon:info(P, memory)} || P <- processes()],
    Sorter = fun({_, {_, M1}}, {_, {_, M2}}) -> M1 > M2;
                (_, _) -> false
             end,
    lists:seq(lists:sort(Sorter, Pinfo), N).

initial_call(Pid) when is_pid(Pid) ->
    Pinfo = process_info(Pid),
    D = proplists:get_value(dictionary, Pinfo),
    proplists:get_value('$initial_call', D).
