-module(test_drv).
-include("test_drv.hrl").

-export([start/0, stop/0, init/0]).

% unit tests for erlang function bindings
-export([test/0]).

% port driver function interface
-export([sleep_test1/1, sleep_test2/1, integer_test1/0, char_test1/1,
         char_test2/1, float_test1/0, pchar_test1/1, time_test1/1,
         float_test2/1, integer_test2/4, integer_test3/4]).

sleep_test1(Arg1) when is_integer(Arg1) ->
    call_port([<<?FUNCTION_sleep_test1?uint8_t,
                 Arg1?uint32_t>>]).
sleep_test2(Arg1) when is_integer(Arg1) ->
    call_port([<<?FUNCTION_sleep_test2?uint8_t,
                 Arg1?uint32_t>>]).
integer_test1() ->
    call_port([<<?FUNCTION_integer_test1?uint8_t>>]).
char_test1(Arg1) when is_integer(Arg1) ->
    call_port([<<?FUNCTION_char_test1?uint8_t,
                 Arg1?int8_t>>]).
char_test2(Arg1) when is_integer(Arg1) ->
    call_port([<<?FUNCTION_char_test2?uint8_t,
                 Arg1?uint8_t>>]).
float_test1() ->
    call_port([<<?FUNCTION_float_test1?uint8_t>>]).
pchar_test1(Arg1) when is_list(Arg1) ->
    call_port([<<?FUNCTION_pchar_test1?uint8_t>>,
                 encode(Arg1)]).
time_test1(Arg1) when is_integer(Arg1) ->
    call_port([<<?FUNCTION_time_test1?uint8_t,
                 Arg1?int32_t>>]).
float_test2(Arg1) when is_float(Arg1) ->
    call_port([<<?FUNCTION_float_test2?uint8_t,
                 Arg1?double>>]).
integer_test2(Arg1, Arg2, Arg3, Arg4) when is_integer(Arg1),
                                           is_integer(Arg2),
                                           is_integer(Arg3),
                                           is_integer(Arg4) ->
    call_port([<<?FUNCTION_integer_test2?uint8_t,
                 Arg1?int8_t,
                 Arg2?int16_t,
                 Arg3?int32_t,
                 Arg4?int64_t>>]).
integer_test3(Arg1, Arg2, Arg3, Arg4) when is_integer(Arg1),
                                           is_integer(Arg2),
                                           is_integer(Arg3),
                                           is_integer(Arg4) ->
    call_port([<<?FUNCTION_integer_test3?uint8_t,
                 Arg1?uint8_t,
                 Arg2?uint16_t,
                 Arg3?uint32_t,
                 Arg4?uint64_t>>]).

test() ->
    io:format("sync ", []),
    ok = test_drv:sleep_test1(1),
    io:format("async ", []),
    ok = test_drv:sleep_test2(1),
    io:format("sync ", []),
    {ok, 18446744073709551615} = test_drv:integer_test1(),
    io:format("sync ", []),
    {ok,  -1} = test_drv:char_test1(-1),
    io:format("sync ", []),
    {ok, 255} = test_drv:char_test2(-1),
    io:format("sync ", []),
    {ok, F1} = test_drv:float_test1(),
    true = F1 < 3.0e-8,
    io:format("sync ", []),
    {ok, <<"foobar is great">>} = test_drv:pchar_test1("foobar is great"),
    io:format("sync ", []),
    {ok, <<"Mon Jan 18 19:14:07 2038\n">>} = test_drv:time_test1(2147483647),
    io:format("sync ", []),
    {ok, <<"Sat Feb  6 22:28:15 2106\n">>} = test_drv:time_test1(4294967295),
    io:format("sync ", []),
    {ok, F2} = test_drv:float_test2(0.3333333333333333),
    true = F2 > 0.3333333333333333,
    io:format("sync ", []),
    {ok, 377} = test_drv:integer_test2(55, 89, 377, -144),
    io:format("sync ", []),
    {ok, 610} = test_drv:integer_test3(34, 55, 144, 377),
    ok.

encode(Value) when is_list(Value) ->
    Data = list_to_binary(Value),
    DataSize = size(Data),
    <<DataSize?uint32_t, Data/binary>>.

start() ->
    {ok, Path} = load_path(?PORT_DRIVER_NAME ++ ".so"),
    case erl_ddll:load_driver(Path, ?PORT_DRIVER_NAME) of
        ok ->
            ok;
        {error, already_loaded} ->
            ok;
        {error, bad_driver_name} ->
            % the driver name specified when loading does not match
            % the driver name specified within the C driver (ErlDrvEntry)
            exit({error, bad_driver_name});
        {error, {open_error, -10}} ->
            % driver does not exist in the path specified...
            % or is unable to be read...
            % or can not be loaded because all the
            %     required symbols are not present...
            % etc.
            exit({error, {open_error, -10}});
        {error, Reason} ->
            exit({error, Reason})
    end,
    spawn(?MODULE, init, []).

init() ->
    register(test_drv, self()),
    Port = case open_port({spawn, ?PORT_DRIVER_NAME}, []) of
        P when port(P) ->
            P;
        Error ->
            exit({error, Error})
    end,
    loop(Port).

stop() ->
    test_drv ! stop.

call_port(Msg) ->
    test_drv ! {call, self(), Msg},
    receive
        {test_drv, Result} ->
            Result
    end.

loop(Port) when port(Port) ->
    receive
        {call, Caller, Msg} ->
            Result = case catch erlang:port_command(Port, Msg) of
                true -> 
                    io:format("call", []),
                    receive
                        {Port, ok} ->
                            io:format(" done~n", []),
                            ok;
                        {Port, ok, M} ->
                            io:format(" done~n", []),
                            {ok, M};
                        {Port, error, Reason} ->
                            {error, Reason}
                    end;
                Err  ->
                    {error, Err}
            end,
            Caller ! {test_drv, Result},
            loop(Port);
        stop ->
            try erlang:port_close(Port) of
                true ->
                    exit(normal)
            catch
                exit:Reason ->
                    exit({error, Reason});
                error:Reason ->
                    exit({error, Reason})
            end;
        {'EXIT', _, Reason} ->
            exit({error, Reason})
    end.

load_path(File) ->
    case lists:filter(fun(D) ->
                              case file:read_file_info(D ++ "/" ++ File) of
                                  {ok, _} -> true;
                                  _ -> false
                              end
                      end, code:get_path()) of
        [Dir|_] ->
            {ok, Dir};
        [] ->
            io:format("~s not found in code path~n", [File]),
            {error, enoent}
    end.

