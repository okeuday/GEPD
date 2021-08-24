%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% GENERIC ERLANG PORT [DRIVER]
%%% automatically create Erlang bindings to C++/C that requires an OS process
%%%
%%% MIT License
%%%
%%% Copyright (c) 2009-2021 Michael Truog <mjtruog at protonmail dot com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(test_bindings).

-behavior(gen_server).

%% external interface
-export([test/0]).

%% gen_server interface
-export([start_link/0, start/0]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-include("erlang_functions.hrl").

% testing port driver by default, uncomment to test port
-undef(ERL_PORT_DRIVER_NAME).

-type client() :: {pid(), any()}.
-record(state,
    {
        file_name :: string(),
        port :: port(),
        replies = [] :: list({non_neg_integer(), client()})
    }).

test() ->
    io:format("sync sleep~n", []),
    {ok, ok} = test_bindings:sleep_test1(?MODULE, 2),
    io:format("async sleep~n", []),
    case test_bindings:sleep_test2(?MODULE, 2) of
        ok -> ok; % async port driver call
        {ok, ok} -> ok % sync port call
    end,
    io:format("...~n", []),
    {ok, 18446744073709551615} = test_bindings:integer_test1(?MODULE),
    {ok, CharTest1} = test_bindings:char_test1(?MODULE, -1),
    true = (CharTest1 =:= -1) orelse (CharTest1 =:= 255), % standard ambiguous
    {ok, 255} = test_bindings:char_test2(?MODULE, -1),
    {ok, F1} = test_bindings:float_test1(?MODULE),
    true = F1 < 3.0e-8,
    {ok, "foobar is great"} =
        test_bindings:pchar_test1(?MODULE, "foobar is great"),
    {ok, "Mon Jan 18 19:14:07 2038\n"} =
        test_bindings:time_test1(?MODULE, 2147483647),
    {ok, "Sat Feb  6 22:28:15 2106\n"} =
        test_bindings:time_test1(?MODULE, 4294967295),
    {ok, F2} = test_bindings:float_test2(?MODULE, 0.3333333333333333),
    true = F2 > 0.3333333333333333,
    {ok, 377} = test_bindings:integer_test2(?MODULE, 55, 89, 377, -144),
    {ok, 610} = test_bindings:integer_test3(?MODULE, 34, 55, 144, 377),
    {ok, "abcdefghijklmnopqrstuvwxyz"} = 
        test_bindings:pchar_test2(?MODULE,
                                  "abcdefgh", $i, 
                                  "jklmnop", $q, 
                                  "rstuvwxy", $z),
    {ok, <<"Hello World!">>} = test_bindings:hello_test1(?MODULE),
    {error, "Invalid function call"} = erroneous_call(?MODULE),
    ok.

%%%------------------------------------------------------------------------
%%% Interface functions from gen_server
%%%------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([]) ->
    % port/port_driver find/load/open
    erlang:process_flag(trap_exit, true),
    Name = local_port_name(),
    case load_local_port(Name) of
        {ok, Port} when is_port(Port) ->
            {ok, #state{file_name = Name,
                        port = Port}};
        {error, Reason} ->
            {stop, Reason}
    end.

%% handle synchronous function calls on the port/port_driver
handle_call({call, Command, Message}, Client,
            #state{port = Port,
                   replies = Replies} = State) ->
    case call_port(Port, Message) of
        ok ->
            {noreply, State#state{replies = Replies ++ [{Command, Client}]}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call(Request, _, State) ->
    io:format("Unknown call \"~p\"~n", [Request]),
    {stop, "Unknown call", State}.

%% handle asynchronous function calls on the port driver
handle_cast({call, _, Message},
            #state{port = Port} = State) ->
    case call_port(Port, Message) of
        ok ->
            ok;
        {error, Reason} ->
            io:format("error ~p~n", [Reason]),
            ok
    end,
    {noreply, State};

handle_cast(Request, State) ->
    io:format("Unknown cast \"~p\"~n", [Request]),
    {noreply, State}.

%% port exited with a fatal error/signal
handle_info({Port, {exit_status, Status}},
            #state{port = Port} = State) ->
    {stop, "port exited with " ++ exit_status_to_string(Status), State};

%% port/port_driver sync response
handle_info({Port, {data, Data}},
            #state{port = Port} = State) ->
    case transform_data(Data) of
        {Command, Success} ->
            call_port_reply(Command, {ok, Success}, State);
        {error, 0, Reason} ->
            {stop, Reason, State};
        {error, Command, Reason} ->
            call_port_reply(Command, {error, Reason}, State);
        {Stream, OsPid, Output} when Stream == stdout; Stream == stderr ->
            FormattedOutput = lists:flatmap(fun(Line) ->
                io_lib:format("    ~s~n", [Line])
            end, string:tokens(Output, "\n")),
            io:format("~w (pid ~w):~n~s", [Stream, OsPid, FormattedOutput]),
            {noreply, State}
    end;

%% port_driver async response
handle_info({Port, {async, RawData}},
            #state{port = Port} = State) ->
    Data = case transform_data(RawData) of
        {error, _, Reason} ->
            {error, Reason};
        {_, Success} ->
            {ok, Success}
    end,
    io:format("async function call returned: ~p~n", [Data]),
    {noreply, State};

%% something unexpected happened when sending to the port
handle_info({'EXIT', Port, PosixCode},
            #state{port = Port} = State) ->
    {stop, port_exit_to_string(PosixCode), State};

handle_info(Request, State) ->
    io:format("Unknown info \"~p\"~n", [Request]),
    {noreply, State}.

terminate(_, #state{port = Port}) ->
    _ = (catch erlang:port_close(Port)),
    ok;

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-ifdef(ERL_PORT_DRIVER_NAME).
local_port_name() ->
    ?ERL_PORT_DRIVER_NAME.
%local_port_name_prefix() ->
%    ?ERL_PORT_DRIVER_NAME_PREFIX.
%unload_local_port(Name) when is_list(Name) ->
%    % assume only one process owns the driver
%    % (otherwise more complex logic is necessary)
%    case erl_ddll:try_unload(Name, []) of
%        {ok, unloaded} ->
%            ok;
%        {ok, Error} when is_atom(Error) ->
%            {error, atom_to_list(Error)};
%        {error, Error} when is_atom(Error) ->
%            {error, atom_to_list(Error)}
%    end.
load_local_port(Name) when is_list(Name) ->
    io:format("using port driver~n", []),
    {ok, Path} = load_path(Name ++ ".so"),
    Result = case erl_ddll:load_driver(Path, Name) of
        ok ->
            ok;
        {error, already_loaded} ->
            ok;
        {error, ErrorDesc} ->
            {error, erl_ddll:format_error(ErrorDesc)}
    end,
    if
        Result =:= ok ->
            {ok, erlang:open_port({spawn, Name}, [])};
        true ->
            Result
    end.
transform_data(D) ->
    D.
%% only a port driver can perform asynchronous function calls
%% (you might be able to put a thread pool in an Erlang port, but why bother?)
call_port_async(Process, Command, Message)
    when is_integer(Command), is_list(Message) ->
    gen_server:cast(Process, {call, Command, Message}).
-else.
-ifdef(ERL_PORT_NAME).
local_port_name() ->
    ?ERL_PORT_NAME.
%local_port_name_prefix() ->
%    ?ERL_PORT_NAME_PREFIX.
%unload_local_port(Name) when is_list(Name) ->
%    ok.
load_local_port(Name) when is_list(Name) ->
    io:format("using port~n", []),
    {ok, Path} = load_path(Name),
    P = erlang:open_port({spawn, Path ++ "/" ++ Name},
                         [{packet, 4}, binary, exit_status, nouse_stdio]),
    {ok, P}.
transform_data(D) ->
    erlang:binary_to_term(D).
call_port_async(Process, Command, Message) ->
    call_port_sync(Process, Command, Message).
-endif.
-endif.

call_port_sync(Process, Command, Message)
    when is_integer(Command), is_list(Message) ->
    gen_server:call(Process, {call, Command, Message}).

call_port(Port, Message) when is_port(Port), is_list(Message) ->
    try erlang:port_command(Port, Message) of
        true ->
            ok
    catch
        error:_ ->
            {error, badarg}
    end.

call_port_reply(Command, Reply,
                #state{replies = Replies} = State) ->
    case lists:keytake(Command, 1, Replies) of
        false ->
            {stop, "invalid reply", State};
        {value, {Command, Client}, RepliesNew} ->
            _ = gen_server:reply(Client, Reply),
            {noreply, State#state{replies = RepliesNew}}
    end.

load_path(File) when is_list(File) ->
    Paths = lists:dropwhile(fun(D) ->
        case file:read_file_info(filename:join(D, File)) of
            {ok, _} -> false;
            _ -> true
        end
    end, code:get_path()),
    case Paths of
        [Dir | _] ->
            {ok, Dir};
        [] ->
            {error, enoent}
    end.

erroneous_call(Process) ->
    call_port_sync(Process, 32767, [<<32767:16/unsigned-integer-native>>]).

port_exit_to_string(eacces) ->
    "permission denied";
port_exit_to_string(eagain) ->
    "resource temporarily unavailable";
port_exit_to_string(ebadf) ->
    "bad file number";
port_exit_to_string(ebusy) ->
    "file busy";
port_exit_to_string(edquot) ->
    "disk quota exceeded";
port_exit_to_string(eexist) ->
    "file already exists";
port_exit_to_string(efault) ->
    "bad address in system call argument";
port_exit_to_string(efbig) ->
    "file too large";
port_exit_to_string(eintr) ->
    "interrupted system call";
port_exit_to_string(einval) ->
    "invalid argument";
port_exit_to_string(eio) ->
    "IO error";
port_exit_to_string(eisdir) ->
    "illegal operation on a directory";
port_exit_to_string(eloop) ->
    "too many levels of symbolic links";
port_exit_to_string(emfile) ->
    "too many open files";
port_exit_to_string(emlink) ->
    "too many links";
port_exit_to_string(enametoolong) ->
    "file name too long";
port_exit_to_string(enfile) ->
    "file table overflow";
port_exit_to_string(enodev) ->
    "no such device";
port_exit_to_string(enoent) ->
    "no such file or directory";
port_exit_to_string(enomem) ->
    "not enough memory";
port_exit_to_string(enospc) ->
    "no space left on device";
port_exit_to_string(enotblk) ->
    "block device required";
port_exit_to_string(enotdir) ->
    "not a directory";
port_exit_to_string(enotsup) ->
    "operation not supported";
port_exit_to_string(enxio) ->
    "no such device or address";
port_exit_to_string(eperm) ->
    "not owner";
port_exit_to_string(epipe) ->
    "broken pipe";
port_exit_to_string(erofs) ->
    "read-only file system";
port_exit_to_string(espipe) ->
    "invalid seek";
port_exit_to_string(esrch) ->
    "no such process";
port_exit_to_string(estale) ->
    "stale remote file handle";
port_exit_to_string(exdev) ->
    "cross-domain link";
port_exit_to_string(Other) when is_atom(Other) ->
    erlang:atom_to_list(Other).

%% exit status messages

exit_status_to_string(  0) -> "no error occurred";
%% GEPD exit status values (from InternalExitStatus in port.cpp)
%% exit_status >= GEPD::ExitStatus::errors_min (from port.hpp)
%% (GEPD::ExitStatus::errors_min == 80)
exit_status_to_string( 80) -> "erlang exited";
exit_status_to_string( 81) -> "erlang port read EAGAIN";
exit_status_to_string( 82) -> "erlang port read EBADF";
exit_status_to_string( 83) -> "erlang port read EFAULT";
exit_status_to_string( 84) -> "erlang port read EINTR";
exit_status_to_string( 85) -> "erlang port read EINVAL";
exit_status_to_string( 86) -> "erlang port read EIO";
exit_status_to_string( 87) -> "erlang port read EISDIR";
exit_status_to_string( 88) -> "erlang port read null";
exit_status_to_string( 89) -> "erlang port read overflow";
exit_status_to_string( 90) -> "erlang port read unknown";
exit_status_to_string( 91) -> "erlang port write EAGAIN";
exit_status_to_string( 92) -> "erlang port write EBADF";
exit_status_to_string( 93) -> "erlang port write EFAULT";
exit_status_to_string( 94) -> "erlang port write EFBIG";
exit_status_to_string( 95) -> "erlang port write EINTR";
exit_status_to_string( 96) -> "erlang port write EINVAL";
exit_status_to_string( 97) -> "erlang port write EIO";
exit_status_to_string( 98) -> "erlang port write ENOSPC";
exit_status_to_string( 99) -> "erlang port write EPIPE";
exit_status_to_string(100) -> "erlang port write null";
exit_status_to_string(101) -> "erlang port write overflow";
exit_status_to_string(102) -> "erlang port write unknown";
exit_status_to_string(103) -> "erlang port ei_encode_error";
exit_status_to_string(104) -> "erlang port poll EBADF";
exit_status_to_string(105) -> "erlang port poll EFAULT";
exit_status_to_string(106) -> "erlang port poll EINTR";
exit_status_to_string(107) -> "erlang port poll EINVAL";
exit_status_to_string(108) -> "erlang port poll ENOMEM";
exit_status_to_string(109) -> "erlang port poll ERR";
exit_status_to_string(110) -> "erlang port poll HUP";
exit_status_to_string(111) -> "erlang port poll NVAL";
exit_status_to_string(112) -> "erlang port poll unknown";
exit_status_to_string(113) -> "erlang port pipe EFAULT";
exit_status_to_string(114) -> "erlang port pipe EINVAL";
exit_status_to_string(115) -> "erlang port pipe EMFILE";
exit_status_to_string(116) -> "erlang port pipe ENFILE";
exit_status_to_string(117) -> "erlang port pipe unknown";
exit_status_to_string(118) -> "erlang port dup EBADF";
exit_status_to_string(119) -> "erlang port dup EBUSY";
exit_status_to_string(120) -> "erlang port dup EINTR";
exit_status_to_string(121) -> "erlang port dup EINVAL";
exit_status_to_string(122) -> "erlang port dup EMFILE";
exit_status_to_string(123) -> "erlang port dup unknown";
exit_status_to_string(124) -> "erlang port close EBADF";
exit_status_to_string(125) -> "erlang port close EINTR";
exit_status_to_string(126) -> "erlang port close EIO";
exit_status_to_string(127) -> "erlang port close unknown";
exit_status_to_string(Status) when is_integer(Status) ->
    if
        Status > 128 ->
            %% exit status values due to signals
            signal_to_string(Status - 128);
        true ->
            erlang:integer_to_list(Status)
    end.

% Only signal integers that are consistent among all platforms use a
% specific string.
signal_to_string( 1) -> "SIGHUP";
signal_to_string( 2) -> "SIGINT";
signal_to_string( 3) -> "SIGQUIT";
signal_to_string( 4) -> "SIGILL";
signal_to_string( 5) -> "SIGTRAP";
signal_to_string( 6) -> "SIGABRT";
signal_to_string( 8) -> "SIGFPE";
signal_to_string( 9) -> "SIGKILL";
signal_to_string(11) -> "SIGSEGV";
signal_to_string(13) -> "SIGPIPE";
signal_to_string(14) -> "SIGALRM";
signal_to_string(15) -> "SIGTERM";
signal_to_string(Signal) when is_integer(Signal), Signal > 0 ->
    "SIG#" ++ erlang:integer_to_list(Signal).

