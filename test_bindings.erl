%%% -*- Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab:

%%% GENERIC ERLANG PORT [DRIVER] VERSION 0.6

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
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

-record(state, {last_port_name,
                port = undefined}).

test() ->
    io:format("sync sleep~n", []),
    {ok, ok} = test_bindings:sleep_test1(2),
    io:format("async sleep~n", []),
    case test_bindings:sleep_test2(2) of
        ok -> ok; % async port driver call
        {ok, ok} -> ok % sync port call
    end,
    io:format("...~n", []),
    {ok, 18446744073709551615} = test_bindings:integer_test1(),
    {ok,  -1} = test_bindings:char_test1(-1),
    {ok, 255} = test_bindings:char_test2(-1),
    {ok, F1} = test_bindings:float_test1(),
    true = F1 < 3.0e-8,
    {ok, "foobar is great"} = test_bindings:pchar_test1("foobar is great"),
    {ok, "Mon Jan 18 19:14:07 2038\n"} = test_bindings:time_test1(2147483647),
    {ok, "Sat Feb  6 22:28:15 2106\n"} = test_bindings:time_test1(4294967295),
    {ok, F2} = test_bindings:float_test2(0.3333333333333333),
    true = F2 > 0.3333333333333333,
    {ok, 377} = test_bindings:integer_test2(55, 89, 377, -144),
    {ok, 610} = test_bindings:integer_test3(34, 55, 144, 377),
    {ok, "abcdefghijklmnopqrstuvwxyz"} = 
        test_bindings:pchar_test2("abcdefgh", $i, 
                                  "jklmnop", $q, 
                                  "rstuvwxy", $z),
    {error, "Invalid function call"} = erroneous_call(),
    ok.

% async thread usage in port driver makes the driver permanent
% such that the driver can not be upgraded/downgraded during runtime
% http://www.trapexit.org/forum/viewtopic.php?t=14295&sid=5de3c47e32136682080511d8d95d458e
%
% example:
%> F = test_bindings:do_code_change_function().
%> F(Pid, test_bindings_new, "_vsn-1", {}).
%
%do_code_change_function() ->
%    fun (Pid, NewModule, OldVsn, Extra) ->
%        c:l(NewModule),
%        sys:suspend(Pid),
%        sys:change_code(Pid, NewModule, OldVsn, Extra),
%        sys:resume(Pid),
%        code:purge(NewModule)
%    end.

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
            {ok, #state{last_port_name = Name,
                        port = Port}};
        {error, Reason} ->
            {stop, Reason}
    end.

%%% synchronous port/port_driver calls should be used for short-lived 
%%%     function calls where efficiency is not important
%%% asynchronous port driver calls should be used for long-lived
%%%     function calls where efficiency is important

%% handle synchronous function calls on the port/port_driver
handle_call({call, Msg}, _, #state{port = Port} = State) when is_port(Port),
                                                              is_list(Msg) ->
    case call_port(Port, Msg) of
        ok ->
            receive
                % port exited with a fatal signal
                {Port, {exit_status, Status}} when Status > 128 ->
                    io:format("~nexit with signal ~p~n", [Status - 128]),
                    {reply, {error, Status}, State#state{port = undefined}};
                % port exited with a fatal error
                {Port, {exit_status, Status}} ->
                    io:format("~nexit with error ~p~n", [Status]),
                    {reply, {error, Status}, State#state{port = undefined}};
                % port/port_driver response
                {Port, {data, Data}} ->
                    case transform_data(Data) of
                        {error, Reason} ->
                            {reply, {error, Reason}, State};
                        Result ->
                            {reply, {ok, Result}, State}
                    end
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(Request, _, State) ->
    io:format("Unknown call \"~p\"~n", [Request]),
    {stop, "Unknown call", State}.

%% handle asynchronous function calls on the port driver
handle_cast({call, Msg}, #state{port = Port} = State) when is_port(Port) ->
    case call_port(Port, Msg) of
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
handle_info({Port, {exit_status, Status}}, State) when is_port(Port) ->
    io:format("async function call returned: ~p~n", [{exit_status, Status}]),
    {noreply, State#state{port = undefined}};

%% port/port_driver non-fatal error
handle_info({Port, {data, {error, Reason}}}, State) when is_port(Port) ->
    io:format("async function call returned: ~p~n", [{error, Reason}]),
    {noreply, State};

%% port/port_driver response
handle_info({Port, {data, Data}}, State) when is_port(Port) ->
    io:format("async function call returned: ~p~n", [Data]),
    {noreply, State};

handle_info({'EXIT', Port, Reason}, 
            #state{port = Port} = State) when is_port(Port) ->
    io:format("Port terminated: ~s~n", [Reason]),
    {stop, "Port terminated", State#state{port = undefined}};

handle_info(Request, State) ->
    io:format("Unknown info \"~p\"~n", [Request]),
    {noreply, State}.

terminate(_, #state{port = Port}) when is_port(Port) ->
    catch erlang:port_close(Port),
    ok;

terminate(_, _) ->
    ok.

% problem mentioned here:
% http://www.trapexit.org/forum/viewtopic.php?t=14295&sid=5de3c47e32136682080511d8d95d458e
%code_change({down, Vsn}, State, Extra) ->
%    code_change(Vsn, State, Extra);
%code_change(Vsn, #state{last_port_name = Name,
%                        port = Port} = State, _) when is_port(Port) ->
%    % XXX Does not work because of the automatic driver_lock_driver() call
%    % in erts/emulator/beam/erl_async.c at line 143
%    %
%    % all driver_async() calls will trigger making the linked in driver
%    % permanent, probably to avoid the possibility of trying to
%    % upgrade/downgrade a driver while async threads are still in use.
%    catch erlang:port_close(Port),
%    Result = case unload_local_port(Name) of
%        ok ->
%            NewName = local_port_name_prefix() ++ Vsn,
%            case load_local_port(NewName) of
%                {ok, Port} ->
%                    {ok, #state{last_port_name = NewName,
%                                port = Port}};
%                {error, Reason} ->
%                    {error, Reason}
%            end;
%        {error, Reason} ->
%            {error, Reason}
%    end,
%    case Result of
%        {ok, NewState} ->
%            {ok, NewState};
%        {error, R} ->
%            io:format("error in code_change/3: ~p~n", [R]),
%            {ok, State#state{port = undefined}}
%    end;

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
    {ok, Path} = load_path(Name ++ ".so"),
    Result = case erl_ddll:load_driver(Path, Name) of
        ok -> ok;
        {error, already_loaded} -> ok;
        % {open_error, -10}, otherwise known as "Unknown dlload error",
        % could mean any of the following:
        % - driver does not exist in the path specified
        % - driver can not be read
        % - driver does not have all the required symbols
        % etc.
        {error, ErrorDesc} -> {error, erl_ddll:format_error(ErrorDesc)}
    end,
    case Result of
        ok ->
            case erlang:open_port({spawn, Name}, []) of
                P when port(P) ->
                    {ok, P};
                Error ->
                    {error, Error}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
transform_data(D) ->
    D.
%% only a port driver can perform asynchronous function calls
%% (you might be able to put a thread pool in an Erlang port, but why bother?)
call_port_async(Msg) when is_list(Msg) ->
    gen_server:cast(?MODULE, {call, Msg}).
-else.
-ifdef(ERL_PORT_NAME).
local_port_name() ->
    ?ERL_PORT_NAME.
%local_port_name_prefix() ->
%    ?ERL_PORT_NAME_PREFIX.
%unload_local_port(Name) when is_list(Name) ->
%    ok.
load_local_port(Name) when is_list(Name) ->
    {ok, Path} = load_path(Name),
    case erlang:open_port({spawn, Path ++ "/" ++ Name},
                          [{packet, 2}, binary, exit_status, use_stdio]) of
        P when port(P) ->
            {ok, P};
        Error ->
            {error, Error}
    end.
transform_data(D) ->
    erlang:binary_to_term(D).
-else.
local_port_name() ->
    "".
%local_port_name_prefix() ->
%    "".
%unload_local_port(_) ->
%    ok.
load_local_port(_) ->
    {error, "Neither ERL_PORT_DRIVER_NAME nor ERL_PORT_NAME defined"}.
transform_data(D) ->
    D.
-endif.
-endif.

call_port_sync(Msg) when is_list(Msg) ->
    gen_server:call(?MODULE, {call, Msg}).

call_port(Port, Msg) when is_port(Port), is_list(Msg) ->
    try erlang:port_command(Port, Msg) of
        true -> ok
    catch
        error:badarg ->
            try erlang:iolist_size(Msg) of
                _ -> {error, einval}
            catch
                error:_ -> {error, badarg}
            end;
        error:Reason -> {error, Reason}
    end.

load_path(File) when is_list(File) ->
    case lists:filter(fun(D) ->
                              case file:read_file_info(D ++ "/" ++ File) of
                                  {ok, _} -> true;
                                  _ -> false
                              end
                      end, code:get_path()) of
        [Dir|_] ->
            {ok, Dir};
        [] ->
            {error, enoent}
    end.

erroneous_call() ->
    call_port_sync([<<32767:16/unsigned-integer-native>>]).

