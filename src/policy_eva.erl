%% @author Barco You <barcojie@gmail.com>
%% This source is NOT limited by any license.
%%
%% This module simulates the Exmaple 4.1 and Figure 4.2
%% in the book: <Reinforcement Learning: An Introduction>
-module(policy_eva).
-author('barcojie@gmail.com').

-define(REWARD, -1).
-define(FINAL, 0).
-define(ROWS, 4).
-define(COLUMNS, 4).
-define(GAMA, 1.0).
-define(DELTA, 0.000001).

-behaviour(gen_fsm).

-export([start/0,
		run/1,
		pause/0,
		stop/0]).
-export([init/1,
		paused/2,
		evaluate/2,
		handle_event/3,
		handle_sync_event/4,
		code_change/4,
		terminate/3,
		handle_info/3]).



%%%
%%% API
%%% 
start() ->
	gen_fsm:start_link({local, ?MODULE}, ?MODULE, 0, []). 

run(Policy) ->
	gen_fsm:send_event(?MODULE, {evaluate, Policy}).

pause() ->
	gen_fsm:send_event(?MODULE, pause).

stop() ->
	gen_fsm:send_event(?MODULE, stop).
%%% 
%%% Callbacks
%%% 
init(N) ->
	{ok, paused, {0, no_policy, [{N, action_set([X,Y])} || X <- lists:seq(1,?ROWS), Y <- lists:seq(1,?COLUMNS)]}}.

paused(stop, StateData={Count, Policy, Value}) ->
	io:format("Stopped at Iteration: ~w, with Policy: ~w, and State-Values:~n~p~n",[Count, Policy, make_matrix(Value)]),
	{stop, normal, StateData};
paused({evaluate, Policy}, {Count, OldPolicy, Value}) ->
	io:format("Previous evaluation at Iteration: ~w, with Policy: ~w, and State-Values:~n~p~n",[Count, OldPolicy, make_matrix(Value)]),
	{next_state, evaluate, {0, Policy, Value}, 0}.

evaluate(timeout, {Count, Policy, Value}) ->
	Current = [bellman([X,Y], Value, choose_pi(Policy)) || X<-lists:seq(1,?ROWS), Y<-lists:seq(1,?COLUMNS)],
	io:format("Evaluation at Iteration: ~w, with Policy: ~w, and State-Values:~n~p~n",[Count, Policy, make_matrix(Current)]),
	case delta(Current, Value)<?DELTA of
		true ->
			io:format("Evaluation Converges at Iteration: ~w, with Policy: ~w, and State-Values:~n~p~n",[Count, Policy, make_matrix(Current)]),
			{next_state, paused, {Count, Policy, Current}};
		false ->
			{next_state, evaluate, {Count+1, Policy, Current}, 1000}
	end;
evaluate(pause, {Count, Policy, Value}) ->
	io:format("Pause at Iteration: ~w, with Policy: ~w, and State-Values:~n~p~n",[Count, Policy, make_matrix(Value)]),
	{next_state, paused, {Count, Policy, Value}}.

handle_event(cancel, StateName, StateData) ->
	notice("CANCEL", StateName),
	{stop, cancelled, StateData};
handle_event(Event, StateName, StateData) ->
	unexpected(Event, StateName),
	{next_state, StateName, StateData}.

handle_sync_event(cancel, _From, StateName, StateData) ->
	notice("CANCEL", StateName),
	{stop, cancelled, ok, StateData};
handle_sync_event(Event, _From, StateName, StateData) ->
	unexpected(Event, StateName),
	{next_state, StateName, StateData}.

code_change(_OldeVsn, StateName, Data, _Extra) ->
	{ok, StateName, Data}.

terminate(normal, StateName, StateData) ->
	{stop, StateName, StateData};
terminate(_Reason, _StateName, _StateData) ->
	ok.

handle_info(Info, StateName, StateData) ->
	unexpected(Info, StateName),
	{next_state, StateName, StateData}.
%%% 
%%% Internal Function
%%% 
bellman(State, Next, Pi) ->
	StateValue = lists:foldl(fun(A, Acc1) ->
				Acc1+Pi(State, A, Next)*lists:foldl(fun(Sp, Acc2) ->
							Acc2+transition_prob(State, Sp, A)*(reward(State, Sp, A)+?GAMA*get_state_var(Sp, Next))
					end,
					0,
					transition(State, A))
		end,
		0,
		action_set(State)),
	L = [lists:foldl(fun(Sp, Acc) ->
					Acc+transition_prob(State, Sp, A)*(reward(State, Sp, A)+?GAMA*get_state_var(Sp, Next))
			end,
			0,
			transition(State, A)) || A <- action_set(State)],
	Max = lists:max(L),
	{StateValue, max_actions(Max, L, action_set(State))}.


action_set(_State) ->
	[left, right, up, down].

equiprob_policy(_State, _Action, _Value) ->
	1/4.

optimal_policy(State, Action, Value) ->
	MaxActions = get_state_maxact(State, Value),
	case lists:member(Action, MaxActions) of
		true ->
			1/length(MaxActions);
		false ->
			0
	end.
%%
%% @spec transition(State::list(), A::atom()) -> States::list()
%% @doc State-Action pair -> list of states
transition([1,1], _A) -> %absorbing state
	[[1,1]];
transition([4,4], _A) -> %absorbing state
	[[4,4]];
transition([H,T], A) ->
	case A of
		left ->
			if
				T == 1 ->
					[[H, T]];
				true ->
					[[H, T-1]]
			end;
		right ->
			if
				T == ?COLUMNS ->
					[[H, T]];
				true ->
					[[H, T+1]]
			end;
		up ->
			if
				H == 1 ->
					[[H, T]];
				true ->
					[[H-1, T]]
			end;
		down ->
			if
				H == ?ROWS ->
					[[H, T]];
				true ->
					[[H+1, T]]
			end
	end.

transition_prob(_S, _Sp, _A) ->
	1.0.

reward([1,1], _Sp, _A) ->
	?FINAL;
reward([?ROWS,?COLUMNS], _Sp, _A) ->
	?FINAL;
reward(_S, _Sp, _A) ->
	?REWARD.

delta(L1, L2) ->
	delta(L1, L2, 0).
delta([], [], Sum) ->
	Sum;
delta([{H1V,_}|T1], [{H2V,_}|T2], Sum) ->
	delta(T1, T2, Sum+abs(H1V-H2V)).

make_matrix(List) ->
	Fun = fun([X,Y]) ->
			lists:nth(?ROWS*(X-1)+Y, List)
	end,
	matrix:gen([?ROWS, ?COLUMNS], Fun).

get_state_var([R, C], VarL) ->
	case lists:nth(?ROWS*(R-1)+C, VarL) of
		{V, _} ->
			V;
		Value ->
			Value
	end.

get_state_maxact([R,C], VarL) ->
	case lists:nth(?ROWS*(R-1)+C, VarL) of
		{_V, Acts} ->
			Acts;
		_ ->
			action_set([R,C])
	end.

max_actions(Max, Values, Actions) ->
	max_actions(Max, Values, Actions, []).
max_actions(_, [], [], L) ->
	L;
max_actions(Max, [Vh|Vt], [Ah|At], L) ->
	if
		Max == Vh ->
			max_actions(Max, Vt, At, [Ah|L]);
		true ->
			max_actions(Max, Vt, At, L)
	end.

choose_pi(Policy) ->
	case Policy of
		equi_prob ->
			fun equiprob_policy/3;
		optimal ->
			fun optimal_policy/3
	end.

notice(Msg, StateName) ->
	io:format("Received "++Msg++" at state: ~w", [StateName]).


unexpected(Event, StateName) ->
	io:format("Received unexpected event: ~p, at state: ~w", [Event, StateName]).
