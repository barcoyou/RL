%% @author Barco You <barcojie@gmail.com>
%% This source is NOT limited by any license.
%%
%% This module simulates the Exmaple 4.3 and Figure 4.6
%% in the book: <Reinforcement Learning: An Introduction>
-module(gambler).
-author('barcojie@gmail.com').

-define(GOAL, 100).
-define(FLIP, 0.4).
-define(MAXCAP, 99).
-define(DELTA, 0.000001).

-behaviour(gen_fsm).

-export([start/0,
		print/0,
		stop/0]).
-export([init/1,
		sweep/2,
		paused/2,
		handle_event/3,
		handle_sync_event/4,
		code_change/4,
		terminate/3,
		handle_info/3]).
-record(state, {values, policy, count}).


%%%
%%% API
%%% 
start() ->
	gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

print() ->
	gen_fsm:send_event(?MODULE, print).

stop() ->
	gen_fsm:send_event(?MODULE, stop).

%%% 
%%% Callbacks
%%% 
init([]) ->
	{ok, sweep, #state{values=[0 || _X<-lists:seq(1,?MAXCAP)], count=1},0}.

sweep(timeout, Data) ->
	io:format("Sweep: ~w~n", [Data#state.count]),
	MaxActs = [{S, max_action(S,Data#state.values)} || S<-lists:seq(1,?MAXCAP)],
	Policy =
	fun(State) ->
			{_State, Act} = lists:keyfind(State, 1, MaxActs),
			Act
	end,
	Values = full_backup(Policy, Data#state.values),
	case delta(Values, Data#state.values)<?DELTA of
		true ->
			{next_state, paused, Data#state{values = Values, policy=Policy}};
		false ->
			[print_policy(S, Policy) || S<-lists:seq(1,?MAXCAP)],
			io:format("Values:~n~p~n", [Values]),
			{next_state, sweep, Data#state{values=Values,policy=Policy, count=Data#state.count+1}, 0}
	end.

paused(print, Data) ->
	io:format("Final sweep: ~w~n",[Data#state.count]),
	io:format("Policy:~n",[]),
	[print_policy(S, Data#state.policy) || S<-lists:seq(1,?MAXCAP)],
	io:format("Values:~n~p", [Data#state.values]),
	{next_state, paused, Data};
paused(stop, Data) ->
	{stop, normal, Data}.


%%% 
%%% Internal Function
full_backup(Policy, Old) ->
	[q_pi(S, Policy, Old) || S<-lists:seq(1,?MAXCAP)].

q_pi(State, Policy, Values) ->
	Action = Policy(State),
	reward(State, Action, Values).

reward(State, Action, Values) ->
	?FLIP*get_state_var(min(?GOAL,State+Action), Values) + (1-?FLIP)*get_state_var(State-Action, Values).

max_action(State, Values) ->
	Actions = action_set(State),
	L = [reward(State, A, Values) || A <- Actions],
	Max = lists:max(L),
	max_action(Max, L, Actions).

max_action(Max, [Vh|Vt], [Ah|At]) ->
	if
		Max == Vh ->
			Ah;
		true ->
			max_action(Max, Vt, At)
	end.

action_set(State) ->
	lists:seq(0, min(State, ?GOAL-State)).

delta(L1, L2) ->
	delta(L1, L2, 0).
delta([], [], Sum) ->
	Sum;
delta([H1|T1], [H2|T2], Sum) ->
	delta(T1, T2, Sum+abs(H1-H2)).

get_state_var(S, VarL) ->
	case S of
		?GOAL ->
			1;
		0 ->
			0;
		S ->
			lists:nth(S, VarL)
	end.

print_policy(State, Policy) ->
	io:format("~p --> ~w~n", [State, Policy(State)]). 
%%
%% Not important callbacks
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

notice(Msg, StateName) ->
	io:format("Received "++Msg++" at state: ~w", [StateName]).


unexpected(Event, StateName) ->
	io:format("Received unexpected event: ~p, at state: ~w", [Event, StateName]).
