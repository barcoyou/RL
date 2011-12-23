%% @author Barco You <barcojie@gmail.com>
%% This source is NOT limited by any license.
%%
%% This module simulates the Exmaple 4.2 and Figure 4.4
%% in the book: <Reinforcement Learning: An Introduction>
-module(car_rental).
-author('barcojie@gmail.com').

-define(CREDIT, 10).
-define(COST, -2).
-define(LAMBDA1_OUT, 3).
-define(LAMBDA2_OUT, 4).
-define(LAMBDA1_IN, 3).
-define(LAMBDA2_IN, 2).
-define(LIMIT, 20).
-define(MAXMOV, 5).
-define(GAMA, 0.9).
-define(DELTA, 0.000001).
-define(MINPROB, 0.01).

-behaviour(gen_fsm).

-export([start/0,
		run/0,
		print/0,
		stop/0]).
-export([init/1,
		policy_eval/2,
		policy_impr/2,
		paused/2,
		handle_event/3,
		handle_sync_event/4,
		code_change/4,
		terminate/3,
		handle_info/3]).
-compile([export_all]).
-record(state, {values, policy}).


%%%
%%% API
%%% 
start() ->
	gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

run() ->
	gen_fsm:send_event(?MODULE, start).

print() ->
	gen_fsm:send_event(?MODULE, print).

stop() ->
	gen_fsm:send_event(?MODULE, stop).

%%% 
%%% Callbacks
%%% 
init([]) ->
	{ok, policy_eval, #state{values=[0 || _X<-lists:seq(0,?LIMIT), _Y<-lists:seq(0,?LIMIT)]}}.

policy_eval(start, Data) ->
	Policy =
	fun(_State) ->
			0
	end,
	Values = full_backup(Policy, Data#state.values),
	io:format("Values:~n~p", [Values]),
	{next_state, policy_eval, Data#state{values=Values,policy=Policy}, 0};
policy_eval(timeout, Data) ->
	Values = full_backup(Data#state.policy, Data#state.values),
	io:format("Evaluating Policy: -------------------------------------~n",[]),
	[print_policy({X,Y}, Data#state.policy) || X<-lists:seq(0,?LIMIT),Y<-lists:seq(0,?LIMIT)],
	io:format("Values:~n~p", [Values]),

	case delta(Values, Data#state.values)<?DELTA of
		true ->
			{next_state, policy_impr, Data#state{values = Values}, 1000};
		false ->
			{next_state, policy_eval, Data#state{values = Values},0}
	end.

policy_impr(timeout, Data) ->
	MaxActs = [{{X,Y}, max_action({X,Y},Data#state.values)} || X<-lists:seq(0,?LIMIT),Y<-lists:seq(0,?LIMIT)],
	Policy =
	fun(State) ->
			{_State, Act} = lists:keyfind(State, 1, MaxActs),
			Act
	end,
	io:format("New Policy: ^^^^^^^^^^^^^^^^^~n",[]),
	[print_policy({X,Y}, Policy) || X<-lists:seq(0,?LIMIT),Y<-lists:seq(0,?LIMIT)],
	%io:format("Values:~n~p", [Values]),
	case is_policy_same(Policy, Data#state.policy) of
		true ->
			io:format("!!!!! Policy Stable !!!!!~n",[]),
			{next_state, paused, Data};
		false ->
			Values = full_backup(Policy, Data#state.values),
			{next_state, policy_eval,Data#state{values=Values, policy=Policy}, 0}
	end.

paused(print, Data) ->
	io:format("Policy:~n",[]),
	[print_policy({X,Y}, Data#state.policy) || X<-lists:seq(0,?LIMIT),Y<-lists:seq(0,?LIMIT)],
	io:format("Values:~n~p", [Data#state.values]),
	{next_state, paused, Data};
paused(stop, Data) ->
	{stop, normal, Data}.


%%% 
%%% Internal Function
full_backup(Policy, Old) ->
	[q_pi({X,Y}, Policy, Old) || X<-lists:seq(0,?LIMIT), Y<-lists:seq(0,?LIMIT)].

q_pi(State, Policy, Values) ->
	Action = Policy(State),
	reward(State, Action, Values).

reward({A, B}, Action, Values) ->
	lists:sum([reward_event({A,B},Action,Ra,Rb,Da,Db, Values) ||
			Ra<-req_a(), Rb<-req_b(), Da<- drop_a(), Db<-drop_b()]). %only 40 cars in all

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

action_set({A, B}) ->
	lists:seq(-min(5,B),min(5,A)).

transition({A,B}, Action, Ra, Rb, Da, Db) ->
	{min(?LIMIT,Da+(A-Action)-min(A-Action, Ra)), min(?LIMIT,Db+(B+Action)-min(B+Action, Rb))}.

req_a() ->
	max_prob_list(?LAMBDA1_OUT).
req_b() ->
	max_prob_list(?LAMBDA2_OUT).
drop_a() ->
	max_prob_list(?LAMBDA1_IN).
drop_b() ->
	max_prob_list(?LAMBDA2_IN).

max_prob_list(Lambda) ->
	max_prob_list(Lambda, [], 0).
max_prob_list(Lambda, L, N) ->
	case poisson(Lambda, N)<?MINPROB of
		true ->
			L;
		false ->
			max_prob_list(Lambda, [N|L], N+1)
	end.
%% The probability of event with Ra requests at site A, Rb requests at site B,
%% and Da dropoffs at site A, Db dropoffs at site B
prob_event(Ra, Rb, Da, Db) ->
	poisson(?LAMBDA1_OUT,Ra)*poisson(?LAMBDA2_OUT,Rb)*poisson(?LAMBDA1_IN,Da)*poisson(?LAMBDA2_IN,Db).

reward_event({A,B}, Action, Ra, Rb, Da, Db, Values) ->
	prob_event(Ra,Rb,Da,Db)*(?CREDIT*(min(A-Action,Ra)+min(B+Action,Rb))+?COST*abs(Action)+
		?GAMA*get_state_var(transition({A,B},Action,Ra,Rb,Da,Db), Values)).

delta(L1, L2) ->
	delta(L1, L2, 0).
delta([], [], Sum) ->
	Sum;
delta([H1|T1], [H2|T2], Sum) ->
	delta(T1, T2, Sum+abs(H1-H2)).

get_state_var({A, B}, VarL) ->
	lists:nth((?LIMIT+1)*A+B+1, VarL).

poisson(Lambda, N) ->
	math:pow(Lambda, N)*math:exp(-Lambda)/factorial(N).

factorial(N) ->
	factorial(N, 1).
factorial(0, V) ->
	V;
factorial(N, V) ->
	factorial(N-1, N*V).

print_policy(State, Policy) ->
	io:format("~p --> ~w~n", [State, Policy(State)]). 

is_policy_same(P1, P2) ->
	A1 = [P1({X,Y}) || X<-lists:seq(0,?LIMIT), Y<-lists:seq(0,?LIMIT)],
	A2 = [P2({X,Y}) || X<-lists:seq(0,?LIMIT), Y<-lists:seq(0,?LIMIT)],
	if
		A1==A2 ->
			true;
		true ->
			false
	end.

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
