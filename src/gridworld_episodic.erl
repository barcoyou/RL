%% @author Barco You <barcojie@gmail.com>
%% This source is not limited by any license.
%%
%% This module simulates the Gridworld Exmaple 3.8 and Figure 3.5
%% in the book: <Reinforcement Learning: An Introduction>,and 
%% compute the state value function in episodic mode
-module(gridworld_episodic).
-author('barcojie@gmail.com').

-define(REWARD1, -1).
-define(REWARD2, 0).
-define(REWARD3, 10).
-define(REWARD4, 5).
-define(ROWS, 5).
-define(COLUMNS, 5).
-define(GAMA, 0.9).
-define(EPISODES, 10).

-export([run/0,
	run_optimal/0]).

%%%
%%% API
%%% 

%%
%% @doc Computes the state-value of the grid points
%% from index [1,1] to [5,5]
run() ->
	grid([?ROWS, ?COLUMNS], value_fun).

%%
%% @doc Computes the optimal state-value of the grid points
%% from index [1,1] to [5,5]
run_optimal() ->
	grid([?ROWS, ?COLUMNS], optimal_value_fun).

%%% 
%%% Internal Function
%%% 
grid(Dims, Goal) ->
	grid(Dims, [], Goal).
grid([H|[]], Index, Goal) ->
	Bu = fun(X) ->
			state_value(lists:reverse([X|Index]), Goal)
	end,
	init_list(H, Bu, []);
grid([H|T], Index, Goal) ->
	Bu = fun(X) ->
			grid(T, [X|Index], Goal)
	end,
	init_list(H, Bu, []).

init_list(0, _Fun, L) ->
	L;
init_list(N, Fun, L) ->
	init_list(N-1, Fun, [Fun(N)|L]).

state_value(State, Goal) ->
	state_value(State, Goal, ?EPISODES).
state_value(_State, _Goal, 0) ->
	0;
state_value(State, Goal, N) ->
	state_value(State, action_set(State), Goal, N).
state_value(State, Actions, value_fun, N) ->
	lists:foldl(fun(A, Acc1) ->
				Acc1+policy(State, A)*lists:foldl(fun(Sp, Acc2) ->
							Acc2+transition_prob(State, Sp, A)*(reward(State, Sp, A)+?GAMA*state_value(Sp, value_fun, N-1))
					end,
					0,
					transition(State, A))
		end,
		0,
		Actions);
state_value(State, Actions, optimal_value_fun, N) ->
	Q_A = [lists:foldl(fun(Sp, Acc) ->
					Acc+transition_prob(State, Sp, A)*(reward(State, Sp, A)+?GAMA*state_value(Sp, optimal_value_fun, N-1))
			end,
			0,
			transition(State, A)) || A <- Actions],
	if
		N==?EPISODES ->
			{lists:max(Q_A), max_action(lists:max(Q_A), Q_A, Actions)};
		true ->
			lists:max(Q_A)
	end.

action_set(_State) ->
	[left, right, up, down].

policy(_State, left) ->
	1/4;
policy(_State, right) ->
	1/4;
policy(_State, up) ->
	1/4;
policy(_State, down) ->
	1/4;
policy(_State, _Wrong_Action) ->
	0.
%%
%% @spec transition(State::list(), A::atom()) -> States::list()
%% @doc State-Action pair -> list of states
transition([1,2], _A) ->
	[[5, 2]];
transition([1,4], _A) ->
	[[3, 4]];
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
				T == 5 ->
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
				H == 5 ->
					[[H, T]];
				true ->
					[[H+1, T]]
			end
	end.

transition_prob(_S, _Sp, _A) ->
	1.0.

reward([1,2], [5,2], _A) ->
	?REWARD3;
reward([1,4], [3,4], _A) ->
	?REWARD4;
reward([Sh,St], [Sph,Spt], _A) ->
	if
		(Sh==Sph) and (St==Spt) ->
			?REWARD1;
		true ->
			?REWARD2	
	end.

max_action(Max, Q, A) ->
	max_action(Max, Q, A, []).
max_action(_, [], [], L) ->
	L;
max_action(Max, [Qh|Qt], [Ah|At], L) ->
	if
		Max==Qh ->
			max_action(Max, Qt, At, [Ah|L]);
		true ->
			max_action(Max, Qt, At, L)
	end.
