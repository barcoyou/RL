%% @author Barco You <barcojie@gmail.com>
%% This source is not limited by any license.
%%
%% This module simulates the Gridworld Exmaple 3.8 and Figure 3.5
%% in the book: <Reinforcement Learning: An Introduction>, and
%% supposing it an episodic task.
-module(gridworld).
-author('barcojie@gmail.com').

-define(REWARD1, -1).
-define(REWARD2, 0).
-define(REWARD3, 10).
-define(REWARD4, 5).
-define(ROWS, 5).
-define(COLUMNS, 5).
-define(GAMA, 0.9).
-define(DELTA, 0.000001).

-export([run/0,
	run_optimal/0]).

%%%
%%% API
%%% 

%%
%% @doc Computes the state-value of the grid points
%% from index [1,1] to [5,5]
run() ->
	%One dimensional list representing the state values
	make_matrix(state_value([0.0 || _X <-lists:seq(1,?ROWS), _Y <-lists:seq(1,?COLUMNS)])).

%%
%% @doc Computes the optimal state-value of the grid
%% points from index [1,1] to [5,5]
run_optimal() ->
	%One dimensional list representing the state values
	make_matrix(optimal_state_value([0.0 || _X <-lists:seq(1,?ROWS), _Y <-lists:seq(1,?COLUMNS)])).

%%% 
%%% Internal Function
%%% 
state_value(Next) ->
	Current = [bellman([X, Y], Next) || X <-lists:seq(1,?ROWS), Y <-lists:seq(1,?COLUMNS)],
	Delta = delta(Current, Next),
	if
		Delta < ?DELTA ->
			Current;
		true ->
			state_value(Current)
	end.

optimal_state_value(Next) ->
	Current = [optimal_bellman([X,Y], Next) || X <-lists:seq(1,?ROWS), Y <- lists:seq(1,?COLUMNS)],
	{Value, _} = lists:unzip(Current),
	case delta(Value, Next)<?DELTA of
		true ->
			Current;
		false ->
			optimal_state_value(Value)
	end.

bellman(State, Next) ->
	lists:foldl(fun(A, Acc1) ->
				Acc1+policy(State, A)*lists:foldl(fun(Sp, Acc2) ->
							Acc2+transition_prob(State, Sp, A)*(reward(State, Sp, A)+?GAMA*get_state_var(Sp, Next))
					end,
					0,
					transition(State, A))
		end,
		0,
		action_set(State)).

optimal_bellman(State, Next) ->
	L = [lists:foldl(fun(Sp, Acc) ->
					Acc+transition_prob(State, Sp, A)*(reward(State, Sp, A)+?GAMA*get_state_var(Sp, Next))
			end,
			0,
			transition(State, A)) || A <- action_set(State)],
	Max = lists:max(L),
	{Max, max_actions(Max, L, action_set(State))}.


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

delta(L1, L2) ->
	delta(L1, L2, 0).
delta([], [], Sum) ->
	Sum;
delta([H1|T1], [H2|T2], Sum) ->
	delta(T1, T2, Sum+abs(H1-H2)).

make_matrix(List) ->
	Fun = fun([X,Y]) ->
			lists:nth(?ROWS*(X-1)+Y, List)
	end,
	matrix:gen([?ROWS, ?COLUMNS], Fun).

get_state_var([R, C], VarL) ->
	lists:nth(?ROWS*(R-1)+C, VarL).

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
