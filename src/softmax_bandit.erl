%% @author Barco You <barcojie@gmail.com>
%% @copyright 2011 Barco You 
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.
-module(softmax_bandit).
-author('barcojie@gmail.com').

%%% -------------------------------------------------------------------------
%%% This is an agent which solves the n-Armed Bandit problem using
%%% the Action-Value methods, and mapping estimated rewards to action
%%% selection with softmax (Boltzmann) method.
%%% This module will simulate the algorithm introduced in chapter 2.3 of the
%%% book <Reinforcement Learning: An Introduction>
%%% -------------------------------------------------------------------------
-behaviour(gen_agent).
-include("bandit.hrl").

%%% -------------------------------------------------------------------------
%%% Exports
%%% -------------------------------------------------------------------------
-export([run/4]).
-export([policy/1,
		reward/2,
		value/1,
		model/2]).

%%% =========================================================================  
%%% API
%%% ========================================================================= 

%%----------------------------------------------------------------------
%% Function: run/4
%% Purpose: To start the simulation of the agent.
%% Args:  N is an integer specifying the number of optional actions.
%% 		  Runs is an integer indicating the number of rounds of task
%% 		  Temp is an float as the temperature for softmax algorithm 
%%             with Boltzmann method.
%%    	  Filename a string denote the file to be generated
%%
%% Returns: ok or {error, Reason}
%%          If ok, there would be a side effect - a data file recording
%% 			the task number and corresponding average reward and the
%% 			percentage of the optimal actions taken, which is used to
%% 			generate the figure by gnuplot
%%----------------------------------------------------------------------
run(N, Runs, Temp, Filename) when is_integer(N), is_integer(Runs) ->
	State = init(N, Temp, Filename),
	go(State, Runs);

run(N, Runs, Temp, Filename) ->
	io:format("Wrong arguments: N ~p, Runs ~p, Temp ~p, Filename ~p~n", [N, Runs, Temp, Filename]),
	{error, wrong_arguments}.

policy(#state{qt = Qt, param = Temp}) ->
	case Temp of
		0 ->
			{ok, string:str(Qt, [lists:max(Qt)])};
		Temp ->
			Exps = [math:exp(X/Temp) || X <- Qt],
			SumExps = lists:sum(Exps),
			Boltzman = [X/SumExps || X <- Exps],
			SortBoltzman = lists:reverse(lists:sort(Boltzman)),
			case catch([case (random:uniform()<X) of
						true -> throw(X);
						false -> ok end || X <- SortBoltzman]) of
					[ok|_] ->
						{ok, string:str(Qt, [lists:max(Qt)])};
					X ->
						{ok, string:str(Boltzman, [X])}
				end
		end;
policy(_State) ->
	{error, wrong_state}.

reward(State, Action) ->
	lists:nth(Action, State#state.rqt).

model(State, {Action, Reward}) ->
	{Qt, Act_Times} = update(State, Action, Reward),
	NewPlay = State#state.play + 1,
	NewReward = (State#state.reward * State#state.play + Reward)/NewPlay,
	NewPercent =
	case lists:max(State#state.rqt) of
		Reward ->
			(State#state.percent * State#state.play + 1) / NewPlay;
		_Other ->
			State#state.percent * State#state.play / NewPlay
	end,
	State#state{qt = Qt,
			rqt = [gauss_gen:get_number(State#state.env,0,1) || _ <- Qt],
			action_times = Act_Times,
			play = NewPlay,
			reward = NewReward,
			percent = NewPercent}.

value(State) ->
	State.

%%% =========================================================================  
%%% Internal Functions
%%% ========================================================================= 
init(N, Temp, Filename) ->
	{ok, FileIo} = file:open(Filename, [write]),
	Gauss = gauss_gen:start(),
	#state{qt = [0 || _ <- lists:seq(1,N)],
		rqt = [gauss_gen:get_number(Gauss,0,1) || _ <- lists:seq(1,N)],
		action_times = [0 || _ <- lists:seq(1,N)],
		reward = 0.0,
		percent =0.0,
		file = FileIo,
		env = Gauss,
		param = Temp}.

go(State, 0) ->
	io:format(State#state.file,"~w    ~f    ~f~n", [State#state.play, State#state.reward, State#state.percent]),
	file:close(State#state.file),
	gauss_gen:stop(State#state.env);
go(State, N) ->
	{ok, Action} = policy(State),
	Reward = reward(State, Action),
	NewState = model(State, {Action, Reward}),
	io:format(NewState#state.file,"~w    ~f    ~f~n", [NewState#state.play, NewState#state.reward, NewState#state.percent]),
	go(NewState, N-1).

update(State, Action, Reward) ->
	update(State#state.qt, State#state.action_times, Action, Reward, [], [], 1).

update([], [], _Action, _Reward, Qt, At, _N) ->
	{lists:reverse(Qt), lists:reverse(At)};
update([QtH|QtT],[ATH|ATT], Action, Reward, Qt, At, N) ->
	if
		N == Action ->
			update(QtT, ATT, Action, Reward, [QtH+(Reward-QtH)/(ATH+1)|Qt],[ATH+1|At], N+1);
		true ->
			update(QtT, ATT, Action, Reward, [QtH|Qt], [ATH|At], N+1)
	end.


