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
-module(constant_stepsize_parameter).
-author('barcojie@gmail.com').

%%% -------------------------------------------------------------------------
%%% This is a modified greedy_bandit agent which estimates action rewards  
%%% with 'exponential recency-weighted average' introduced in chapter 2.6 
%%% of the book <Reinforcement Learning: An Introduction>
%%% -------------------------------------------------------------------------
-behaviour(gen_agent).
-include("bandit.hrl").

-define(EPSILON, 0).
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
%% Args:  N is an integer specifying the number optional actions.
%% 		  Runs is an integer indicating the number rounds of task
%% 		  Alpha is an float number between 0 and 1 as the constant  
%%             step-size parameter.
%%    	  Filename a string denotes the file to be generated
%%
%% Returns: ok or {error, Reason}
%%          If ok, there would be a side effect - a data file recording
%% 			the task number and corresponding average reward and the
%% 			percentage of the optimal actions taken, which is used to
%% 			generate the figure by gnuplot
%%----------------------------------------------------------------------
run(N, Runs, Alpha, Filename) when is_integer(N), is_integer(Runs), 0 < Alpha, Alpha < 1 ->
	State = init(N, Alpha, Filename),
	go(State, Runs);

run(N, Runs, Epsi, Filename) ->
	io:format("Wrong arguments: N ~p, Runs ~p, Epsi ~p, Filename ~p~n", [N, Runs, Epsi, Filename]),
	{error, wrong_arguments}.

policy(#state{qt = Qt}) ->
	case ?EPSILON of
		0 ->
			{ok, string:str(Qt, [lists:max(Qt)])};
		Epsi ->
			Ran = random:uniform(),
			if
				Ran < Epsi ->
					{ok, random:uniform(length(Qt))};
				true ->
					{ok, string:str(Qt, [lists:max(Qt)])}
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
init(N, Alpha, Filename) ->
	{ok, FileIo} = file:open(Filename, [write]),
	Gauss = gauss_gen:start(),
	#state{qt = [2 || _ <- lists:seq(1,N)], %optimistic initial value
		rqt = [gauss_gen:get_number(Gauss,0,1) || _ <- lists:seq(1,N)],
		action_times = [0 || _ <- lists:seq(1,N)],
		reward = 0.0,
		percent = 0.0,
		file = FileIo,
		env = Gauss,
		param = Alpha}.

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
	update(State#state.qt, State#state.action_times, Action, Reward, State#state.param, [], [], 1).

update([], [], _Action, _Reward, _Alpha, Qt, At, _N) ->
	{lists:reverse(Qt), lists:reverse(At)};
update([QtH|QtT],[ATH|ATT], Action, Reward, Alpha, Qt, At, N) ->
	if
		N == Action ->
			update(QtT, ATT, Action, Reward, Alpha, [QtH+Alpha*(Reward-QtH)|Qt],[ATH+1|At], N+1);
		true ->
			update(QtT, ATT, Action, Reward, Alpha, [QtH|Qt], [ATH|At], N+1)
	end.


