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
-module(instructed_vs_actionvalue).
-author('barcojie@gmail.com').

%%% -------------------------------------------------------------------------
%%% This module simulates the A and B Bandit problem and compares the 
%%% instructed and action-value methods, by generating the Figure 2.3
%%% in the book <Reinforcement Learning: An Introduction>
%%% -------------------------------------------------------------------------
-behaviour(gen_agent).
-include("bandit.hrl").

-define(EPSILON, 0.1).
-define(N, 2).

%%% -------------------------------------------------------------------------
%%% Exports
%%% -------------------------------------------------------------------------
-export([run/3]).
-export([policy/1,
		reward/2,
		value/1,
		model/2]).

%%% =========================================================================  
%%% API
%%% ========================================================================= 

%%----------------------------------------------------------------------
%% Function: run/3
%% Purpose: To start the simulation of the agent.
%% Args:  Runs - an integer indicating the number rounds of task
%%    	  Filename - a string denoting the file to be generated
%%        Prob_Succ - a list of success probabilities of actions
%%
%% Returns: ok or {error, Reason}
%%          If ok, there would be a side effect - a data file recording
%% 			the task number and corresponding average reward and the
%% 			percentage of the optimal actions taken, which is used to
%% 			generate the figure by gnuplot
%%----------------------------------------------------------------------
run(Runs, Filename, Prob_Succ) when is_integer(Runs), is_list(Filename), is_list(Prob_Succ) ->
	State = init(Filename, Prob_Succ),
	go(State, Runs);

run(Runs, Filename, Prob_Succ) ->
	io:format("Wrong arguments: Runs ~p, Filename ~p, Probability_of_Success: ~w~n", [Runs, Filename, Prob_Succ]),
	{error, wrong_arguments}.

policy(#state{qt = Qt, param = Epsi, action_value=[{Act_Instr,Val_Instr}|_]}) ->
	Action_Instr =
	case Val_Instr of
		0.0 ->
			opposite(Act_Instr);
		1.0 ->
			Act_Instr
	end,
	Action_Greedy =
	case Epsi of
		0 ->
			string:str(Qt, [lists:max(Qt)]);
		Epsi ->
			Ran = random:uniform(),
			if
				Ran < Epsi ->
					random:uniform(length(Qt));
				true ->
					string:str(Qt, [lists:max(Qt)])
			end
	end,
	{ok, {Action_Instr, Action_Greedy}};
policy(_State) ->
	{error, wrong_state}.

reward(State, {Act_Intr, Act_Greedy}) ->
	{lists:nth(Act_Intr, State#state.rqt), lists:nth(Act_Greedy, State#state.rqt)}.

model(State, {{Act_Intr, Act_Greedy}, {Rew_Intr, Rew_Greedy}}) ->
	{Qt, Act_Times} = update(State, Act_Greedy, Rew_Greedy),
	NewPlay = State#state.play + 1,
	{Old_Rew_Intr, Old_Rew_Greedy} = State#state.reward,
	NewReward = {(Old_Rew_Intr * State#state.play + Rew_Intr)/NewPlay,
		(Old_Rew_Greedy * State#state.play + Rew_Greedy)/NewPlay},
	{OldPercent_Intr, OldPercent_Greedy} = State#state.percent,
	NewPercent_Intr =
	case lists:max(State#state.rqt) of
		Rew_Intr ->
			(OldPercent_Intr * State#state.play + 1) / NewPlay;
		_Other ->
			OldPercent_Intr * State#state.play / NewPlay
	end,
	NewPercent_Greedy =
	case lists:max(State#state.rqt) of
		Rew_Greedy ->
			(OldPercent_Greedy * State#state.play+1)/NewPlay;
		_ ->
			OldPercent_Greedy * State#state.play/NewPlay
	end,
	State#state{qt = Qt,
		rqt = [case random:uniform()< X of true->1.0; false->0.0 end || X <- State#state.env],
			action_times = Act_Times,
			action_value = [{Act_Intr, Rew_Intr}, {Act_Greedy, Rew_Greedy}],
			play = NewPlay,
			reward = NewReward,
			percent = {NewPercent_Intr, NewPercent_Greedy}}.

value(State) ->
	State.

%%% =========================================================================  
%%% Internal Functions
%%% ========================================================================= 
init(Filename, Prob_Succ) ->
	{ok, FileIo} = file:open(Filename, [write]),
	#state{qt = [0.0 || _ <- Prob_Succ],
		rqt = [case random:uniform() < X of true->1.0; false->0.0 end || X <- Prob_Succ],
		action_times = [0 || _ <- Prob_Succ],
		action_value = [{1,0.0},{1,0.0}], %{'action selected by instruction method', 'action selected by action-value method'}
		reward = {0.0, 0.0},
		percent = {0.0, 0.0},
		file = FileIo,
		env = Prob_Succ,
		param = ?EPSILON}.

go(State, 0) ->
	{Rew_Intr, Rew_Greedy} = State#state.reward,
	{Percent_Intr, Percent_Greedy} = State#state.percent,
	io:format(State#state.file,"~w    ~f    ~f    ~f    ~f~n",
		[State#state.play, Rew_Intr, Rew_Greedy, Percent_Intr, Percent_Greedy]),
	file:close(State#state.file);
go(State, N) ->
	{ok, Action} = policy(State),
	Reward = reward(State, Action),
	NewState = model(State, {Action, Reward}),
	{Rew_Intr, Rew_Greedy} = NewState#state.reward,
	{Percent_Intr, Percent_Greedy} = NewState#state.percent,
	io:format(NewState#state.file,"~w    ~f    ~f    ~f    ~f~n",
		[NewState#state.play, Rew_Intr, Rew_Greedy, Percent_Intr, Percent_Greedy]),
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

opposite(1) -> 2;
opposite(2) -> 1.
