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
-module(comparison_bandit).
-author('barcojie@gmail.com').

%%% -------------------------------------------------------------------------
%%% This is an agent using Reinforcement Comparison method to solve bandit  
%%% problem which is introduced in chapter 2.8 
%%% of the book <Reinforcement Learning: An Introduction>
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
%% Function: run/3
%% Purpose: To start the simulation of the agent.
%% Args:  N is an integer specifying the number optional actions.
%% 		  Runs is an integer indicating the number rounds of task
%%        Beta is an number between 0 and 1 as the step-size parameter
%%    	  Filename a string denotes the file to be generated
%%
%% Returns: ok or {error, Reason}
%%          If ok, there would be a side effect - a data file recording
%% 			the task number and corresponding average reward and the
%% 			percentage of the optimal actions taken, which is used to
%% 			generate the figure by gnuplot
%%----------------------------------------------------------------------
run(N, Runs, Beta, Filename) when is_integer(N), is_integer(Runs), is_list(Filename) ->
	State = init(N, Beta, Filename),
	go(State, Runs);

run(N, Runs, Beta, Filename) ->
	io:format("Wrong arguments: N ~p, Runs ~p, Beta ~p, Filename ~p~n", [N, Runs, Beta, Filename]),
	{error, wrong_arguments}.

policy(#state{action_prob=Prob}) ->
	Action = action_with_prob(Prob),
	{ok, Action};
policy(_State) ->
	{error, wrong_state}.

reward(State, Action) ->
	lists:nth(Action, State#state.rqt).

model(State, {Action, Reward}) ->
	{Qt, Act_Prob} = update(State, Action, Reward),
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
			action_prob = Act_Prob,
			play = NewPlay,
			reward = NewReward,
			percent = NewPercent}.

value(State) ->
	State.

%%% =========================================================================  
%%% Internal Functions
%%% ========================================================================= 
init(N, Beta, Filename) ->
	{ok, FileIo} = file:open(Filename, [write]),
	Gauss = gauss_gen:start(),
	#state{qt = [0 || _ <- lists:seq(1,N)], %Pt
		rqt = [gauss_gen:get_number(Gauss,0,1) || _ <- lists:seq(1,N)],
		action_prob = [1/N || _ <- lists:seq(1,N)], % hold the probabilities of selecting action on the t-th play
		reward = 0.0,
		percent = 0.0,
		file = FileIo,
		env = Gauss,
		param = Beta}.

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
	update(State#state.qt, Action, Reward, State#state.param, State#state.reward, [], 1).

update([], _Action, _Reward, _Beta, _Average, Qt, _N) ->
	Qqt = lists:reverse(Qt),
	Exp_Qqt = [math:exp(X) || X <- Qqt],
	{Qqt, [X/lists:sum(Exp_Qqt) || X <- Exp_Qqt]};
update([QtH|QtT], Action, Reward, Beta, Average, Qt, N) ->
	if
		N == Action ->
			update(QtT, Action, Reward, Beta, Average, [QtH+Beta*(Reward-Average)|Qt], N+1);
		true ->
			update(QtT, Action, Reward, Beta, Average, [QtH|Qt], N+1)
	end.

action_with_prob(Prob) ->
	action_with_prob(Prob, lists:sort(Prob)).

action_with_prob(Prob, [T|[]]) ->
	string:str(Prob, [T]);
action_with_prob(Prob, [H|T] = S) ->
	case random:uniform() < H/lists:sum(S) of
		true ->
			string:str(Prob, [H]);
		false ->
			action_with_prob(Prob, T)
	end.
