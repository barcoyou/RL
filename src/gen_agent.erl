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
-module(gen_agent).

%%% --------------------------------------------------------------------------
%%%
%%% The idea behind THIS generic agent of reinforcement learning is that
%%% the specific module implements (different) functions for the general
%%% elements of reinforcement learning problem.
%%% 
%%% The user module should export:
%%% 
%%% 	A policy Maps perceived states of the environment to actions when in
%%% 	those states:
%%% 	policy(States)
%%% 		==> {ok, Action}
%%% 		    {error, Reason}	
%%% 
%%% 	A reward function Maps each perceived state (or state-action pair) 
%%% 	of the environment to a single number, a reward:
%%% 	reward(State, Action)
%%% 		==> Reward
%%% 
%%% 	A value function specifies what is good in the long run --- predicates
%%% 	the total amount of reward an agent can expect to accumulate over 
%%% 	the future, starting from that state:
%%% 	value(State)
%%% 		==> Value
%%% 
%%% 	A model of the environment that mimics the behavior of the environment
%%% 	--- given a state and an action the model might predict the resultant
%%% 	next state and next reward:
%%% 	model(States, Actions)
%%% 		==> States
%%% 			ignore
%%% 
%%% ----------------------------------------------------------------------------

%% API
-export([behaviour_info/1]).

%%% ============================================================================
%%% 	API
%%% ============================================================================

behaviour_info(callbacks) ->
	[{policy, 1}, {reward, 2}, {value, 1}, {model, 2}];
behaviour_info(_) ->
	undefined.
