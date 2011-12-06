%%% =========================================================================
%%% This header file defines the data type of the agent (and environment) 
%%% state for the n-Armed Bandit problem.
%%% =========================================================================

%%---------------------------------------------------------------------
%% Data Type: state
%% where:
%% qt: A list records the estimated action-value at the current play.
%% rqt: A list records the true action-value at the current play. 
%% action_times: A list records the how many times actions are selected.
%% play: An integer indicates which play the agent is in.
%% reward: A number denotes the average reward gotten till the current play.
%% percent: A float number denotes the optimal action percentage till the current play. 
%% env: Provide an interface of simulated environment which which the
%%      agent can interact, such as a pid of the Gaussian random number
%%      generating process.
%%----------------------------------------------------------------------
-record(state, {qt = [], %list()
				rqt = [], %list()
				action_times = [], %list()
				action_value, %item(), action-value pairs selected last time
				play = 0, %number()
				reward, %number()s
				percent, %number()s
				file, %io_devide()
				env, %pid()|something else, simulating the environment to interact with
				param = 0}). %float(), adjustable parameters for some
							 %algorithms, such as Epsilon for greedy method and
							 %Temperature for softmax method.


