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
-module(gauss_gen).
-author('barcojie@gmail.com').
%% =========================================================================
%% A very general module used to generate random number from Gaussian Dist
%% =========================================================================
-export([start/0,
		stop/1,
		init/0,
		get_number/3,
		call/2]).

%%% 
%%% API
%%% 
start() ->
	spawn_link(?MODULE, init, []).

init() ->
	<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	random:seed({A,B,C}),
	loop().

stop(Pid) ->
	call(Pid, stop).

get_number(Pid, Mu, Sigma) ->
	call(Pid, {gen_number, Mu, Sigma}).

call(Pid, {gen_number, Mu, Sigma}) ->
	c:flush(),
	Ref = make_ref(),
	Pid ! {gen_number, {self(), Ref}, {Mu, Sigma}},
	receive
		{Ref, {value, Var}} ->
			Var;
		{Ref, {error, Msg}} ->
			{error, Msg}
	after
		5000 ->
			timeout
	end;
call(Pid, stop) ->
	Pid ! stop;
call(_Pid, Arg) ->
	io:format("Inquiry: ~w is not available to gauss_gen service~n", [Arg]).
%%% 
%%% Internal Functions
%%% 
loop() ->
	receive
		{gen_number, {Pid, Ref}, {Mu, Sigma}} ->
			Var = cal(Mu, Sigma),
			Pid ! {Ref, {value, Var}},
			loop();
		stop ->
			stopped
	end.

cal(Mu, Sigma) ->
	V1 = 2.0 * random:uniform() - 1.0,
	V2 = 2.0 * random:uniform() - 1.0 ,
	S = V1*V1 + V2*V2,
	if 
		S >= 1.0 -> 
			cal(Mu, Sigma);
		true ->
			Mu + Sigma*V1*math:sqrt((-2.0*math:log(S)) / S)
	end.

