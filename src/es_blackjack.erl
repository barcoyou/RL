%% This module simulates the Exmaple 5.3 and Figure 5.5
%% in the book: <Reinforcement Learning: An Introduction>,
%% to exemplify the Monte Carlo ES algorithm.

-module(es_blackjack).
-author('Barco You: barcojie@gmail.com').

-define(WIN, 1).
-define(LOSE, -1).
-define(DRAW, 0).

-export([run/1]).

run(N) ->
	%% To generate {State, Action, {Value, Visits}}
	Returns = [{{PC,Show}, A, {0,0}} || PC <-lists:seq(12,21), Show <-lists:seq(1,10), A <- action_set()],
	episode(N, Returns,[]).

episode(0, Returns, _) ->
	SAR = [{S, A, R} || {S, A, {R, _V}} <-Returns],
	io:format("~p", [maxier(SAR)]);
episode(N, Returns, Visit) ->
	{{_Pc,Show}=State, Action} = exploring_start(),
	NewVisit = [{State, Action} | Visit],
	Dc = hit(Show),
	case State of
		{21, _} ->
			case Dc of
				21 ->
					episode(N-1, update_return(Returns, NewVisit, ?DRAW), []);
				_Else ->
					episode(N-1, update_return(Returns, NewVisit, ?WIN), [])
			end;
		State ->
			case player_draw(State, Returns, NewVisit) of
				{bust, _, Vis} ->
					episode(N-1, update_return(Returns, Vis, ?LOSE), []);
				{stick, PlayerCount, Vis} ->
					case deller_draw(Dc) of
						{bust, _} ->
							episode(N-1, update_return(Returns, Vis, ?WIN), []);
						{stick, DellerCount} ->
							if
								PlayerCount==DellerCount ->
									episode(N-1, update_return(Returns, Vis, ?DRAW), []);
								true ->
									if
										PlayerCount> DellerCount ->
											episode(N-1, update_return(Returns, Vis, ?WIN), []);
										true ->
											episode(N-1, update_return(Returns, Vis, ?LOSE), [])
									end
							end
					end
			end
	end.

action_set() ->
	[stick, hit].

exploring_start() ->
	Action =
	case random:uniform() < 0.5 of
		true ->
			stick;
		false ->
			hit
	end,
	{{sum(11, card()), card()}, Action}.

card() ->
	min(10, random:uniform(13)).

policy(State, Returns) ->
	Sap =  [{A, R} || {S, A, {R, _}} <- Returns, S==State],
	{_As, Rs} = lists:unzip(Sap),
	MaxR = lists:max(Rs),
	find_max_act(MaxR, Sap).

find_max_act(MaxR, [{A, R}| T]) ->
	if
		MaxR==R ->
			A;
		true ->
			find_max_act(MaxR, T)
	end.

player_draw({PlayerCount, Show}=State, Returns, Visit) ->
	Action = policy(State, Returns),
	case Action of
		hit ->
			NewCount = hit(PlayerCount),
			if
				NewCount>21 ->
					{bust, NewCount, [{State, Action} | Visit]};
				true ->
					player_draw({NewCount, Show}, Returns, [{State, Action} | Visit])
			end;
		stick ->
			{stick, PlayerCount, [{State, Action} | Visit]}
	end.

hit(Count) ->
	sum(Count, card()).

deller_draw(DellerCount) ->
	Count = hit(DellerCount),
	if
		Count > 21 ->
			{bust, Count};
		true ->
			if
				Count < 17 ->
					deller_draw(Count);
				true ->
					{stick, Count}
			end
	end.

sum(Count, Draw) ->
	case Draw of
		1 ->
			if
				Count+11>21 ->
					Count+1;
				true ->
					Count+11
			end;
		Draw ->
			Count+Draw
	end.

update_return(Returns, [], _Reward) ->
	Returns;
update_return(Returns, [{State, Action}|Vt], Reward) ->
	update_return(lists:map(fun({S, A, {R,V}}) when S==State, A==Action ->
					{S, A, {(R*V+Reward)/(V+1), V+1}};
				(X) ->
					X	
			end, Returns), Vt, Reward).

maxier(List) ->
	maxier(List,0,[]).
maxier([], _, L) ->
	lists:reverse(L);
maxier([H|T], 0, L) ->
	maxier(T, H, L);
maxier([{_,_,R1}=H|T], {_,_,R2}=X, L) ->
	if
		R1 > R2 ->
			maxier(T, 0, [H|L]);
		true ->
			maxier(T, 0, [X|L])
	end.
