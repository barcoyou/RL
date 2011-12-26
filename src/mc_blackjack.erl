%% This module simulates the Exmaple 5.1 and Figure 5.2
%% in the book: <Reinforcement Learning: An Introduction>

-module(mc_blackjack).
-author('Barco You: barcojie@gmail.com').

-define(WIN, 1).
-define(LOSE, -1).
-define(DRAW, 0).

-export([run/1]).

run(N) ->
	Returns = [[] || _PC <-lists:seq(12,21), _Show <-lists:seq(1,10)],
	episode(N, Returns,[]).

episode(0, Returns, _) ->
	average(Returns);
episode(N, Returns, Visit) ->
	Show = card(),
	State = {sum(11,card()), Show},
	NewVisit = [State|Visit],
	Dc = sum(Show, card()),
	case State of
		{21, _} ->
			case Dc of
				21 ->
					episode(N-1, update_return(Returns, NewVisit, ?DRAW), []);
				_Else ->
					episode(N-1, update_return(Returns, NewVisit, ?WIN), [])
			end;
		State ->
			case policy(State, NewVisit) of
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

card() ->
	min(10, random:uniform(13)).

policy({PlayerCount, DellerShow}, Visit) ->
	NewCount = hit(PlayerCount),
	if
		NewCount>21 ->
			{bust, NewCount, Visit};
		true ->
			if
				NewCount<20 ->
					policy({NewCount, DellerShow}, [{NewCount, DellerShow}|Visit]);
				true ->
					{stick, NewCount, [{NewCount, DellerShow}|Visit]}
			end
	end.

hit(Count) ->
	sum(Count, card()).

deller_draw(DellerCount) ->
	Count = sum(DellerCount, card()),
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

average(Returns) ->
	average(Returns, []).
average([], L) ->
	lists:reverse(L);
average([H|T], L) ->
	average(T, [lists:sum(H)/length(H)|L]).

update_return(Returns, [], _R) ->
	Returns;
update_return(Returns, [{Count, Show}|Vt], R) ->
	update_return(update_list((Count-12)*10+Show, Returns, R), Vt, R).

update_list(Index, List, R) ->
	update_list(Index, List, R, [], 1).
update_list(_Index, [], _R, L, _N) ->
	lists:reverse(L);
update_list(Index, [H|T], R, L, N) ->
	if
		Index==N ->
			update_list(Index, T, R, [[R|H] | L], N+1);
		true ->
			update_list(Index, T, R, [H | L], N+1)
	end.
