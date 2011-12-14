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
-module(matrix).
-author('barcojie@gmail.com').

%%% -------------------------------------------------------------------------
%%% @doc
%%% This module provides general functionalities to generate multi-dimensional
%%% matrices and to manipulate elements of the matrices.
%%% -------------------------------------------------------------------------
-export([gen/2,
		get_elem/2,
		dim/1,
		replace/3,
		sum/1]).

%%----------------------------------------------------------------------
%% @doc
%% Function: gen/2
%% Purpose: To generate a multi-dimensional matrix.
%% Args: Dims - A list indicating the dimensions of the matrix.
%% 		 Initiator - A function|number to initialize the elements of the matrix.
%%                If it's a function, it must take a list as the only argument
%%                to represent index of an element of the matrix.      
%% Returns: A list representing a matrix  
%%----------------------------------------------------------------------
gen(Dims, Initiator) ->
	make_list(Dims, Initiator).

%%----------------------------------------------------------------------
%% @doc
%% Function: get_elem/2
%% Purpose: Get the element value of a matrix.
%% Args: Index - A list indicating the index of the element. 
%%       Matrix - A list representing a multi-dimensional matrix
%% Returns: A number as the value of the element 
%%----------------------------------------------------------------------
get_elem([IndexH|[]], Matrix) ->
	lists:nth(IndexH, Matrix);
get_elem([IndexH|IndexT], Matrix) ->
	get_elem(IndexT, lists:nth(IndexH, Matrix)).

%%----------------------------------------------------------------------
%% Function: dim/1
%% Purpose: Return the dimensions of a multi-dimensional matrix.
%% Args:  Matrix - A list representing a multi-dimensional matrix. 
%% Returns: A list denoting the dimensions of the matrix. 
%%----------------------------------------------------------------------
dim(Matrix) ->
	dim(Matrix, 1, []).
dim([MatrixH|[]], N, L) when is_list(MatrixH) ->
	dim(MatrixH, 1, [N|L]);
dim([_MatrixH|[]], N, L) ->
	lists:reverse([N|L]);
dim([_MatrixH|MatrixT], N, L) ->
	dim(MatrixT, N+1, L).


%%----------------------------------------------------------------------
%% Function: replace/3
%% Purpose: Replace a specific element of a matrix with a new value 
%% Args: Index - A list denoting the index of the element to be replaced 
%%       Matrix - A list representing a multi-dimensional matrix
%%       Value - A number as the new value
%% Returns: A list representing a new multi-dimensional matrix 
%%----------------------------------------------------------------------
replace(Index, Matrix, Value) ->
	Dims = dim(Matrix),
	make_new_list(Dims, Index, Matrix, Value, []).

%%----------------------------------------------------------------------
%% Function: sum/1
%% Purpose: Sum all the values of element of a matrix 
%% Args: Matrix - A list representing a multi-dimensional matrix
%% Returns: A number 
%%----------------------------------------------------------------------
sum(Matrix) ->
	sum(Matrix, 0).
sum([H|[]], Sum) when is_list(H) ->
	sum(H, Sum);
sum([H|T], Sum) when is_list(H) ->
	sum(T, sum(H)+Sum);
sum([H|[]], Sum) ->
	H+Sum;
sum([H|T], Sum) ->
	sum(T, H+Sum).

%%% 
%%% Internal Functions
%%% 
make_list(Dims, Initiator) when is_function(Initiator) ->
	make_list(Dims, Initiator, []);
make_list([DimsH|[]], Initiator) ->
	make_list_helper1(DimsH, Initiator, []);
make_list([DimsH|DimsT], Initiator) ->
	make_list_helper1(DimsH, make_list(DimsT, Initiator), []).

make_list([DimsH|[]], Initiator, Index) ->
	Inner = fun(X) ->
			Initiator(lists:reverse([X|Index]))
	end,
	make_list_helper2(DimsH, Inner, []); 
make_list([DimsH|DimsT], Initiator, Index) ->
	Outter = fun(X) ->
			make_list(DimsT, Initiator, [X|Index])
	end,
	make_list_helper2(DimsH, Outter, []).

make_list_helper1(0, _X, L) ->
	L;
make_list_helper1(N, X, L) ->
	make_list_helper1(N-1, X, [X|L]).

make_list_helper2(0, _Fun, L) ->
	L;
make_list_helper2(N, Fun, L) ->
	make_list_helper2(N-1, Fun, [Fun(N)|L]).

make_new_list([DimsH|[]], Index, Matrix, Value, In) ->
	Inner = fun(X) ->
			case lists:reverse([X|In]) == Index of
				true ->
					Value;
				false ->
					get_elem(lists:reverse([X|In]), Matrix)
			end
	end,
	make_list_helper2(DimsH, Inner, []);
make_new_list([DimsH|DimsT], Index, Matrix, Value, In) ->
	Outter = fun(X) ->
			make_new_list(DimsT, Index, Matrix, Value, [X|In])
	end,
	make_list_helper2(DimsH, Outter, []).
