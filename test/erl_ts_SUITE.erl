%%%-------------------------------------------------------------------
%%% @author Carlos F. Clavijo
%%% @copyright (C) 2024, Carlos F. Clavijo
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%% limitations under the License.
%%% @doc
%%%
%%% @end
%%%
%%% Created : 18 Dec 2024 by Carlos F. Clavijo
%%%-------------------------------------------------------------------
-module(erl_ts_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

suite() ->
  [{timetrap,{seconds,70}}].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

init_per_group(_GroupName, Config) ->
  Config.

end_per_group(_GroupName, _Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

groups() ->
  [].

all() ->
  [segfault_post_print].

%% @doc The communication between Erlang and C goes through ERL_NIF_TERMS which
%% ideally are being explicitly marked to be freed by the BEAM. However, if
%% subsequent objects require access to a resource that was marked to be freed,
%% a race condition starts that will probably end in a segmentation fault.
%% The ct:print statement seems to be the point where the BEAM starts making
%% calls to free resources, as such, any subsequent call to a resource dependant
%% would show the aforementioned behavior.
%% It is then responsibility to the users to decide on expliciteness when calling
%% the destructors for those resources of high importance (i.e. TSParser, TSTree).
%% This tests exemplifies the challenges of memory management.
segfault_post_print(_Config) ->
  SC = "fun(A)->1+2.",
  {ok, Parser} = erl_ts:parser_new(),
  {ok, Lang} = erl_ts:tree_sitter_erlang(),
  true = erl_ts:parser_set_language(Parser, Lang),
  Tree = erl_ts:parser_parse_string(Parser, SC),
  RootNode = erl_ts:tree_root_node(Tree),
  ct:print(default, ?LOW_IMPORTANCE, "ct:print + erl_ts:node_end_byte = segfault", [], []),
  %% ct:pal("~p~nFunDeclNode=~p", [{?MODULE, ?FUNCTION_NAME, ?LINE}, erl_ts:node_child(RootNode, 0)]),
  %% erl_ts:tree_language(Tree), %% this makes the subsequent calls to erl_ts work
  %% ?assertNot(erl_ts:node_is_null(RootNode)),
  %% RootNodeB = erl_ts:tree_root_node(Tree), %% this also makes the subsequent calls to erl_ts work
  FunDeclNode = erl_ts:node_child(RootNode, 0),
  ChildCount = erl_ts:node_child_count(FunDeclNode),
  %% ?assertEqual(2, ChildCount),

  %% FunClauseNode = erl_ts:node_child(FunDeclNode, 0),
  %% PointNode = erl_ts:node_child(FunDeclNode, 1),

  %% ?assertEqual("fun(A)->1+2", erl_ts:node_text(FunClauseNode, SC)),
  %% ?assertEqual(".", erl_ts:node_text(PointNode, SC)),

  %% ?assertEqual("fun(A)->1+2.", erl_ts:node_text(FunDeclNode, SC)),
  %% erl_ts:node_start_byte(FunDeclNode),
  %% A = erl_ts:node_end_byte(FunDeclNode),
  %% A = erl_ts:node_end_byte(FunDeclNode),
  ct:pal("ending"),
  ok.
