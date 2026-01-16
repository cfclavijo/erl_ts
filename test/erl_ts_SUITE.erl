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

-export([all/0, groups/0, suite/0]).
-export([ init_per_suite/1
        , end_per_suite/1
        , init_per_group/2
        , end_per_group/2
        , init_per_testcase/2
        , end_per_testcase/2]).
-export([ no_segfault_post_print/1
        , no_sibling_return_undefined/1]).

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
  [ no_segfault_post_print
  , no_sibling_return_undefined
  ].

%% @doc The communication between Erlang and C goes through ERL_NIF_TERMS which
%% ideally are being explicitly marked to be freed by the BEAM. However, if
%% subsequent objects require access to a resource that was marked to be freed,
%% a race condition starts that will probably end in a segmentation fault.
%% The ct:print statement seems to be one of the points where the BEAM starts making
%% calls to free resources, as such, any subsequent call to a resource dependant on it
%% would show the aforementioned behavior.
%% It is then the responsibility of the users to decide on expliciteness when calling
%% the destructors for those resources of high importance (i.e. TSParser, TSTree).
no_segfault_post_print(_Config) ->
  SC = "fun(A)->1+2.",
  {ok, Parser} = erl_ts:parser_new(),
  {ok, Lang} = erl_ts:tree_sitter_erlang(),
  true = erl_ts:parser_set_language(Parser, Lang),
  Tree = erl_ts:parser_parse_string(Parser, SC),
  RootNode = erl_ts:tree_root_node(Tree),
  ct:print(default, ?LOW_IMPORTANCE, "ct:print + erl_ts:node_end_byte = segfault", [], []),
  ct:pal("a flush could mark the Tree or their child nodes as to be deleted"),
  FunDeclNode = erl_ts:node_child(RootNode, 0),
  ?assertNotEqual({error,tstree_freed}, FunDeclNode),
  ChildCount = erl_ts:node_child_count(FunDeclNode),
  ?assertEqual(3, ChildCount),

  FunNode = erl_ts:node_child(FunDeclNode, 0),
  ?assertEqual("fun", erl_ts:node_text(FunNode, SC)),
  ct:pal("ending"),
  ok = erl_ts:tree_delete(Tree), %% Finally, explicitly free the resource
  ok.

%% @doc return undefined when a sibling node does not exist.
no_sibling_return_undefined(_Config) ->
  SC = "fun(A)->1+2.",
  {ok, Parser} = erl_ts:parser_new(),
  {ok, Lang} = erl_ts:tree_sitter_erlang(),
  true = erl_ts:parser_set_language(Parser, Lang),
  Tree = erl_ts:parser_parse_string(Parser, SC),
  RootNode = erl_ts:tree_root_node(Tree),
  FunDeclNode = erl_ts:node_child(RootNode, 0),
  PrevSiblingNode = erl_ts:node_prev_sibling(FunDeclNode),
  DotNode = erl_ts:node_next_sibling(FunDeclNode), %% a dot node .
  NextSiblingNode = erl_ts:node_next_sibling(DotNode),
  erl_ts:tree_delete(Tree),
  ?assertEqual(undefined, PrevSiblingNode),
  ?assertNotEqual(undefined, DotNode),
  ?assertEqual(undefined, NextSiblingNode),
  ok.
