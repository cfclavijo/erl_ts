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
        , no_sibling_return_undefined/1
        , basic_functionality/1
        , query_function_test/1
        , tree_gc_frees_memory/1
        , tree_delete_works/1
        , language_version_test/1
        , language_min_abi_version_test/1
        , parser_included_ranges_test/1
        , parser_language_test/1
        , language_name_test/1
        , node_eq_test/1
        , parser_parse_string_test/1
        ]).

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
  , basic_functionality
  , query_function_test
  , tree_gc_frees_memory
  , tree_delete_works
  , language_version_test
  , language_min_abi_version_test
  , parser_included_ranges_test
  , parser_language_test
  , language_name_test
  , node_eq_test
  , parser_parse_string_test
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
  erlang:garbage_collect(),
  ct:print(default, ?LOW_IMPORTANCE, "ct:print + erl_ts:node_end_byte = segfault", [], []),
  ct:pal("a flush could mark the Tree or their child nodes as to be deleted"),
  timer:sleep(20),
  erlang:garbage_collect(),
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

%% @doc Test that the core TS‑Parser functions work as expected.
basic_functionality(_Config) ->
  SC = "fun(A) -> 1 + 2.", %% simple Erlang expression
  {ok, Parser} = erl_ts:parser_new(),
  {ok, Lang} = erl_ts:tree_sitter_erlang(),
  true = erl_ts:parser_set_language(Parser, Lang),
  Tree = erl_ts:parser_parse_string(Parser, SC),
  RootNode = erl_ts:tree_root_node(Tree),

                                             % Verify parse tree has expected structure (as before)
  FunDeclNode = erl_ts:node_child(RootNode, 0),      % fun declaration is first child of root node
  ?assertNotEqual(undefined, FunDeclNode),
  ChildCount = erl_ts:node_child_count(FunDeclNode),
  ?assertEqual(3, ChildCount),                            %- FUN_NODE | ARG_LIST | DOT_NODE

  FunNode = erl_ts:node_child(FunDeclNode, 0),       % the “fun” keyword node
  ?assertEqual("fun", erl_ts:node_text(FunNode, SC)),

  %% New sibling checks for FunNode
  PrevSibling = erl_ts:node_prev_sibling(FunNode),      % should be undefined before the fun starts
  NextSibling = erl_ts:node_next_sibling(FunNode),      % points to argument list node (e.g., (")

  ?assertEqual(undefined, PrevSibling),                   % no previous sibling for FunNode
  ?assertNotEqual(undefined, NextSibling),           % there is a next sibling (argument list)
  ArgListText = erl_ts:node_text(NextSibling, SC),
  ?assertEqual("(A) -> 1 + 2", ArgListText),               %% full expected text
  ok = erl_ts:tree_delete(Tree).

%% @doc Verify erl_ts:query_new/2 function works correctly
query_function_test(_Config) ->
  SC = "fun(A)->1+2.\nfoo(B)->3.",
  {ok, Parser} = erl_ts:parser_new(),
  {ok, Lang}   = erl_ts:tree_sitter_erlang(),
  true = erl_ts:parser_set_language(Parser, Lang),
  Tree = erl_ts:parser_parse_string(Parser, SC),
  RootNode = erl_ts:tree_root_node(Tree),

  ?assertEqual(
     "(source_file exprs: (anonymous_fun clauses: "
"(fun_clause args: (expr_args args: (var)) body: (clause_body exprs: "
"(binary_op_expr lhs: (integer) rhs: (integer)))) (MISSING \"end\")) "
"exprs: (call expr: (atom) args: (expr_args args: (var))) (ERROR) exprs: (integer))",
     erl_ts:node_string(RootNode)),

  QueryString = "(fun_clause)",
  {Query, _, NoError} = erl_ts:query_new(Lang, QueryString),
  ?assertEqual(error_none, NoError),
   ?assertEqual(1, erl_ts:query_pattern_count(Query)),
   ok = erl_ts:tree_delete(Tree).

%% @doc Verify that TSTree memory is released when garbage collected.
%% This test validates that the NIF properly frees the underlying C tree
%% when the Erlang resource is garbage collected by BEAM.
%% NOTE: This test may show variance due to BEAM's memory management.
%% The key check is that memory doesn't grow unbounded with more trees.
tree_gc_frees_memory(_Config) ->
  Source = "fun(A) -> 1 + 2.",
  {ok, Parser} = erl_ts:parser_new(),
  {ok, Lang} = erl_ts:tree_sitter_erlang(),
  true = erl_ts:parser_set_language(Parser, Lang),

  %% Create first batch
  Trees1 = [erl_ts:parser_parse_string(Parser, Source) || _ <- lists:seq(1, 100)],
  RootNode = erl_ts:tree_root_node(hd(Trees1)),
  IsNamed = erl_ts:node_is_named(RootNode),
  ?assertEqual(true, IsNamed),

  erlang:garbage_collect(),
  timer:sleep(20),
  erlang:garbage_collect(),
  MemAfterFirstBatch = erlang:memory(total),

  %% Create second batch (should not significantly increase if properly freed)
  Trees2 = [erl_ts:parser_parse_string(Parser, Source) || _ <- lists:seq(1, 100)],
  RootNode2 = erl_ts:tree_root_node(hd(Trees2)),
  ?assertEqual(true, erl_ts:node_is_named(RootNode2)),

  erlang:garbage_collect(),
  timer:sleep(20),
  erlang:garbage_collect(),
  MemAfterSecondBatch = erlang:memory(total),

  Growth = MemAfterSecondBatch - MemAfterFirstBatch,
  ct:pal("After first 100: ~p, After second 100: ~p, Growth: ~p bytes",
         [MemAfterFirstBatch, MemAfterSecondBatch, Growth]),
  ?assert(Growth < 2 * 1024 * 1024),
  ok.

%% @doc Verify that tree_delete/1 works correctly and releases resources.
%% This test creates trees and explicitly deletes them, then verifies
%% that the tree resources are properly cleaned up.
tree_delete_works(_Config) ->
  Source = "fun(A) -> 1 + 2.",
  {ok, Parser} = erl_ts:parser_new(),
  {ok, Lang} = erl_ts:tree_sitter_erlang(),
  true = erl_ts:parser_set_language(Parser, Lang),

  %% Create and immediately delete trees
  Trees = [begin
             T = erl_ts:parser_parse_string(Parser, Source),
             ok = erl_ts:tree_delete(T),
             T
           end || _ <- lists:seq(1, 50)],

  %% Verify we can still use the parser after deleting trees
  Tree = erl_ts:parser_parse_string(Parser, Source),
  Root = erl_ts:tree_root_node(Tree),
  ?assertEqual(true, erl_ts:node_is_named(Root)),

  %% Clean up the last tree
  ok = erl_ts:tree_delete(Tree),

  %% Try to delete already deleted tree (should return ok)
  ok = erl_ts:tree_delete(hd(Trees)),
  ok.

%% @doc Test language version functions
language_version_test(_Config) ->
  {ok, Lang} = erl_ts:tree_sitter_erlang(),

  Version = erl_ts:language_version(Lang),
  ?assert(is_integer(Version)),
  ?assertEqual(14, Version),  %% minimum compatible version

  AbiVersion = erl_ts:language_abi_version(Lang),
  ?assert(is_integer(AbiVersion)),
  ?assertEqual(14, AbiVersion),  %% minimum compatible version

  ?assertEqual(Version, AbiVersion),
  ok.

%% @doc Test language min ABI version functions
language_min_abi_version_test(_Config) ->
  {ok, Lang} = erl_ts:tree_sitter_erlang(),

  %% Test arity 0 - returns minimum version
  MinVersion = erl_ts:language_min_abi_version(),
  ?assert(is_integer(MinVersion)),
  ?assertEqual(14, MinVersion),

  %% Test arity 1 - returns compatibility tuple
  {ok, IsCompatible} = erl_ts:language_min_abi_version(Lang),
  ?assert(is_boolean(IsCompatible)),
  ?assert(IsCompatible),
  ok.

%% @doc Test parser_included_ranges/1 and parser_set_included_ranges/2
parser_included_ranges_test(_Config) ->
  {ok, Parser} = erl_ts:parser_new(),
  {ok, Lang} = erl_ts:tree_sitter_erlang(),
  true = erl_ts:parser_set_language(Parser, Lang),

  %% Test with empty list - returns ok
  EmptyResult = erl_ts:parser_set_included_ranges(Parser, []),
  ?assertEqual(ok, EmptyResult),

  %% Verify default ranges (whole document when empty)
  DefaultRanges = erl_ts:parser_included_ranges(Parser),
  ?assert(is_list(DefaultRanges)),
  ?assertEqual(1, length(DefaultRanges)),

  %% Test with ranges - returns ok
  Range1 = #{
    start_point => #{row => 0, column => 0},
    end_point => #{row => 0, column => 10},
    start_byte => 0,
    end_byte => 10
  },
  Range2 = #{
    start_point => #{row => 2, column => 0},
    end_point => #{row => 2, column => 10},
    start_byte => 20,
    end_byte => 30
  },
  Ranges = [Range1, Range2],

  SetResult = erl_ts:parser_set_included_ranges(Parser, Ranges),
  ?assertEqual(ok, SetResult),

  %% Verify ranges were set
  IncludedRanges = erl_ts:parser_included_ranges(Parser),
  ?assertEqual(2, length(IncludedRanges)),
  ok.

%% @doc Test get language from parser
parser_language_test(_Config) ->
  {ok, Parser} = erl_ts:parser_new(),
  {ok, Lang} = erl_ts:tree_sitter_erlang(),
  true = erl_ts:parser_set_language(Parser, Lang),

  {ok, LangResult} = erl_ts:parser_language(Parser),
  ?assert(is_reference(LangResult)),

  Version1 = erl_ts:language_abi_version(Lang),
  Version2 = erl_ts:language_abi_version(LangResult),
  ?assertEqual(Version1, Version2),
  ok.

%% @doc Get language name - may return undefined for older
%% tree-sitter versions or Languages not implementing this
%% feature (abi_version <= 14)
language_name_test(_Config) ->
  {ok, Lang} = erl_ts:tree_sitter_erlang(),
  Name = erl_ts:language_name(Lang),
  case Name of
    undefined ->
      ok;
    _ ->
      ?assertEqual("tree-sitter-erlang", Name)
  end,
  ok.

%% @doc Test node_eq
node_eq_test(_Config) ->
  {ok, Parser} = erl_ts:parser_new(),
  {ok, Lang} = erl_ts:tree_sitter_erlang(),
  true = erl_ts:parser_set_language(Parser, Lang),

  Source = "foo() -> ok.",
  Tree = erl_ts:parser_parse_string(Parser, Source),
  Root = erl_ts:tree_root_node(Tree),

  %% Same node compared to itself should be equal
  ?assert(erl_ts:node_eq(Root, Root)),

  %% Get child node
  Child = erl_ts:node_child(Root, 0),

  %% Child is different from root
  ?assertNot(erl_ts:node_eq(Root, Child)),

  ok = erl_ts:tree_delete(Tree),
  ok.

%% @doc Test parses a string using the parser
parser_parse_string_test(_Config) ->
  {ok, Parser} = erl_ts:parser_new(),
  {ok, Lang} = erl_ts:tree_sitter_erlang(),
  true = erl_ts:parser_set_language(Parser, Lang),

  Source = "foo() -> ok.",
  Tree = erl_ts:parser_parse_string(Parser, Source),

  Root = erl_ts:tree_root_node(Tree),
  ?assertNotEqual(undefined, Root),

  IsNamed = erl_ts:node_is_named(Root),
  ?assertEqual(true, IsNamed),

  RootText = erl_ts:node_string(Root),
  ?assert(is_list(RootText)),
  ok = erl_ts:tree_delete(Tree),
  ok.
