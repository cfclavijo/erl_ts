-module(erl_ts).

-export([init/0]).

-export([ tree_sitter_erlang/0,
          parser_new/0,
          parser_delete/1,
          parser_language/1,
          parser_set_language/2,
          parser_set_included_ranges/3,
          parser_included_ranges/1,
          parser_parse/3,
          parser_parse_with_options/4,
          parser_parse_string/3,
          parser_parse_string_encoding/5,
          parser_reset/1,
          parser_set_timeout_micros/2,
          parser_timeout_micros/1,
          parser_set_cancellation_flag/2,
          parser_cancellation_flag/1,
          parser_set_logger/2,
          parser_logger/1,
          parser_print_dot_graphs/2,
          tree_copy/1,
          tree_delete/1,
          tree_root_node/1,
          tree_root_node_with_offset/3,
          tree_language/1,
          tree_included_ranges/2,
          tree_edit/2,
          tree_get_changed_ranges/3,
          tree_print_dot_graph/2,
          node_type/1,
          node_symbol/1,
          node_language/1,
          node_grammar_type/1,
          node_grammar_symbol/1,
          node_start_byte/1,
          node_start_point/1,
          node_end_byte/1,
          node_end_point/1,
          node_string/1,
          node_is_null/1,
          node_is_named/1,
          node_is_missing/1,
          node_is_extra/1,
          node_has_changes/1,
          node_has_error/1,
          node_is_error/1,
          node_parse_state/1,
          node_next_parse_state/1,
          node_parent/1,
          node_child_containing_descendant/2,
          node_child_with_descendant/2,
          node_child/2,
          node_field_name_for_child/2,
          node_field_name_for_named_child/2,
          node_child_count/1,
          node_named_child/2,
          node_named_child_count/1,
          node_child_by_field_name/2,
          node_child_by_field_id/2,
          node_next_sibling/1,
          node_prev_sibling/1,
          node_next_named_sibling/1,
          node_prev_named_sibling/1,
          node_first_child_for_byte/2,
          node_first_named_child_for_byte/2,
          node_descendant_count/1,
          node_descendant_for_byte_range/3,
          node_descendant_for_point_range/3,
          node_named_descendant_for_byte_range/3,
          node_named_descendant_for_point_range/3,
          node_edit/2,
          node_eq/2,
          node_text/2,
          tree_cursor_new/1,
          tree_cursor_delete/1,
          tree_cursor_reset/2,
          tree_cursor_reset_to/2,
          tree_cursor_current_node/1,
          tree_cursor_current_field_name/1,
          tree_cursor_current_field_id/1,
          tree_cursor_goto_parent/1,
          tree_cursor_goto_next_sibling/1,
          tree_cursor_goto_previous_sibling/1,
          tree_cursor_goto_first_child/1,
          tree_cursor_goto_last_child/1,
          tree_cursor_goto_descendant/2,
          tree_cursor_current_descendant_index/1,
          tree_cursor_current_depth/1,
          tree_cursor_goto_first_child_for_byte/2,
          tree_cursor_goto_first_child_for_point/2,
          tree_cursor_copy/1,
          query_new/2,
          query_delete/1,
          query_pattern_count/1,
          query_capture_count/1,
          query_string_count/1,
          query_start_byte_for_pattern/2,
          query_end_byte_for_pattern/2,
          query_predicates_for_pattern/3,
          query_is_pattern_rooted/2,
          query_is_pattern_non_local/2,
          query_is_pattern_guaranteed_at_step/2,
          query_capture_name_for_id/2,
          query_capture_quantifier_for_id/3,
          query_string_value_for_id/2,
          query_disable_capture/3,
          query_disable_pattern/2,
          query_capture/2,
          query_cursor_new/0,
          query_cursor_delete/1,
          query_cursor_exec/3,
          query_cursor_exec_with_options/4,
          query_cursor_did_exceed_match_limit/1,
          query_cursor_match_limit/1,
          query_cursor_set_match_limit/2,
          query_cursor_set_timeout_micros/2,
          query_cursor_timeout_micros/1,
          query_cursor_set_byte_range/3,
          query_cursor_set_point_range/3,
          query_cursor_next_match/2,
          query_cursor_remove_match/2,
          query_cursor_next_capture/3,
          query_cursor_set_max_start_depth/2,
          language_copy/1,
          language_delete/1,
          language_symbol_count/1,
          language_state_count/1,
          language_symbol_name/2,
          language_symbol_for_name/4,
          language_field_count/1,
          language_field_name_for_id/2,
          language_field_id_for_name/2,
          language_symbol_type/2,
          language_version/1,
          language_next_state/3,
          language_name/1,
          lookahead_iterator_new/2,
          lookahead_iterator_delete/1,
          lookahead_iterator_reset_state/2,
          lookahead_iterator_reset/3,
          lookahead_iterator_language/1,
          lookahead_iterator_next/1,
          lookahead_iterator_current_symbol/1,
          lookahead_iterator_current_symbol_name/1
        ]).


-define(APPNAME, erl_ts).
-define(LIBNAME, erl_ts).

init() ->
  ErlTsLib =
    case code:priv_dir(?APPNAME) of
      {error, bad_name} ->
        case filelib:is_dir(filename:join(["..", priv])) of
          true ->
            filename:join(["..", priv, ?LIBNAME]);
          _ ->
            filename:join([priv, ?LIBNAME])
        end;
      Dir ->
        filename:join(Dir, ?LIBNAME)
    end,
  ok = erlang:load_nif(ErlTsLib, 0).

tree_sitter_erlang() ->
  erlang:nif_error(nif_library_not_loaded).

parser_new() ->
  erlang:nif_error(nif_library_not_loaded).

parser_delete(_) ->
  erlang:nif_error(nif_library_not_loaded).

parser_language(_) ->
  erlang:nif_error(nif_library_not_loaded).

parser_set_language(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

parser_set_included_ranges(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

parser_included_ranges(_) ->
  erlang:nif_error(nif_library_not_loaded).

parser_parse(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

parser_parse_with_options(_, _, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

parser_parse_string(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

parser_parse_string_encoding(_, _, _, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

parser_reset(_) ->
  erlang:nif_error(nif_library_not_loaded).

parser_set_timeout_micros(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

parser_timeout_micros(_) ->
  erlang:nif_error(nif_library_not_loaded).

parser_set_cancellation_flag(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

parser_cancellation_flag(_) ->
  erlang:nif_error(nif_library_not_loaded).

parser_set_logger(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

parser_logger(_) ->
  erlang:nif_error(nif_library_not_loaded).

parser_print_dot_graphs(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

tree_copy(_) ->
  erlang:nif_error(nif_library_not_loaded).

tree_delete(_) ->
  erlang:nif_error(nif_library_not_loaded).

tree_root_node(_) ->
  erlang:nif_error(nif_library_not_loaded).

tree_root_node_with_offset(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

tree_language(_) ->
  erlang:nif_error(nif_library_not_loaded).

tree_included_ranges(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

tree_edit(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

tree_get_changed_ranges(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

tree_print_dot_graph(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

node_type(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_symbol(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_language(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_grammar_type(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_grammar_symbol(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_start_byte(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_start_point(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_end_byte(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_end_point(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_string(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_is_null(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_is_named(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_is_missing(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_is_extra(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_has_changes(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_has_error(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_is_error(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_parse_state(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_next_parse_state(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_parent(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_child_containing_descendant(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

node_child_with_descendant(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

node_child(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

node_field_name_for_child(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

node_field_name_for_named_child(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

node_child_count(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_named_child(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

node_named_child_count(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_child_by_field_name(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

node_child_by_field_id(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

node_next_sibling(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_prev_sibling(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_next_named_sibling(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_prev_named_sibling(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_first_child_for_byte(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

node_first_named_child_for_byte(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

node_descendant_count(_) ->
  erlang:nif_error(nif_library_not_loaded).

node_descendant_for_byte_range(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

node_descendant_for_point_range(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

node_named_descendant_for_byte_range(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

node_named_descendant_for_point_range(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

node_edit(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

node_eq(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

node_text(Node, SourceCode) ->
  Start = node_start_byte(Node),
  End = node_end_byte(Node),
  string:sub_string(SourceCode, Start + 1, End).

tree_cursor_new(_) ->
  erlang:nif_error(nif_library_not_loaded).

tree_cursor_delete(_) ->
  erlang:nif_error(nif_library_not_loaded).

tree_cursor_reset(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

tree_cursor_reset_to(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

tree_cursor_current_node(_) ->
  erlang:nif_error(nif_library_not_loaded).

tree_cursor_current_field_name(_) ->
  erlang:nif_error(nif_library_not_loaded).

tree_cursor_current_field_id(_) ->
  erlang:nif_error(nif_library_not_loaded).

tree_cursor_goto_parent(_) ->
  erlang:nif_error(nif_library_not_loaded).

tree_cursor_goto_next_sibling(_) ->
  erlang:nif_error(nif_library_not_loaded).

tree_cursor_goto_previous_sibling(_) ->
  erlang:nif_error(nif_library_not_loaded).

tree_cursor_goto_first_child(_) ->
  erlang:nif_error(nif_library_not_loaded).

tree_cursor_goto_last_child(_) ->
  erlang:nif_error(nif_library_not_loaded).

tree_cursor_goto_descendant(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

tree_cursor_current_descendant_index(_) ->
  erlang:nif_error(nif_library_not_loaded).

tree_cursor_current_depth(_) ->
  erlang:nif_error(nif_library_not_loaded).

tree_cursor_goto_first_child_for_byte(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

tree_cursor_goto_first_child_for_point(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

tree_cursor_copy(_) ->
  erlang:nif_error(nif_library_not_loaded).

query_new(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

query_delete(_) ->
  erlang:nif_error(nif_library_not_loaded).

query_pattern_count(_) ->
  erlang:nif_error(nif_library_not_loaded).

query_capture_count(_) ->
  erlang:nif_error(nif_library_not_loaded).

query_string_count(_) ->
  erlang:nif_error(nif_library_not_loaded).

query_start_byte_for_pattern(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

query_end_byte_for_pattern(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

query_predicates_for_pattern(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

query_is_pattern_rooted(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

query_is_pattern_non_local(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

query_is_pattern_guaranteed_at_step(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

query_capture_name_for_id(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

query_capture_quantifier_for_id(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

query_string_value_for_id(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

query_disable_capture(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

query_disable_pattern(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

-spec query_capture(TSNode :: term(), TSQuery :: term()) -> [tuple()].
query_capture(_TSNode, _TSQuery) ->
  erlang:nif_error(nif_library_not_loaded).

query_cursor_new() ->
  erlang:nif_error(nif_library_not_loaded).

query_cursor_delete(_) ->
  erlang:nif_error(nif_library_not_loaded).

query_cursor_exec(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

query_cursor_exec_with_options(_, _, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

query_cursor_did_exceed_match_limit(_) ->
  erlang:nif_error(nif_library_not_loaded).

query_cursor_match_limit(_) ->
  erlang:nif_error(nif_library_not_loaded).

query_cursor_set_match_limit(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

query_cursor_set_timeout_micros(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

query_cursor_timeout_micros(_) ->
  erlang:nif_error(nif_library_not_loaded).

query_cursor_set_byte_range(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

query_cursor_set_point_range(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

query_cursor_next_match(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

query_cursor_remove_match(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

query_cursor_next_capture(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

query_cursor_set_max_start_depth(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

language_copy(_) ->
  erlang:nif_error(nif_library_not_loaded).

language_delete(_) ->
  erlang:nif_error(nif_library_not_loaded).

language_symbol_count(_) ->
  erlang:nif_error(nif_library_not_loaded).

language_state_count(_) ->
  erlang:nif_error(nif_library_not_loaded).

language_symbol_name(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

language_symbol_for_name(_, _, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

language_field_count(_) ->
  erlang:nif_error(nif_library_not_loaded).

language_field_name_for_id(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

language_field_id_for_name(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

language_symbol_type(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

language_version(_) ->
  erlang:nif_error(nif_library_not_loaded).

language_next_state(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

language_name(_) ->
  erlang:nif_error(nif_library_not_loaded).

lookahead_iterator_new(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

lookahead_iterator_delete(_) ->
  erlang:nif_error(nif_library_not_loaded).

lookahead_iterator_reset_state(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

lookahead_iterator_reset(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

lookahead_iterator_language(_) ->
  erlang:nif_error(nif_library_not_loaded).

lookahead_iterator_next(_) ->
  erlang:nif_error(nif_library_not_loaded).

lookahead_iterator_current_symbol(_) ->
  erlang:nif_error(nif_library_not_loaded).

lookahead_iterator_current_symbol_name(_) ->
  erlang:nif_error(nif_library_not_loaded).
