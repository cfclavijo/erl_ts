-module(erl_ts).

-export([init/0]).

-export([ tree_sitter_erlang/0,
          ts_parser_new/0,
          ts_parser_delete/1,
          ts_parser_language/1,
          ts_parser_set_language/2,
          ts_parser_set_included_ranges/3,
          ts_parser_included_ranges/1,
          ts_parser_parse/3,
          ts_parser_parse_with_options/4,
          ts_parser_parse_string/3,
          ts_parser_parse_string_encoding/5,
          ts_parser_reset/1,
          ts_parser_set_timeout_micros/2,
          ts_parser_timeout_micros/1,
          ts_parser_set_cancellation_flag/2,
          ts_parser_cancellation_flag/1,
          ts_parser_set_logger/2,
          ts_parser_logger/1,
          ts_parser_print_dot_graphs/2,
          ts_tree_copy/1,
          ts_tree_delete/1,
          ts_tree_root_node/1,
          ts_tree_root_node_with_offset/3,
          ts_tree_language/1,
          ts_tree_included_ranges/2,
          ts_tree_edit/2,
          ts_tree_get_changed_ranges/3,
          ts_tree_print_dot_graph/2,
          ts_node_type/1,
          ts_node_symbol/1,
          ts_node_language/1,
          ts_node_grammar_type/1,
          ts_node_grammar_symbol/1,
          ts_node_start_byte/1,
          ts_node_start_point/1,
          ts_node_end_byte/1,
          ts_node_end_point/1,
          ts_node_string/1,
          ts_node_is_null/1,
          ts_node_is_named/1,
          ts_node_is_missing/1,
          ts_node_is_extra/1,
          ts_node_has_changes/1,
          ts_node_has_error/1,
          ts_node_is_error/1,
          ts_node_parse_state/1,
          ts_node_next_parse_state/1,
          ts_node_parent/1,
          ts_node_child_containing_descendant/2,
          ts_node_child_with_descendant/2,
          ts_node_child/2,
          ts_node_field_name_for_child/2,
          ts_node_field_name_for_named_child/2,
          ts_node_child_count/1,
          ts_node_named_child/2,
          ts_node_named_child_count/1,
          ts_node_child_by_field_name/2,
          ts_node_child_by_field_id/2,
          ts_node_next_sibling/1,
          ts_node_prev_sibling/1,
          ts_node_next_named_sibling/1,
          ts_node_prev_named_sibling/1,
          ts_node_first_child_for_byte/2,
          ts_node_first_named_child_for_byte/2,
          ts_node_descendant_count/1,
          ts_node_descendant_for_byte_range/3,
          ts_node_descendant_for_point_range/3,
          ts_node_named_descendant_for_byte_range/3,
          ts_node_named_descendant_for_point_range/3,
          ts_node_edit/2,
          ts_node_eq/2,
          ts_node_text/2,
          ts_tree_cursor_new/1,
          ts_tree_cursor_delete/1,
          ts_tree_cursor_reset/2,
          ts_tree_cursor_reset_to/2,
          ts_tree_cursor_current_node/1,
          ts_tree_cursor_current_field_name/1,
          ts_tree_cursor_current_field_id/1,
          ts_tree_cursor_goto_parent/1,
          ts_tree_cursor_goto_next_sibling/1,
          ts_tree_cursor_goto_previous_sibling/1,
          ts_tree_cursor_goto_first_child/1,
          ts_tree_cursor_goto_last_child/1,
          ts_tree_cursor_goto_descendant/2,
          ts_tree_cursor_current_descendant_index/1,
          ts_tree_cursor_current_depth/1,
          ts_tree_cursor_goto_first_child_for_byte/2,
          ts_tree_cursor_goto_first_child_for_point/2,
          ts_tree_cursor_copy/1,
          ts_query_new/2,
          ts_query_delete/1,
          ts_query_pattern_count/1,
          ts_query_capture_count/1,
          ts_query_string_count/1,
          ts_query_start_byte_for_pattern/2,
          ts_query_end_byte_for_pattern/2,
          ts_query_predicates_for_pattern/3,
          ts_query_is_pattern_rooted/2,
          ts_query_is_pattern_non_local/2,
          ts_query_is_pattern_guaranteed_at_step/2,
          ts_query_capture_name_for_id/3,
          ts_query_capture_quantifier_for_id/3,
          ts_query_string_value_for_id/2,
          ts_query_disable_capture/3,
          ts_query_disable_pattern/2,
          ts_query_capture/2,
          ts_query_cursor_new/0,
          ts_query_cursor_delete/1,
          ts_query_cursor_exec/3,
          ts_query_cursor_exec_with_options/4,
          ts_query_cursor_did_exceed_match_limit/1,
          ts_query_cursor_match_limit/1,
          ts_query_cursor_set_match_limit/2,
          ts_query_cursor_set_timeout_micros/2,
          ts_query_cursor_timeout_micros/1,
          ts_query_cursor_set_byte_range/3,
          ts_query_cursor_set_point_range/3,
          ts_query_cursor_next_match/2,
          ts_query_cursor_remove_match/2,
          ts_query_cursor_next_capture/3,
          ts_query_cursor_set_max_start_depth/2,
          ts_language_copy/1,
          ts_language_delete/1,
          ts_language_symbol_count/1,
          ts_language_state_count/1,
          ts_language_symbol_name/2,
          ts_language_symbol_for_name/4,
          ts_language_field_count/1,
          ts_language_field_name_for_id/2,
          ts_language_field_id_for_name/2,
          ts_language_symbol_type/2,
          ts_language_version/1,
          ts_language_next_state/3,
          ts_language_name/1,
          ts_lookahead_iterator_new/2,
          ts_lookahead_iterator_delete/1,
          ts_lookahead_iterator_reset_state/2,
          ts_lookahead_iterator_reset/3,
          ts_lookahead_iterator_language/1,
          ts_lookahead_iterator_next/1,
          ts_lookahead_iterator_current_symbol/1,
          ts_lookahead_iterator_current_symbol_name/1
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

ts_parser_new() ->
  erlang:nif_error(nif_library_not_loaded).

ts_parser_delete(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_parser_language(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_parser_set_language(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_parser_set_included_ranges(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_parser_included_ranges(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_parser_parse(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_parser_parse_with_options(_, _, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_parser_parse_string(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_parser_parse_string_encoding(_, _, _, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_parser_reset(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_parser_set_timeout_micros(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_parser_timeout_micros(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_parser_set_cancellation_flag(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_parser_cancellation_flag(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_parser_set_logger(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_parser_logger(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_parser_print_dot_graphs(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_copy(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_delete(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_root_node(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_root_node_with_offset(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_language(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_included_ranges(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_edit(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_get_changed_ranges(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_print_dot_graph(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_type(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_symbol(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_language(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_grammar_type(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_grammar_symbol(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_start_byte(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_start_point(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_end_byte(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_end_point(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_string(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_is_null(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_is_named(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_is_missing(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_is_extra(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_has_changes(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_has_error(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_is_error(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_parse_state(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_next_parse_state(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_parent(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_child_containing_descendant(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_child_with_descendant(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_child(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_field_name_for_child(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_field_name_for_named_child(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_child_count(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_named_child(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_named_child_count(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_child_by_field_name(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_child_by_field_id(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_next_sibling(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_prev_sibling(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_next_named_sibling(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_prev_named_sibling(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_first_child_for_byte(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_first_named_child_for_byte(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_descendant_count(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_descendant_for_byte_range(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_descendant_for_point_range(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_named_descendant_for_byte_range(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_named_descendant_for_point_range(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_edit(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_eq(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_node_text(Node, SourceCode) ->
  Start = ts_node_start_byte(Node),
  End = ts_node_end_byte(Node),
  string:sub_string(SourceCode, Start + 1, End).

ts_tree_cursor_new(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_cursor_delete(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_cursor_reset(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_cursor_reset_to(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_cursor_current_node(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_cursor_current_field_name(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_cursor_current_field_id(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_cursor_goto_parent(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_cursor_goto_next_sibling(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_cursor_goto_previous_sibling(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_cursor_goto_first_child(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_cursor_goto_last_child(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_cursor_goto_descendant(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_cursor_current_descendant_index(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_cursor_current_depth(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_cursor_goto_first_child_for_byte(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_cursor_goto_first_child_for_point(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_tree_cursor_copy(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_new(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_delete(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_pattern_count(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_capture_count(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_string_count(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_start_byte_for_pattern(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_end_byte_for_pattern(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_predicates_for_pattern(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_is_pattern_rooted(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_is_pattern_non_local(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_is_pattern_guaranteed_at_step(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_capture_name_for_id(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_capture_quantifier_for_id(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_string_value_for_id(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_disable_capture(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_disable_pattern(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_capture(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_cursor_new() ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_cursor_delete(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_cursor_exec(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_cursor_exec_with_options(_, _, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_cursor_did_exceed_match_limit(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_cursor_match_limit(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_cursor_set_match_limit(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_cursor_set_timeout_micros(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_cursor_timeout_micros(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_cursor_set_byte_range(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_cursor_set_point_range(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_cursor_next_match(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_cursor_remove_match(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_cursor_next_capture(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_query_cursor_set_max_start_depth(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_language_copy(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_language_delete(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_language_symbol_count(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_language_state_count(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_language_symbol_name(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_language_symbol_for_name(_, _, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_language_field_count(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_language_field_name_for_id(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_language_field_id_for_name(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_language_symbol_type(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_language_version(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_language_next_state(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_language_name(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_lookahead_iterator_new(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_lookahead_iterator_delete(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_lookahead_iterator_reset_state(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_lookahead_iterator_reset(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

ts_lookahead_iterator_language(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_lookahead_iterator_next(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_lookahead_iterator_current_symbol(_) ->
  erlang:nif_error(nif_library_not_loaded).

ts_lookahead_iterator_current_symbol_name(_) ->
  erlang:nif_error(nif_library_not_loaded).
