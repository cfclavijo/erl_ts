/* erl_ts_nif.c */

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <tree_sitter/api.h>
#include <erl_nif.h>
#include <tree_sitter/api.h>

#define ERL_TS_FUNCTION_DECL(f) \
  static ERL_NIF_TERM f(ErlNifEnv* , int, const ERL_NIF_TERM []);

#define ERL_TS_FUNCTION(f) \
  static ERL_NIF_TERM f(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])

#define ERL_TS_FUNCTION_ARRAY(f , a) { #f, a, f ## _nif }

#define RETURN_BADARG_IF(p) if (p) return enif_make_badarg(env)

/* for testing purpose  */
const TSLanguage *tree_sitter_erlang(void);

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;
static ERL_NIF_TERM atom_undefined;
static ERL_NIF_TERM atom_row;
static ERL_NIF_TERM atom_column;
static ERL_NIF_TERM atom_start_point;
static ERL_NIF_TERM atom_end_point;
static ERL_NIF_TERM atom_old_end_point;
static ERL_NIF_TERM atom_start_byte;
static ERL_NIF_TERM atom_end_byte;
static ERL_NIF_TERM atom_old_end_byte;
static ERL_NIF_TERM atom_TSSymbolTypeRegular;
static ERL_NIF_TERM atom_TSSymbolTypeAnonymus;
static ERL_NIF_TERM atom_TSSymbolTypeSuperType;
static ERL_NIF_TERM atom_TSSymbolTypeAuxiliary;
static ERL_NIF_TERM atom_TSQueryErrorNone;
static ERL_NIF_TERM atom_TSQueryErrorSyntax;
static ERL_NIF_TERM atom_TSQueryErrorNodeType;
static ERL_NIF_TERM atom_TSQueryErrorField;
static ERL_NIF_TERM atom_TSQueryErrorCapture;
static ERL_NIF_TERM atom_TSQueryErrorStructure;
static ERL_NIF_TERM atom_TSQueryErrorLanguage;

static ERL_NIF_TERM
mk_atom(ErlNifEnv *env, const char *atom) {
  ERL_NIF_TERM ret;
  if (!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1)) {
    return enif_make_atom(env, atom);
  }
  return ret;
}

static ERL_NIF_TERM
mk_error(ErlNifEnv *env, const char *msg) {
  return enif_make_tuple2(env, atom_error, mk_atom(env, msg));
}

#define ERL_TS_COMP_LANGUAGE_VERSION 14

/* TODO: implement those resources */
/* TSQueryCursor       */
/* TSLookaheadIterator */


/************************/
/* Resource declaration */
/************************/

ErlNifResourceType *res_TSLanguage = NULL;
ErlNifResourceType *res_TSParser = NULL;
ErlNifResourceType *res_TSTree = NULL;
ErlNifResourceType *res_TSQuery = NULL;
ErlNifResourceType *res_TSNode = NULL;
ErlNifResourceType *res_TSQueryCursor = NULL;
ErlNifResourceType *res_TSTreeCursor = NULL;

void free_TSLanguage(ErlNifEnv*, void *);
void free_TSParser(ErlNifEnv *, void *);
void free_TSTree(ErlNifEnv *, void *);
void free_TSQuery(ErlNifEnv *, void *);
void free_TSNode(ErlNifEnv *, void *);
void free_TSQueryCursor(ErlNifEnv *, void *);
void free_TSTreeCursor(ErlNifEnv *, void *);

typedef struct {
  const TSLanguage* val;
} struct_TSLanguage;

typedef struct {
  TSParser* val;
} struct_TSParser;

typedef struct {
  TSTree* val;
} struct_TSTree;

typedef struct {
  TSQuery* val;
} struct_TSQuery;

typedef struct {
  TSNode val;
} struct_TSNode;

typedef struct {
  TSQueryCursor* val;
} struct_TSQueryCursor;

typedef struct {
  TSTreeCursor val;
} struct_TSTreeCursor;

void free_TSLanguage(ErlNifEnv *env, void *object) {
  const TSLanguage* lang = ((struct_TSLanguage *)object)->val;
  ts_language_delete(lang);
}

void free_TSParser(ErlNifEnv *env, void *object) {
  TSParser *parser = ((struct_TSParser *)object)->val;
  ts_parser_delete(parser);
}

void free_TSTree(ErlNifEnv *env, void *object) {
  TSTree *tree = ((struct_TSTree *)object)->val;
  ts_tree_delete(tree);
}

void free_TSQuery(ErlNifEnv *env, void *object) {
  TSQuery *query = ((struct_TSQuery *)object)->val;
  ts_query_delete(query);
}

void free_TSNode(ErlNifEnv *env, void *object) {
  /* TSNode is no longer a pointer, no free required */
  /* TSNode *node = ((struct_TSNode *)object)->val; */
  /* enif_free(node); */
}

void free_TSQueryCursor(ErlNifEnv *env, void *object) {
  TSQueryCursor *qcursor = ((struct_TSQueryCursor *)object)->val;
  ts_query_cursor_delete(qcursor);
}

void free_TSTreeCursor(ErlNifEnv *env, void *object) {
  TSTreeCursor *tcursor = &((struct_TSTreeCursor *)object)->val;
  ts_tree_cursor_delete(tcursor);
}


/********************************************************************/
/* Struct utils definition */
/********************************************************************/
int map_to_tspoint(ErlNifEnv *, ERL_NIF_TERM, TSPoint *);
ERL_NIF_TERM tspoint_to_map(ErlNifEnv *, const TSPoint);
int map_to_tsrange(ErlNifEnv *, ERL_NIF_TERM, TSRange *);
ERL_NIF_TERM tsrange_to_map(ErlNifEnv *, TSRange);

int map_to_tsinput_edit(ErlNifEnv *, ERL_NIF_TERM, TSInputEdit *);
ERL_NIF_TERM tsinput_edit_to_map(ErlNifEnv *, TSInputEdit);

ERL_NIF_TERM tssymbol_type_to_atom(ErlNifEnv *, TSSymbolType);
int atom_to_tssymbol_type(ERL_NIF_TERM, TSSymbolType *);
ERL_NIF_TERM tsquery_error_to_atom(ErlNifEnv *, TSQueryError);
int atom_to_tsquery_error(ERL_NIF_TERM, TSQueryError *);

int map_to_tspoint(ErlNifEnv *env, ERL_NIF_TERM map, TSPoint* tspoint) {
  ERL_NIF_TERM row, column;
  if (!enif_get_map_value(env, map, atom_row, &row))
    return 0;
  if (!enif_get_map_value(env, map, atom_column, &column))
    return 0;

  if (!enif_get_uint(env, row, &tspoint->row))
    return 0;
  if (!enif_get_uint(env, column, &tspoint->column))
    return 0;

  return 1;
}

ERL_NIF_TERM tspoint_to_map(ErlNifEnv *env, const TSPoint tspoint) {
  ERL_NIF_TERM map = enif_make_new_map(env);
  enif_make_map_put(env, map, atom_row, enif_make_int(env, tspoint.row), &map);
  enif_make_map_put(env, map, atom_column, enif_make_int(env, tspoint.column), &map);

  return map;
}

int map_to_tsrange(ErlNifEnv *env, ERL_NIF_TERM map, TSRange *tsrange) {
  ERL_NIF_TERM start_point, end_point, start_byte, end_byte;
  if (!enif_get_map_value(env, map, atom_start_point, &start_point))
    return 0;
  if (!enif_get_map_value(env, map, atom_end_point, &end_point))
    return 0;
  if (!enif_get_map_value(env, map, atom_start_byte, &start_byte))
    return 0;
  if (!enif_get_map_value(env, map, atom_end_byte, &end_byte))
    return 0;

  if (!map_to_tspoint(env, start_point, &tsrange->start_point))
    return enif_make_badarg(env);
  if (!map_to_tspoint(env, end_point, &tsrange->end_point))
    return enif_make_badarg(env);
  if (!enif_get_uint(env, start_byte, &tsrange->start_byte))
    return 0;
  if (!enif_get_uint(env, end_byte, &tsrange->end_byte))
    return 0;

  return 1;
}

ERL_NIF_TERM tsrange_to_map(ErlNifEnv *env, TSRange tsrange) {
  ERL_NIF_TERM map = enif_make_new_map(env);
  ERL_NIF_TERM start_point, end_point, start_byte, end_byte;
  start_point = tspoint_to_map(env, tsrange.start_point);
  end_point = tspoint_to_map(env, tsrange.end_point);
  start_byte = enif_make_uint(env, tsrange.start_byte);
  end_byte = enif_make_uint(env, tsrange.end_byte);
  enif_make_map_put(env, map, atom_start_point, start_point, &map);
  enif_make_map_put(env, map, atom_end_point, end_point, &map);
  enif_make_map_put(env, map, atom_start_byte, start_byte, &map);
  enif_make_map_put(env, map, atom_end_byte, end_byte, &map);
  return map;
}

int map_to_tsinput_edit(ErlNifEnv *env, ERL_NIF_TERM map, TSInputEdit *tsinput) {
  ERL_NIF_TERM start_byte, end_byte, old_end_byte;
  ERL_NIF_TERM start_point, end_point, old_end_point;
  if (!enif_get_map_value(env, map, atom_start_byte, &start_byte))
    return 0;
  if (!enif_get_map_value(env, map, atom_end_byte, &end_byte))
    return 0;
  if (!enif_get_map_value(env, map, atom_old_end_byte, &old_end_byte))
    return 0;

  if (!enif_get_map_value(env, map, atom_start_point, &start_point))
    return 0;
  if (!enif_get_map_value(env, map, atom_end_point, &end_point))
    return 0;
  if (!enif_get_map_value(env, map, atom_old_end_point, &old_end_point))
    return 0;

  if (!map_to_tspoint(env, start_point, &tsinput->start_point))
    return 0;
  if (!map_to_tspoint(env, end_point, &tsinput->new_end_point))
    return 0;
  if (!map_to_tspoint(env, old_end_point, &tsinput->old_end_point))
    return 0;
  if (!enif_get_uint(env, start_byte, &tsinput->start_byte))
    return 0;
  if (!enif_get_uint(env, end_byte, &tsinput->new_end_byte))
    return 0;
  if (!enif_get_uint(env, old_end_byte, &tsinput->old_end_byte))
    return 0;

  return 1;
}

ERL_NIF_TERM tssymbol_type_to_atom(ErlNifEnv *env, TSSymbolType type) {
  /* TODO: TREE_SITTER_LANGUAGE_VERSION > 14 */
  /* if (TSSymbolTypeSupertype == type) */
  /*     return atom_symbol_type_supertype; */
  if (TSSymbolTypeRegular == type)
    return atom_TSSymbolTypeRegular;
  if (TSSymbolTypeAnonymous == type)
    return atom_TSSymbolTypeAnonymus;
  if (TSSymbolTypeAuxiliary == type)
    return atom_TSSymbolTypeAuxiliary;
  return atom_undefined;
}

int atom_to_tssymbol_type(ERL_NIF_TERM atom, TSSymbolType *type) {
  /* TODO: TREE_SITTER_LANGUAGE_VERSION > 14 */
  /* if (enif_is_identical(atom_symbol_type_regular, atom)) { */
  /*   *type = TSSymbolTypeRegular; */
  /*   return 1; */
  /* } */
  if (enif_is_identical(atom_TSSymbolTypeRegular, atom)) {
    *type = TSSymbolTypeRegular;
    return 1;
  }
  if (enif_is_identical(atom_TSSymbolTypeAnonymus, atom)) {
    *type = TSSymbolTypeAnonymous;
    return 1;
  }
  if (enif_is_identical(atom_TSSymbolTypeAuxiliary, atom)) {
    *type = TSSymbolTypeAuxiliary;
    return 1;
  }
  return 0;
}

ERL_NIF_TERM tsquery_error_to_atom(ErlNifEnv *env, TSQueryError error) {
  if (TSQueryErrorNone == error) return atom_TSQueryErrorNone;
  if (TSQueryErrorSyntax == error) return atom_TSQueryErrorSyntax;
  if (TSQueryErrorNodeType == error) return atom_TSQueryErrorNodeType;
  if (TSQueryErrorField == error) return atom_TSQueryErrorField;
  if (TSQueryErrorCapture == error) return atom_TSQueryErrorCapture;
  if (TSQueryErrorStructure == error) return atom_TSQueryErrorStructure;
  if (TSQueryErrorLanguage == error) return atom_TSQueryErrorLanguage;
  return atom_undefined;
}

int atom_to_tsquery_error(ERL_NIF_TERM atom, TSQueryError *error) {
  if (enif_is_identical(atom_TSQueryErrorNone, atom)) {
    *error = TSQueryErrorNone;
    return 1;
  }
  if (enif_is_identical(atom_TSQueryErrorSyntax, atom)) {
    *error = TSQueryErrorSyntax;
    return 1;
  }
  if (enif_is_identical(atom_TSQueryErrorNodeType, atom)) {
    *error = TSQueryErrorNodeType;
    return 1;
  }
  if (enif_is_identical(atom_TSQueryErrorField, atom)) {
    *error = TSQueryErrorField;
    return 1;
  }
  if (enif_is_identical(atom_TSQueryErrorCapture, atom)) {
    *error = TSQueryErrorCapture;
    return 1;
  }
  if (enif_is_identical(atom_TSQueryErrorStructure, atom)) {
    *error = TSQueryErrorStructure;
    return 1;
  }
  if (enif_is_identical(atom_TSQueryErrorLanguage, atom)) {
    *error = TSQueryErrorLanguage;
    return 1;
  }
  return 0;
}


/********************************************************************/
/* Functions declaration */
/********************************************************************/
ERL_TS_FUNCTION_DECL(parser_new_nif)
ERL_TS_FUNCTION_DECL(parser_delete_nif)
ERL_TS_FUNCTION_DECL(parser_language_nif)
ERL_TS_FUNCTION_DECL(parser_set_language_nif)
ERL_TS_FUNCTION_DECL(parser_set_included_ranges_nif)
ERL_TS_FUNCTION_DECL(parser_included_ranges_nif)
ERL_TS_FUNCTION_DECL(parser_parse_nif)
ERL_TS_FUNCTION_DECL(parser_parse_with_options_nif)
ERL_TS_FUNCTION_DECL(parser_parse_string_nif)
ERL_TS_FUNCTION_DECL(parser_reparse_string_nif)
ERL_TS_FUNCTION_DECL(parser_parse_string_encoding_nif)
ERL_TS_FUNCTION_DECL(parser_reset_nif)
ERL_TS_FUNCTION_DECL(parser_set_timeout_micros_nif)
ERL_TS_FUNCTION_DECL(parser_timeout_micros_nif)
ERL_TS_FUNCTION_DECL(parser_set_cancellation_flag_nif)
ERL_TS_FUNCTION_DECL(parser_cancellation_flag_nif)
ERL_TS_FUNCTION_DECL(parser_set_logger_nif)
ERL_TS_FUNCTION_DECL(parser_logger_nif)
ERL_TS_FUNCTION_DECL(parser_print_dot_graphs_nif)
ERL_TS_FUNCTION_DECL(tree_copy_nif)
ERL_TS_FUNCTION_DECL(tree_delete_nif)
ERL_TS_FUNCTION_DECL(tree_root_node_nif)
ERL_TS_FUNCTION_DECL(tree_root_node_with_offset_nif)
ERL_TS_FUNCTION_DECL(tree_language_nif)
ERL_TS_FUNCTION_DECL(tree_included_ranges_nif)
ERL_TS_FUNCTION_DECL(tree_edit_nif)
ERL_TS_FUNCTION_DECL(tree_get_changed_ranges_nif)
ERL_TS_FUNCTION_DECL(tree_print_dot_graph_nif)
ERL_TS_FUNCTION_DECL(node_type_nif)
ERL_TS_FUNCTION_DECL(node_symbol_nif)
ERL_TS_FUNCTION_DECL(node_language_nif)
ERL_TS_FUNCTION_DECL(node_grammar_type_nif)
ERL_TS_FUNCTION_DECL(node_grammar_symbol_nif)
ERL_TS_FUNCTION_DECL(node_start_byte_nif)
ERL_TS_FUNCTION_DECL(node_start_point_nif)
ERL_TS_FUNCTION_DECL(node_end_byte_nif)
ERL_TS_FUNCTION_DECL(node_end_point_nif)
ERL_TS_FUNCTION_DECL(node_string_nif)
ERL_TS_FUNCTION_DECL(node_is_null_nif)
ERL_TS_FUNCTION_DECL(node_is_named_nif)
ERL_TS_FUNCTION_DECL(node_is_missing_nif)
ERL_TS_FUNCTION_DECL(node_is_extra_nif)
ERL_TS_FUNCTION_DECL(node_has_changes_nif)
ERL_TS_FUNCTION_DECL(node_has_error_nif)
ERL_TS_FUNCTION_DECL(node_is_error_nif)
ERL_TS_FUNCTION_DECL(node_parse_state_nif)
ERL_TS_FUNCTION_DECL(node_next_parse_state_nif)
ERL_TS_FUNCTION_DECL(node_parent_nif)
ERL_TS_FUNCTION_DECL(node_child_containing_descendant_nif)
ERL_TS_FUNCTION_DECL(node_child_with_descendant_nif)
ERL_TS_FUNCTION_DECL(node_child_nif)
ERL_TS_FUNCTION_DECL(node_field_name_for_child_nif)
ERL_TS_FUNCTION_DECL(node_field_name_for_named_child_nif)
ERL_TS_FUNCTION_DECL(node_child_count_nif)
ERL_TS_FUNCTION_DECL(node_named_child_nif)
ERL_TS_FUNCTION_DECL(node_named_child_count_nif)
ERL_TS_FUNCTION_DECL(node_child_by_field_name_nif)
ERL_TS_FUNCTION_DECL(node_child_by_field_id_nif)
ERL_TS_FUNCTION_DECL(node_next_sibling_nif)
ERL_TS_FUNCTION_DECL(node_prev_sibling_nif)
ERL_TS_FUNCTION_DECL(node_next_named_sibling_nif)
ERL_TS_FUNCTION_DECL(node_prev_named_sibling_nif)
ERL_TS_FUNCTION_DECL(node_first_child_for_byte_nif)
ERL_TS_FUNCTION_DECL(node_first_named_child_for_byte_nif)
ERL_TS_FUNCTION_DECL(node_descendant_count_nif)
ERL_TS_FUNCTION_DECL(node_descendant_for_byte_range_nif)
ERL_TS_FUNCTION_DECL(node_descendant_for_point_range_nif)
ERL_TS_FUNCTION_DECL(node_named_descendant_for_byte_range_nif)
ERL_TS_FUNCTION_DECL(node_named_descendant_for_point_range_nif)
ERL_TS_FUNCTION_DECL(node_edit_nif)
ERL_TS_FUNCTION_DECL(node_eq_nif)
ERL_TS_FUNCTION_DECL(tree_cursor_new_nif)
ERL_TS_FUNCTION_DECL(tree_cursor_delete_nif)
ERL_TS_FUNCTION_DECL(tree_cursor_reset_nif)
ERL_TS_FUNCTION_DECL(tree_cursor_reset_to_nif)
ERL_TS_FUNCTION_DECL(tree_cursor_current_node_nif)
ERL_TS_FUNCTION_DECL(tree_cursor_current_field_name_nif)
ERL_TS_FUNCTION_DECL(tree_cursor_current_field_id_nif)
ERL_TS_FUNCTION_DECL(tree_cursor_goto_parent_nif)
ERL_TS_FUNCTION_DECL(tree_cursor_goto_next_sibling_nif)
ERL_TS_FUNCTION_DECL(tree_cursor_goto_previous_sibling_nif)
ERL_TS_FUNCTION_DECL(tree_cursor_goto_first_child_nif)
ERL_TS_FUNCTION_DECL(tree_cursor_goto_last_child_nif)
ERL_TS_FUNCTION_DECL(tree_cursor_goto_descendant_nif)
ERL_TS_FUNCTION_DECL(tree_cursor_current_descendant_index_nif)
ERL_TS_FUNCTION_DECL(tree_cursor_current_depth_nif)
ERL_TS_FUNCTION_DECL(tree_cursor_goto_first_child_for_byte_nif)
ERL_TS_FUNCTION_DECL(tree_cursor_goto_first_child_for_point_nif)
ERL_TS_FUNCTION_DECL(tree_cursor_copy_nif)
ERL_TS_FUNCTION_DECL(query_new_nif)
ERL_TS_FUNCTION_DECL(query_delete_nif)
ERL_TS_FUNCTION_DECL(query_pattern_count_nif)
ERL_TS_FUNCTION_DECL(query_capture_count_nif)
ERL_TS_FUNCTION_DECL(query_string_count_nif)
ERL_TS_FUNCTION_DECL(query_start_byte_for_pattern_nif)
ERL_TS_FUNCTION_DECL(query_end_byte_for_pattern_nif)
ERL_TS_FUNCTION_DECL(query_predicates_for_pattern_nif)
ERL_TS_FUNCTION_DECL(query_is_pattern_rooted_nif)
ERL_TS_FUNCTION_DECL(query_is_pattern_non_local_nif)
ERL_TS_FUNCTION_DECL(query_is_pattern_guaranteed_at_step_nif)
ERL_TS_FUNCTION_DECL(query_capture_name_for_id_nif)
ERL_TS_FUNCTION_DECL(query_capture_quantifier_for_id_nif)
ERL_TS_FUNCTION_DECL(query_string_value_for_id_nif)
ERL_TS_FUNCTION_DECL(query_disable_capture_nif)
ERL_TS_FUNCTION_DECL(query_disable_pattern_nif)
ERL_TS_FUNCTION_DECL(query_capture_nif)
ERL_TS_FUNCTION_DECL(query_cursor_new_nif)
ERL_TS_FUNCTION_DECL(query_cursor_delete_nif)
ERL_TS_FUNCTION_DECL(query_cursor_exec_nif)
ERL_TS_FUNCTION_DECL(query_cursor_exec_with_options_nif)
ERL_TS_FUNCTION_DECL(query_cursor_did_exceed_match_limit_nif)
ERL_TS_FUNCTION_DECL(query_cursor_match_limit_nif)
ERL_TS_FUNCTION_DECL(query_cursor_set_match_limit_nif)
ERL_TS_FUNCTION_DECL(query_cursor_set_timeout_micros_nif)
ERL_TS_FUNCTION_DECL(query_cursor_timeout_micros_nif)
ERL_TS_FUNCTION_DECL(query_cursor_set_byte_range_nif)
ERL_TS_FUNCTION_DECL(query_cursor_set_point_range_nif)
ERL_TS_FUNCTION_DECL(query_cursor_next_match_nif)
ERL_TS_FUNCTION_DECL(query_cursor_remove_match_nif)
ERL_TS_FUNCTION_DECL(query_cursor_next_capture_nif)
ERL_TS_FUNCTION_DECL(query_cursor_set_max_start_depth_nif)
ERL_TS_FUNCTION_DECL(language_copy_nif)
ERL_TS_FUNCTION_DECL(language_delete_nif)
ERL_TS_FUNCTION_DECL(language_symbol_count_nif)
ERL_TS_FUNCTION_DECL(language_state_count_nif)
ERL_TS_FUNCTION_DECL(language_symbol_name_nif)
ERL_TS_FUNCTION_DECL(language_symbol_for_name_nif)
ERL_TS_FUNCTION_DECL(language_field_count_nif)
ERL_TS_FUNCTION_DECL(language_field_name_for_id_nif)
ERL_TS_FUNCTION_DECL(language_field_id_for_name_nif)
ERL_TS_FUNCTION_DECL(language_symbol_type_nif)
ERL_TS_FUNCTION_DECL(language_version_nif)
ERL_TS_FUNCTION_DECL(language_next_state_nif)
ERL_TS_FUNCTION_DECL(language_name_nif)
ERL_TS_FUNCTION_DECL(lookahead_iterator_new_nif)
ERL_TS_FUNCTION_DECL(lookahead_iterator_delete_nif)
ERL_TS_FUNCTION_DECL(lookahead_iterator_reset_state_nif)
ERL_TS_FUNCTION_DECL(lookahead_iterator_reset_nif)
ERL_TS_FUNCTION_DECL(lookahead_iterator_language_nif)
ERL_TS_FUNCTION_DECL(lookahead_iterator_next_nif)
ERL_TS_FUNCTION_DECL(lookahead_iterator_current_symbol_nif)
ERL_TS_FUNCTION_DECL(lookahead_iterator_current_symbol_name_nif)

ERL_TS_FUNCTION_DECL(tree_sitter_erlang_nif)


/********************************************************************/
/* Functions definition  */
/********************************************************************/


/********************/
/* Section - Parser */
/********************/

ERL_TS_FUNCTION(parser_new_nif) {
  TSParser* parser = ts_parser_new();
  if (!parser) return mk_error(env, "unable_to_create_new_parser");

  struct_TSParser *res_parser =
    enif_alloc_resource(res_TSParser, sizeof(struct_TSParser));
  res_parser->val = parser;
  ERL_NIF_TERM term_parser = enif_make_resource(env, res_parser);
  enif_release_resource(res_parser);
  return enif_make_tuple2(env, atom_ok, term_parser);
}

ERL_TS_FUNCTION(parser_delete_nif) {
  /* TODO: */
  /* void ts_parser_delete_nif(TSParser *self) */
  return atom_undefined;
}

ERL_TS_FUNCTION(parser_language_nif) {
  void* res_parser = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSParser, &res_parser));
  TSParser* parser = ((struct_TSParser*)res_parser)->val;
  const TSLanguage *language = ts_parser_language(parser);

  struct_TSLanguage *res_language =
    enif_alloc_resource(res_TSLanguage, sizeof(struct_TSLanguage));
  res_language->val = language;

  ERL_NIF_TERM term_language = enif_make_resource(env, res_language);
  enif_release_resource(res_language);
  return enif_make_tuple2(env, atom_ok, term_language);
}

ERL_TS_FUNCTION(parser_set_language_nif) {
  void *res_parser = NULL;
  void *res_language = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSParser, &res_parser));
  RETURN_BADARG_IF(!enif_get_resource(env, argv[1], res_TSLanguage, &res_language));

  TSParser *parser = ((struct_TSParser *)res_parser)->val;
  const TSLanguage *language = ((struct_TSLanguage *)res_language)->val;

  bool result = ts_parser_set_language(parser, language);
  return result ? atom_true : atom_false;
}

ERL_TS_FUNCTION(parser_set_included_ranges_nif) {
  /* TODO: */
  /* bool ts_parser_set_included_ranges( */
  /* TSParser *self, */
  /* const TSRange *ranges, */
  /* uint32_t count */
  return atom_undefined;
}

ERL_TS_FUNCTION(parser_included_ranges_nif) {
  /* const TSRange *ts_parser_included_ranges( */
  /*   const TSParser *self, */
  /*   uint32_t *count */
  /* ); */
  void *res_tsparser = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSParser, &res_tsparser));
  TSParser *tsparser = ((struct_TSParser *)res_tsparser)->val;

  uint32_t count;

  const TSRange *ranges = ts_parser_included_ranges(tsparser, &count);
  RETURN_BADARG_IF(!ranges);

  ERL_NIF_TERM list = enif_make_list(env, 0);
  for (uint32_t i = 0; i < count; i++) {
    ERL_NIF_TERM range_map = tsrange_to_map(env, ranges[i]);
    list = enif_make_list_cell(env, range_map, list);
  }
  ERL_NIF_TERM rev_list;
  enif_make_reverse_list(env, list, &rev_list);
  return rev_list;
}

ERL_TS_FUNCTION(parser_parse_nif) {
  /* TODO: */
  /* TSTree *ts_parser_parse( */
  /*   TSParser *self, */
  /*   const TSTree *old_tree, */
  /*   TSInput input */
  /* ); */
  return atom_undefined;
}

ERL_TS_FUNCTION(parser_parse_with_options_nif) {
  /* TODO: */
  /* TSTree* ts_parser_parse_with_options( */
  /* TSParser *self, */
  /* const TSTree *old_tree, */
  /* TSInput input, */
  /* TSParseOptions parse_options */
  /*                                      ); */
  return atom_undefined;
}

ERL_TS_FUNCTION(parser_parse_string_nif) {
  /* deal with the old_tree using parser_reparse_string_nif/3 */
  /*   TSTree *ts_parser_parse_string( */
  /*   TSParser *self, */
  /*   const TSTree *old_tree, */
  /*   const char *string, */
  /*   uint32_t length */
  /* ); */
  void *res_parser = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSParser, &res_parser));
  TSParser *parser = ((struct_TSParser *)res_parser)->val;

  char *in_string = NULL;
  unsigned int in_string_length;
  RETURN_BADARG_IF(!enif_get_list_length(env, argv[1], &in_string_length));
  in_string = (char *)enif_alloc(in_string_length + 1);
  if (!enif_get_string(env, argv[1], in_string, in_string_length + 1, ERL_NIF_LATIN1)) {
    enif_free(in_string);
    return enif_make_badarg(env);
  }

  TSTree *tree = ts_parser_parse_string(parser, NULL, in_string, in_string_length);
  struct_TSTree *res_tree =
    enif_alloc_resource(res_TSTree, sizeof(struct_TSTree));
  res_tree->val = tree;
  ERL_NIF_TERM term_tree = enif_make_resource(env, res_tree);
  enif_release_resource(res_tree);
  return term_tree;
}

ERL_TS_FUNCTION(parser_reparse_string_nif) {
  /* TODO: deal with the old_tree */
  /*   TSTree *ts_parser_parse_string( */
  /*   TSParser *self, */
  /*   const TSTree *old_tree, */
  /*   const char *string, */
  /*   uint32_t length */
  /* ); */
  void *res_parser = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSParser, &res_parser));
  TSParser *parser = ((struct_TSParser *)res_parser)->val;

  void *res_old_tree = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[1], res_TSTree, &res_old_tree));
  TSTree *old_tree = ((struct_TSTree *)res_old_tree)->val;

  char *in_string = NULL;
  unsigned int in_string_length;
  RETURN_BADARG_IF(!enif_get_list_length(env, argv[2], &in_string_length));
  in_string = (char *)enif_alloc(in_string_length + 1);
  if (!enif_get_string(env, argv[2], in_string, in_string_length + 1, ERL_NIF_LATIN1)) {
    enif_free(in_string);
    return enif_make_badarg(env);
  }

  TSTree *tree = ts_parser_parse_string(parser, old_tree, in_string, in_string_length);
  struct_TSTree *res_tree =
    enif_alloc_resource(res_TSTree, sizeof(struct_TSTree));
  res_tree->val = tree;
  ERL_NIF_TERM term_tree = enif_make_resource(env, res_tree);
  enif_release_resource(res_tree);
  return term_tree;
}

ERL_TS_FUNCTION(parser_parse_string_encoding_nif) {
  /* TODO: */
  /* TSTree *ts_parser_parse_string_encoding( */
  /*   TSParser *self, */
  /*   const TSTree *old_tree, */
  /*   const char *string, */
  /*   uint32_t length, */
  /*   TSInputEncoding encoding */
  /* ); */
  return atom_undefined;
}

ERL_TS_FUNCTION(parser_reset_nif) {
  /* void ts_parser_reset(TSParser *self); */
  void *res_tsparser = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSParser, &res_tsparser));
  TSParser *tsparser = ((struct_TSParser *)res_tsparser)->val;

  ts_parser_reset(tsparser);
  return atom_ok;
}

ERL_TS_FUNCTION(parser_set_timeout_micros_nif) {
  /* void ts_parser_set_timeout_micros(TSParser *self, uint64_t timeout_micros); */
  void *res_tsparser = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSParser, &res_tsparser));
  TSParser *tsparser = ((struct_TSParser *)res_tsparser)->val;

  uint64_t timeout_micros;
  RETURN_BADARG_IF(!enif_get_uint64(env, argv[1], &timeout_micros));

  ts_parser_set_timeout_micros(tsparser, timeout_micros);
  return atom_ok;
}

ERL_TS_FUNCTION(parser_timeout_micros_nif) {
  /* uint64_t ts_parser_timeout_micros(const TSParser *self); */
  void *res_tsparser = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSParser, &res_tsparser));
  TSParser *tsparser = ((struct_TSParser *)res_tsparser)->val;

  uint64_t result = ts_parser_timeout_micros(tsparser);
  return enif_make_uint64(env, result);
}

ERL_TS_FUNCTION(parser_set_cancellation_flag_nif) {
  /* TODO: */
  /* void ts_parser_set_cancellation_flag(TSParser *self, const size_t *flag); */
  return atom_undefined;
}

ERL_TS_FUNCTION(parser_cancellation_flag_nif) {
  /* TODO: */
  /* const size_t *ts_parser_cancellation_flag(const TSParser *self); */
  return atom_undefined;
}

ERL_TS_FUNCTION(parser_set_logger_nif) {
  /* TODO: */
  /* void ts_parser_set_logger(TSParser *self, TSLogger logger); */
  return atom_undefined;
}

ERL_TS_FUNCTION(parser_logger_nif) {
  /* TODO: */
  /* TSLogger ts_parser_logger(const TSParser *self); */
  return atom_undefined;
}

ERL_TS_FUNCTION(parser_print_dot_graphs_nif) {
  /* TODO: */
  /* void ts_parser_print_dot_graphs(TSParser *self, int fd); */
  return atom_undefined;
}

/******************/
/* Section - Tree */
/******************/

ERL_TS_FUNCTION(tree_copy_nif) {
  /* TSTree *ts_tree_copy(const TSTree *self); */
  void *res_tstree = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSTree, &res_tstree));
  const TSTree *tstree = ((struct_TSTree *)res_tstree)->val;

  TSTree *result = ts_tree_copy(tstree);
  struct_TSTree *res_result = enif_alloc_resource(res_TSTree, sizeof(struct_TSTree));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(tree_delete_nif) {
  /* TODO: maybe we don´t need to implement it */
  /* void ts_tree_delete(TSTree *self); */
  return atom_undefined;
}

ERL_TS_FUNCTION(tree_root_node_nif) {
  /* TSNode ts_tree_root_node(const TSTree *self); */
  void *res_tree = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSTree, &res_tree));
  TSTree *tree = ((struct_TSTree *)res_tree)->val;
  TSNode node = ts_tree_root_node(tree);
  struct_TSNode *res_node =
    enif_alloc_resource(res_TSNode, sizeof(struct_TSNode));
  res_node->val = node;
  ERL_NIF_TERM term_node = enif_make_resource(env, res_node);
  enif_release_resource(res_node);
  return term_node;
}

ERL_TS_FUNCTION(tree_root_node_with_offset_nif) {
  /* TODO: */
  /*   TSNode ts_tree_root_node_with_offset( */
  /*   const TSTree *self, */
  /*   uint32_t offset_bytes, */
  /*   TSPoint offset_extent */
  /* ); */
  return atom_undefined;
}

ERL_TS_FUNCTION(tree_language_nif) {
  /* const TSLanguage *ts_tree_language(const TSTree *self); */
  void *res_tstree = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSTree, &res_tstree));
  const TSTree *tstree = ((struct_TSTree *)res_tstree)->val;

  const TSLanguage *result = ts_tree_language(tstree);
  struct_TSLanguage *res_result = enif_alloc_resource(res_TSLanguage, sizeof(struct_TSLanguage));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(tree_included_ranges_nif) {
  /* TODO: segmentation fault when calling it 🤷 */
  /* TSRange *ts_tree_included_ranges(const TSTree *self, uint32_t *length); */
  return atom_undefined;
}

ERL_TS_FUNCTION(tree_edit_nif) {
  /* void ts_tree_edit(TSTree *self, const TSInputEdit *edit); */
  void *res_tstree = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSTree, &res_tstree));
  TSTree *tstree = ((struct_TSTree *)res_tstree)->val;

  TSInputEdit edit;
  RETURN_BADARG_IF(!map_to_tsinput_edit(env, argv[1], &edit));

  ts_tree_edit(tstree, &edit);
  return atom_ok;
}

ERL_TS_FUNCTION(tree_get_changed_ranges_nif) {
  /* TODO: */
  /*   TSRange *ts_tree_get_changed_ranges( */
  /*   const TSTree *old_tree, */
  /*   const TSTree *new_tree, */
  /*   uint32_t *length */
  /* ); */
  return atom_undefined;
}

ERL_TS_FUNCTION(tree_print_dot_graph_nif) {
  /* TODO: */
  /* void ts_tree_print_dot_graph(const TSTree *self, int file_descriptor); */
  return atom_undefined;
}

/******************/
/* Section - Node */
/******************/

ERL_TS_FUNCTION(node_type_nif) {
  /* const char *ts_node_type(TSNode self); */
  void *res_node = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_node));
  TSNode node = ((struct_TSNode *)res_node)->val;

  const char *type = ts_node_type(node);
  return enif_make_string(env, type, ERL_NIF_LATIN1);
}

ERL_TS_FUNCTION(node_symbol_nif) {
  /* TSSymbol ts_node_symbol(TSNode self); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  TSSymbol symbol = ts_node_symbol(tsnode);
  return enif_make_uint(env, symbol);
}

ERL_TS_FUNCTION(node_language_nif) {
  /* const TSLanguage *ts_node_language(TSNode self); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  const TSLanguage *result = ts_node_language(tsnode);
  struct_TSLanguage *res_result = enif_alloc_resource(res_TSLanguage, sizeof(struct_TSLanguage));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(node_grammar_type_nif) {
  /* const char *ts_node_grammar_type(TSNode self); */
  void *res_node = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_node));
  TSNode node = ((struct_TSNode *)res_node)->val;

  const char *type = ts_node_grammar_type(node);
  return enif_make_string(env, type, ERL_NIF_LATIN1);
}

ERL_TS_FUNCTION(node_grammar_symbol_nif) {
  /* TODO: */
  /* TSSymbol ts_node_grammar_symbol(TSNode self); */
  return atom_undefined;
}

ERL_TS_FUNCTION(node_start_byte_nif) {
  /* uint32_t ts_node_start_byte(TSNode self); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  uint32_t result = ts_node_start_byte(tsnode);
  return enif_make_uint(env, result);
}

ERL_TS_FUNCTION(node_start_point_nif) {
  /* TSPoint ts_node_start_point(TSNode self); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  TSPoint result = ts_node_start_point(tsnode);
  return tspoint_to_map(env, result);
}

ERL_TS_FUNCTION(node_end_byte_nif) {
  /* uint32_t ts_node_end_byte(TSNode self); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  uint32_t result = ts_node_end_byte(tsnode);
  return enif_make_uint(env, result);
}

ERL_TS_FUNCTION(node_end_point_nif) {
  /* TSPoint ts_node_end_point(TSNode self); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  TSPoint result = ts_node_end_point(tsnode);
  return tspoint_to_map(env, result);
}

ERL_TS_FUNCTION(node_string_nif) {
  void *res_node;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_node));
  TSNode node = ((struct_TSNode *)res_node)->val;

  char *string = ts_node_string(node);
  return enif_make_string(env, string, ERL_NIF_LATIN1);
}

ERL_TS_FUNCTION(node_is_null_nif) {
  /* bool ts_node_is_null(TSNode self); */
  void *res_node = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_node));
  TSNode node = ((struct_TSNode *)res_node)->val;

  bool is_null = ts_node_is_null(node);
  return is_null ? atom_true : atom_false;
}

ERL_TS_FUNCTION(node_is_named_nif) {
  /* bool ts_node_is_named(TSNode self); */
  void *res_node;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_node));
  TSNode node = ((struct_TSNode *)res_node)->val;

  bool is_null = ts_node_is_named(node);
  return is_null ? atom_true : atom_false;
}

ERL_TS_FUNCTION(node_is_missing_nif) {
  /* bool ts_node_is_missing(TSNode self); */
  void *res_node = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_node));
  TSNode node = ((struct_TSNode *)res_node)->val;

  bool is_null = ts_node_is_missing(node);
  return is_null ? atom_true : atom_false;
}

ERL_TS_FUNCTION(node_is_extra_nif) {
  /* bool ts_node_is_extra(TSNode self); */
  void *res_node = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_node));
  TSNode node = ((struct_TSNode *)res_node)->val;

  bool is_null = ts_node_is_extra(node);
  return is_null ? atom_true : atom_false;
}

ERL_TS_FUNCTION(node_has_changes_nif) {
  /* bool ts_node_has_changes(TSNode self); */
  void *res_node = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_node));
  TSNode node = ((struct_TSNode *)res_node)->val;

  bool is_null = ts_node_has_changes(node);
  return is_null ? atom_true : atom_false;
}

ERL_TS_FUNCTION(node_has_error_nif) {
  /* bool ts_node_has_error(TSNode self); */
  void *res_node = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_node));
  TSNode node = ((struct_TSNode *)res_node)->val;

  bool is_null = ts_node_has_error(node);
  return is_null ? atom_true : atom_false;
}

ERL_TS_FUNCTION(node_is_error_nif) {
  /* bool ts_node_is_error(TSNode self); */
  void *res_node = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_node));
  TSNode node = ((struct_TSNode *)res_node)->val;

  bool is_null = ts_node_is_error(node);
  return is_null ? atom_true : atom_false;
}

ERL_TS_FUNCTION(node_parse_state_nif) {
  /* TSStateId ts_node_parse_state(TSNode self); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  TSStateId result = ts_node_parse_state(tsnode);
  return enif_make_uint(env, result);
}

ERL_TS_FUNCTION(node_next_parse_state_nif) {
  /* TSStateId ts_node_next_parse_state(TSNode self); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  TSStateId result = ts_node_next_parse_state(tsnode);
  return enif_make_uint(env, result);
}

ERL_TS_FUNCTION(node_parent_nif) {
  /* TSNode ts_node_parent(TSNode self); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  TSNode result = ts_node_parent(tsnode);
  struct_TSNode *res_result = enif_alloc_resource(res_TSNode, sizeof(struct_TSNode));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(node_child_containing_descendant_nif) {
  /* TSNode ts_node_child_containing_descendant(TSNode self, TSNode descendant); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  void *res_descendant = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[1], res_TSNode, &res_descendant));
  TSNode descendant = ((struct_TSNode *)res_descendant)->val;

  TSNode result = ts_node_child_containing_descendant(tsnode, descendant);
  struct_TSNode *res_result = enif_alloc_resource(res_TSNode, sizeof(struct_TSNode));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(node_child_with_descendant_nif) {
  /* TODO: TREE_SITTER_LANGUAGE_VERSION > 14 */
  /* TSNode ts_node_child_with_descendant(TSNode self, TSNode descendant); */
  return atom_undefined;
}

ERL_TS_FUNCTION(node_child_nif) {
  /* TSNode ts_node_child(TSNode self, uint32_t child_index); */
  RETURN_BADARG_IF(argc != 2);

  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  uint32_t child_index;
  RETURN_BADARG_IF(!enif_get_uint(env, argv[1], &child_index));

  TSNode result = ts_node_child(tsnode, child_index);
  struct_TSNode *res_result = enif_alloc_resource(res_TSNode, sizeof(struct_TSNode));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(node_field_name_for_child_nif) {
  /* const char *ts_node_field_name_for_child(TSNode self, uint32_t
   * child_index); */
  void *res_node = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_node));
  TSNode node = ((struct_TSNode *)res_node)->val;

  uint32_t index = 0;
  RETURN_BADARG_IF(!enif_get_uint(env, argv[1], &index));

  const char *name = ts_node_field_name_for_child(node, index);
  return enif_make_string(env, name, ERL_NIF_LATIN1);
}

ERL_TS_FUNCTION(node_field_name_for_named_child_nif) {
  /* TODO: TREE_SITTER_LANGUAGE_VERSION > 14 */
  /* const char *ts_node_field_name_for_named_child(TSNode self, uint32_t
   * named_child_index); */
  return atom_undefined;
}

ERL_TS_FUNCTION(node_child_count_nif) {
  /* uint32_t ts_node_child_count(TSNode self); */
  void *res_node = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_node));
  TSNode node = ((struct_TSNode *)res_node)->val;

  uint32_t count = ts_node_child_count(node);
  ERL_NIF_TERM term_count = enif_make_uint(env, count);
  return term_count;
}

ERL_TS_FUNCTION(node_named_child_nif) {
  /* TSNode ts_node_named_child(TSNode self, uint32_t child_index); */
  void *res_node = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_node));
  TSNode node = ((struct_TSNode *)res_node)->val;

  uint32_t index = 0;
  RETURN_BADARG_IF(!enif_get_uint(env, argv[1], &index));

  TSNode child = ts_node_named_child(node, index);
  struct_TSNode *res_child =
    enif_alloc_resource(res_TSNode, sizeof(struct_TSNode));
  res_child->val = child;
  ERL_NIF_TERM term_child = enif_make_resource(env, res_child);
  enif_release_resource(res_child);
  return term_child;
}

ERL_TS_FUNCTION(node_named_child_count_nif) {
  /* uint32_t ts_node_named_child_count(TSNode self); */
  void *res_node = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_node));
  TSNode node = ((struct_TSNode *)res_node)->val;

  uint32_t count = ts_node_named_child_count(node);
  ERL_NIF_TERM term_count = enif_make_uint(env, count);
  return term_count;
}

ERL_TS_FUNCTION(node_child_by_field_name_nif) {
  /*   TSNode ts_node_child_by_field_name( */
  /*   TSNode self, */
  /*   const char *name, */
  /*   uint32_t name_length */
  /* ); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  char *name = NULL;
  unsigned int name_length;
  RETURN_BADARG_IF(!enif_get_list_length(env, argv[1], &name_length));
  name = (char *)enif_alloc(name_length + 1);
  if (!enif_get_string(env, argv[1], name, name_length + 1, ERL_NIF_LATIN1)) {
    enif_free(name);
    return enif_make_badarg(env);
  }

  TSNode result = ts_node_child_by_field_name(tsnode, name, name_length);
  struct_TSNode *res_result = enif_alloc_resource(res_TSNode, sizeof(struct_TSNode));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(node_child_by_field_id_nif) {
  /* TSNode ts_node_child_by_field_id(TSNode self, TSFieldId field_id); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  unsigned int field_id;
  RETURN_BADARG_IF(!enif_get_uint(env, argv[1], &field_id));

  TSNode result = ts_node_child_by_field_id(tsnode, field_id);
  struct_TSNode *res_result = enif_alloc_resource(res_TSNode, sizeof(struct_TSNode));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(node_next_sibling_nif) {
  /* TSNode ts_node_next_sibling(TSNode self); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  TSNode result = ts_node_next_sibling(tsnode);
  struct_TSNode *res_result = enif_alloc_resource(res_TSNode, sizeof(struct_TSNode));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(node_prev_sibling_nif) {
  /* TSNode ts_node_prev_sibling(TSNode self); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  TSNode result = ts_node_prev_sibling(tsnode);
  struct_TSNode *res_result = enif_alloc_resource(res_TSNode, sizeof(struct_TSNode));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(node_next_named_sibling_nif) {
  /* TSNode ts_node_next_named_sibling(TSNode self); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  TSNode result = ts_node_next_named_sibling(tsnode);
  struct_TSNode *res_result = enif_alloc_resource(res_TSNode, sizeof(struct_TSNode));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(node_prev_named_sibling_nif) {
  /* TSNode ts_node_prev_named_sibling(TSNode self); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  TSNode result = ts_node_prev_named_sibling(tsnode);
  struct_TSNode *res_result = enif_alloc_resource(res_TSNode, sizeof(struct_TSNode));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(node_first_child_for_byte_nif) {
  /* TSNode ts_node_first_child_for_byte(TSNode self, uint32_t byte); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  uint32_t byte;
  RETURN_BADARG_IF(!enif_get_uint(env, argv[1], &byte));

  TSNode result = ts_node_first_child_for_byte(tsnode, byte);
  struct_TSNode *res_result = enif_alloc_resource(res_TSNode, sizeof(struct_TSNode));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(node_first_named_child_for_byte_nif) {
  /* TSNode ts_node_first_named_child_for_byte(TSNode self, uint32_t byte); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  uint32_t byte;
  RETURN_BADARG_IF(!enif_get_uint(env, argv[1], &byte));

  TSNode result = ts_node_first_named_child_for_byte(tsnode, byte);
  struct_TSNode *res_result = enif_alloc_resource(res_TSNode, sizeof(struct_TSNode));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(node_descendant_count_nif) {
  /* uint32_t ts_node_descendant_count(TSNode self); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  uint32_t result = ts_node_descendant_count(tsnode);
  return enif_make_uint(env, result);
}

ERL_TS_FUNCTION(node_descendant_for_byte_range_nif) {
  /* TSNode ts_node_descendant_for_byte_range(TSNode self, uint32_t start, uint32_t end); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  uint32_t start;
  RETURN_BADARG_IF(!enif_get_uint(env, argv[1], &start));

  uint32_t end;
  RETURN_BADARG_IF(!enif_get_uint(env, argv[2], &end));

  TSNode result = ts_node_descendant_for_byte_range(tsnode, start, end);
  struct_TSNode *res_result = enif_alloc_resource(res_TSNode, sizeof(struct_TSNode));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(node_descendant_for_point_range_nif) {
  /* TSNode ts_node_descendant_for_point_range(TSNode self, TSPoint start, TSPoint end); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  TSPoint start, end;
  RETURN_BADARG_IF(!map_to_tspoint(env, argv[1], &start));
  RETURN_BADARG_IF(!map_to_tspoint(env, argv[2], &end));

  TSNode result = ts_node_descendant_for_point_range(tsnode, start, end);
  struct_TSNode *res_result = enif_alloc_resource(res_TSNode, sizeof(struct_TSNode));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(node_named_descendant_for_byte_range_nif) {
  /* TSNode ts_node_named_descendant_for_byte_range(TSNode self, uint32_t start, uint32_t end); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  uint32_t start;
  RETURN_BADARG_IF(!enif_get_uint(env, argv[1], &start));

  uint32_t end;
  RETURN_BADARG_IF(!enif_get_uint(env, argv[2], &end));

  TSNode result = ts_node_named_descendant_for_byte_range(tsnode, start, end);
  struct_TSNode *res_result = enif_alloc_resource(res_TSNode, sizeof(struct_TSNode));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(node_named_descendant_for_point_range_nif) {
  /* TSNode ts_node_named_descendant_for_point_range(TSNode self, TSPoint start, TSPoint end); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  TSPoint start, end;
  RETURN_BADARG_IF(!map_to_tspoint(env, argv[1], &start));
  RETURN_BADARG_IF(!map_to_tspoint(env, argv[2], &end));

  TSNode result = ts_node_named_descendant_for_point_range(tsnode, start, end);
  struct_TSNode *res_result = enif_alloc_resource(res_TSNode, sizeof(struct_TSNode));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(node_edit_nif) {
  /* Rarely recommended to be used */
  /* void ts_node_edit(TSNode *self, const TSInputEdit *edit); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  TSInputEdit edit;
  RETURN_BADARG_IF(!map_to_tsinput_edit(env, argv[1], &edit));

  ts_node_edit(&tsnode, &edit);
  return atom_ok;
}

ERL_TS_FUNCTION(node_eq_nif) {
  /* bool ts_node_eq(TSNode self, TSNode other); */
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  void *res_other = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[1], res_TSNode, &res_other));
  TSNode other = ((struct_TSNode *)res_other)->val;

  bool result = ts_node_eq(tsnode, other);
  return result? atom_true : atom_false;
}

/************************/
/* Section - TreeCursor */
/************************/

ERL_TS_FUNCTION(tree_cursor_new_nif) {
  /* TSTreeCursor ts_tree_cursor_new(TSNode node); */
  void *res_node = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_node));
  TSNode node = ((struct_TSNode *)res_node)->val;

  TSTreeCursor result = ts_tree_cursor_new(node);
  struct_TSTreeCursor *res_result =
    enif_alloc_resource(res_TSTreeCursor, sizeof(struct_TSTreeCursor));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(tree_cursor_delete_nif) {
  /* TODO: */
  /* void ts_tree_cursor_delete(TSTreeCursor *self); */
  return atom_undefined;
}

ERL_TS_FUNCTION(tree_cursor_reset_nif) {
  /* TODO: */
  /* void ts_tree_cursor_reset(TSTreeCursor *self, TSNode node); */
  return atom_undefined;
}

ERL_TS_FUNCTION(tree_cursor_reset_to_nif) {
  /* TODO: */
  /* void ts_tree_cursor_reset_to(TSTreeCursor *dst, const TSTreeCursor *src); */
  return atom_undefined;
}

ERL_TS_FUNCTION(tree_cursor_current_node_nif) {
  /* TSNode ts_tree_cursor_current_node(const TSTreeCursor *self); */
  void *res_tstreecursor = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSTreeCursor, &res_tstreecursor));
  TSTreeCursor tstreecursor = ((struct_TSTreeCursor *)res_tstreecursor)->val;

  TSNode result = ts_tree_cursor_current_node(&tstreecursor);
  struct_TSNode *res_result = enif_alloc_resource(res_TSNode, sizeof(struct_TSNode));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(tree_cursor_current_field_name_nif) {
  /* TODO: */
  /* const char *ts_tree_cursor_current_field_name(const TSTreeCursor *self); */
  void *res_tstreecursor = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSTreeCursor, &res_tstreecursor));
  TSTreeCursor tstreecursor = ((struct_TSTreeCursor *)res_tstreecursor)->val;

  const char *result = ts_tree_cursor_current_field_name(&tstreecursor);
  return enif_make_string(env, result, ERL_NIF_LATIN1);
}

ERL_TS_FUNCTION(tree_cursor_current_field_id_nif) {
  /* TODO: */
  /* TSFieldId ts_tree_cursor_current_field_id(const TSTreeCursor *self); */
  void *res_tstreecursor = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSTreeCursor, &res_tstreecursor));
  TSTreeCursor tstreecursor = ((struct_TSTreeCursor *)res_tstreecursor)->val;

  uint16_t result = ts_tree_cursor_current_field_id(&tstreecursor);
  return enif_make_uint(env, result);
}

ERL_TS_FUNCTION(tree_cursor_goto_parent_nif) {
  /* TODO: */
  /* bool ts_tree_cursor_goto_parent(TSTreeCursor *self); */
  void *res_tstreecursor = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSTreeCursor, &res_tstreecursor));
  TSTreeCursor tstreecursor = ((struct_TSTreeCursor *)res_tstreecursor)->val;

  bool result = ts_tree_cursor_goto_parent(&tstreecursor);
  return result? atom_true : atom_false;
}

ERL_TS_FUNCTION(tree_cursor_goto_next_sibling_nif) {
  /* TODO: */
  /* bool ts_tree_cursor_goto_next_sibling(TSTreeCursor *self); */
  return atom_undefined;
}

ERL_TS_FUNCTION(tree_cursor_goto_previous_sibling_nif) {
  /* TODO: */
  /* bool ts_tree_cursor_goto_previous_sibling(TSTreeCursor *self); */
  return atom_undefined;
}

ERL_TS_FUNCTION(tree_cursor_goto_first_child_nif) {
  /* TODO: */
  /* bool ts_tree_cursor_goto_first_child(TSTreeCursor *self); */
  return atom_undefined;
}

ERL_TS_FUNCTION(tree_cursor_goto_last_child_nif) {
  /* TODO: */
  /* bool ts_tree_cursor_goto_last_child(TSTreeCursor *self); */
  return atom_undefined;
}

ERL_TS_FUNCTION(tree_cursor_goto_descendant_nif) {
  /* TODO: */
  /* void ts_tree_cursor_goto_descendant(TSTreeCursor *self, uint32_t goal_descendant_index); */

  return atom_undefined;
}

ERL_TS_FUNCTION(tree_cursor_current_descendant_index_nif) {
  /* TODO: */
  /* uint32_t ts_tree_cursor_current_descendant_index(const TSTreeCursor *self); */
  return atom_undefined;
}

ERL_TS_FUNCTION(tree_cursor_current_depth_nif) {
  /* TODO: */
  /* uint32_t ts_tree_cursor_current_depth(const TSTreeCursor *self); */
  return atom_undefined;
}

ERL_TS_FUNCTION(tree_cursor_goto_first_child_for_byte_nif) {
  /* TODO: */
  /* int64_t ts_tree_cursor_goto_first_child_for_byte(TSTreeCursor *self, uint32_t goal_byte); */
  return atom_undefined;
}

ERL_TS_FUNCTION(tree_cursor_goto_first_child_for_point_nif) {
  /* TODO: */
  /* int64_t ts_tree_cursor_goto_first_child_for_point(TSTreeCursor *self, TSPoint goal_point); */
  return atom_undefined;
}

ERL_TS_FUNCTION(tree_cursor_copy_nif) {
  /* TODO: */
  /* TSTreeCursor ts_tree_cursor_copy(const TSTreeCursor *cursor); */
  return atom_undefined;
}

/*******************/
/* Section - Query */
/*******************/

ERL_TS_FUNCTION(query_new_nif) {
  /*   TSQuery *ts_query_new( */
  /*   const TSLanguage *language, */
  /*   const char *source, */
  /*   uint32_t source_len, */
  /*   uint32_t *error_offset, */
  /*   TSQueryError *error_type */
  /* ); */
  void *res_language;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSLanguage, &res_language));
  const TSLanguage *language = ((struct_TSLanguage *)res_language)->val;

  char *source = NULL;
  unsigned int source_length;
  RETURN_BADARG_IF(!enif_get_list_length(env, argv[1], &source_length));
  source = (char *)enif_alloc(source_length + 1);
  if (!enif_get_string(env, argv[1], source, source_length + 1, ERL_NIF_LATIN1)) {
    enif_free(source);
    return enif_make_badarg(env);
  }

  uint32_t error_offset;
  TSQueryError error_type;

  TSQuery *result = ts_query_new(language, source, source_length, &error_offset, &error_type);
  struct_TSQuery *res_result = enif_alloc_resource(res_TSQuery, sizeof(struct_TSQuery));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);

  ERL_NIF_TERM term_error_offset = enif_make_uint(env, error_offset);
  ERL_NIF_TERM term_error_type = tsquery_error_to_atom(env, error_type);

  return enif_make_tuple3(env, term_result, term_error_offset, term_error_type);
}

ERL_TS_FUNCTION(query_delete_nif) {
  /* TODO: */
  /* void ts_query_delete(TSQuery *self); */
  return atom_undefined;
}

ERL_TS_FUNCTION(query_pattern_count_nif) {
  /* uint32_t ts_query_pattern_count(const TSQuery *self); */
  void *res_tsquery;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSQuery, &res_tsquery));
  TSQuery *tsquery = ((struct_TSQuery *)res_tsquery)->val;

  uint32_t result = ts_query_pattern_count(tsquery);
  return enif_make_uint(env, result);
}

ERL_TS_FUNCTION(query_capture_count_nif) {
  /* uint32_t ts_query_capture_count(const TSQuery *self); */
  void *res_tsquery;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSQuery, &res_tsquery));
  TSQuery *tsquery = ((struct_TSQuery *)res_tsquery)->val;

  uint32_t result = ts_query_capture_count(tsquery);
  return enif_make_uint(env, result);
}

ERL_TS_FUNCTION(query_string_count_nif) {
  /* uint32_t ts_query_string_count(const TSQuery *self); */
  void *res_tsquery;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSQuery, &res_tsquery));
  TSQuery *tsquery = ((struct_TSQuery *)res_tsquery)->val;

  uint32_t result = ts_query_string_count(tsquery);
  return enif_make_uint(env, result);
}

ERL_TS_FUNCTION(query_start_byte_for_pattern_nif) {
  /* uint32_t ts_query_start_byte_for_pattern(const TSQuery *self, uint32_t pattern_index); */
  void *res_tsquery;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSQuery, &res_tsquery));
  TSQuery *tsquery = ((struct_TSQuery *)res_tsquery)->val;

  uint32_t pattern_index;
  RETURN_BADARG_IF(!enif_get_uint(env, argv[1], &pattern_index));

  uint32_t result = ts_query_start_byte_for_pattern(tsquery, pattern_index);
  return enif_make_uint(env, result);
}

ERL_TS_FUNCTION(query_end_byte_for_pattern_nif) {
  /* uint32_t ts_query_end_byte_for_pattern(const TSQuery *self, uint32_t pattern_index); */
  void *res_tsquery;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSQuery, &res_tsquery));
  TSQuery *tsquery = ((struct_TSQuery *)res_tsquery)->val;

  uint32_t pattern_index;
  RETURN_BADARG_IF(!enif_get_uint(env, argv[1], &pattern_index));

  uint32_t result = ts_query_end_byte_for_pattern(tsquery, pattern_index);
  return enif_make_uint(env, result);
}

ERL_TS_FUNCTION(query_predicates_for_pattern_nif) {
  /* TODO: */
  /*   const TSQueryPredicateStep *ts_query_predicates_for_pattern( */
  /*   const TSQuery *self, */
  /*   uint32_t pattern_index, */
  /*   uint32_t *step_count */
  /* ); */
  return atom_undefined;
}

ERL_TS_FUNCTION(query_is_pattern_rooted_nif) {
  /* bool ts_query_is_pattern_rooted(const TSQuery *self, uint32_t pattern_index); */
  void *res_tsquery;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSQuery, &res_tsquery));
  TSQuery *tsquery = ((struct_TSQuery *)res_tsquery)->val;

  uint32_t pattern_index;
  RETURN_BADARG_IF(!enif_get_uint(env, argv[1], &pattern_index));

  bool result = ts_query_is_pattern_rooted(tsquery, pattern_index);
  return result? atom_true : atom_false;
}

ERL_TS_FUNCTION(query_is_pattern_non_local_nif) {
  /* bool ts_query_is_pattern_non_local(const TSQuery *self, uint32_t pattern_index); */
  void *res_tsquery;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSQuery, &res_tsquery));
  TSQuery *tsquery = ((struct_TSQuery *)res_tsquery)->val;

  uint32_t pattern_index;
  RETURN_BADARG_IF(!enif_get_uint(env, argv[1], &pattern_index));

  bool result = ts_query_is_pattern_non_local(tsquery, pattern_index);
  return result? atom_true : atom_false;
}

ERL_TS_FUNCTION(query_is_pattern_guaranteed_at_step_nif) {
  /* bool ts_query_is_pattern_guaranteed_at_step(const TSQuery *self, uint32_t byte_offset); */
  void *res_tsquery;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSQuery, &res_tsquery));
  TSQuery *tsquery = ((struct_TSQuery *)res_tsquery)->val;

  uint32_t byte_offset;
  RETURN_BADARG_IF(!enif_get_uint(env, argv[1], &byte_offset));

  bool result = ts_query_is_pattern_guaranteed_at_step(tsquery, byte_offset);
  return result? atom_true : atom_false;
}

ERL_TS_FUNCTION(query_capture_name_for_id_nif) {
  /*   const char *ts_query_capture_name_for_id( */
  /*   const TSQuery *self, */
  /*   uint32_t index, */
  /*   uint32_t *length */
  /* ); */
  void *res_tsquery;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSQuery, &res_tsquery));
  TSQuery *tsquery = ((struct_TSQuery *)res_tsquery)->val;

  uint32_t index;
  RETURN_BADARG_IF(!enif_get_uint(env, argv[1], &index));

  uint32_t capture_count = ts_query_capture_count(tsquery);
  if (index >= capture_count)
    return atom_undefined;

  uint32_t length = 0;

  const char *result = ts_query_capture_name_for_id(tsquery, index, &length);
  return enif_make_string(env, result, ERL_NIF_LATIN1);
}

ERL_TS_FUNCTION(query_capture_quantifier_for_id_nif) {
  /* TODO: */
  /*   TSQuantifier ts_query_capture_quantifier_for_id( */
  /*   const TSQuery *self, */
  /*   uint32_t pattern_index, */
  /*   uint32_t capture_index */
  /* ); */
  return atom_undefined;
}

ERL_TS_FUNCTION(query_string_value_for_id_nif) {
  /* TODO: segfault */
  /*   const char *ts_query_string_value_for_id( */
  /*   const TSQuery *self, */
  /*   uint32_t index, */
  /*   uint32_t *length */
  /* ); */
  void *res_tsquery;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSQuery, &res_tsquery));
  TSQuery *tsquery = ((struct_TSQuery *)res_tsquery)->val;

  uint32_t index;
  RETURN_BADARG_IF(!enif_get_uint(env, argv[1], &index));

  uint32_t string_count = ts_query_string_count(tsquery);
  if (index >= string_count)
    return atom_undefined;

  uint32_t length;

  const char *result = ts_query_string_value_for_id(tsquery, index, &length);
  return enif_make_string(env, result, ERL_NIF_LATIN1);
}

ERL_TS_FUNCTION(query_disable_capture_nif) {
  /* TODO: */
  /* void ts_query_disable_capture(TSQuery *self, const char *name, uint32_t length); */
  return atom_undefined;
}

ERL_TS_FUNCTION(query_disable_pattern_nif) {
  /* TODO: */
  /* void ts_query_disable_pattern(TSQuery *self, uint32_t pattern_index); */
  return atom_undefined;
}

ERL_TS_FUNCTION(query_capture_nif) {
  void *res_tsnode = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSNode, &res_tsnode));
  TSNode tsnode = ((struct_TSNode *)res_tsnode)->val;

  void *res_query;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[1], res_TSQuery, &res_query));
  TSQuery *query = ((struct_TSQuery *)res_query)->val;

  TSQueryCursor *query_cursor = ts_query_cursor_new();

  ts_query_cursor_exec(query_cursor, query, tsnode);

  TSQueryMatch match;
  uint32_t capture_index;
  ERL_NIF_TERM list = enif_make_list(env, 0);

  while (ts_query_cursor_next_capture(query_cursor, &match, &capture_index)){
    for (int i = 0; i < match.capture_count; i++) {
      TSQueryCapture qc = match.captures[i];
      uint32_t length;
      const char *capture =
        ts_query_capture_name_for_id(query, qc.index, &length);

      struct_TSNode *res_result = enif_alloc_resource(res_TSNode, sizeof(struct_TSNode));
      res_result->val = qc.node;
      ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
      enif_release_resource(res_result);

      ERL_NIF_TERM entry = enif_make_tuple2(
                                            env, enif_make_string(env, capture, ERL_NIF_LATIN1), term_result);
      list = enif_make_list_cell(env, entry, list);
    }
  }
  ts_query_cursor_delete(query_cursor);
  return list;
}

ERL_TS_FUNCTION(query_cursor_new_nif) {
  /* TSQueryCursor *ts_query_cursor_new(void); */
  TSQueryCursor *result = ts_query_cursor_new();
  struct_TSQueryCursor *res_result = enif_alloc_resource(res_TSQueryCursor, sizeof(struct_TSQueryCursor));
  res_result->val = result;
  ERL_NIF_TERM term_result = enif_make_resource(env, res_result);
  enif_release_resource(res_result);
  return term_result;
}

ERL_TS_FUNCTION(query_cursor_delete_nif) {
  /* TODO: */
  /* void ts_query_cursor_delete(TSQueryCursor *self); */
  return atom_undefined;
}

ERL_TS_FUNCTION(query_cursor_exec_nif) {
  /* void ts_query_cursor_exec(TSQueryCursor *self, const TSQuery *query, TSNode node); */
  void *res_tsquerycursor;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSQueryCursor, &res_tsquerycursor));
  TSQueryCursor *tsquerycursor = ((struct_TSQueryCursor *)res_tsquerycursor)->val;

  void *res_query;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[1], res_TSQuery, &res_query));
  TSQuery *query = ((struct_TSQuery *)res_query)->val;

  void *res_node;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[2], res_TSNode, &res_node));
  TSNode node = ((struct_TSNode *)res_node)->val;

  ts_query_cursor_exec(tsquerycursor, query, node);
  return atom_ok;
}

ERL_TS_FUNCTION(query_cursor_exec_with_options_nif) {
  /* TODO: */
  /*   void ts_query_cursor_exec_with_options( */
  /*   TSQueryCursor *self, */
  /*   const TSQuery *query, */
  /*   TSNode node, */
  /*   const TSQueryCursorOptions *query_options */
  /* ); */
  return atom_undefined;
}

ERL_TS_FUNCTION(query_cursor_did_exceed_match_limit_nif) {
  /* bool ts_query_cursor_did_exceed_match_limit(const TSQueryCursor *self); */
  void *res_tsquerycursor;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSQueryCursor, &res_tsquerycursor));
  TSQueryCursor *tsquerycursor = ((struct_TSQueryCursor *)res_tsquerycursor)->val;

  bool result = ts_query_cursor_did_exceed_match_limit(tsquerycursor);
  return result? atom_true : atom_false;
}

ERL_TS_FUNCTION(query_cursor_match_limit_nif) {
  /* uint32_t ts_query_cursor_match_limit(const TSQueryCursor *self); */
  void *res_tsquerycursor;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSQueryCursor, &res_tsquerycursor));
  TSQueryCursor *tsquerycursor = ((struct_TSQueryCursor *)res_tsquerycursor)->val;

  uint32_t result = ts_query_cursor_match_limit(tsquerycursor);
  return enif_make_uint(env, result);
}

ERL_TS_FUNCTION(query_cursor_set_match_limit_nif) {
  /* TODO: */
  /* void ts_query_cursor_set_match_limit(TSQueryCursor *self, uint32_t limit); */
  return atom_undefined;
}

ERL_TS_FUNCTION(query_cursor_set_timeout_micros_nif) {
  /* TODO: */
  /* void ts_query_cursor_set_timeout_micros(TSQueryCursor *self, uint64_t timeout_micros); */
  return atom_undefined;
}

ERL_TS_FUNCTION(query_cursor_timeout_micros_nif) {
  /* TODO: */
  /* uint64_t ts_query_cursor_timeout_micros(const TSQueryCursor *self); */
  return atom_undefined;
}

ERL_TS_FUNCTION(query_cursor_set_byte_range_nif) {
  /* TODO: */
  /* bool ts_query_cursor_set_byte_range(TSQueryCursor *self, uint32_t start_byte, uint32_t end_byte); */
  return atom_undefined;
}

ERL_TS_FUNCTION(query_cursor_set_point_range_nif) {
  /* TODO: */
  /* bool ts_query_cursor_set_point_range(TSQueryCursor *self, TSPoint start_point, TSPoint end_point); */
  return atom_undefined;
}

ERL_TS_FUNCTION(query_cursor_next_match_nif) {
  /* TODO: */
  /* bool ts_query_cursor_next_match(TSQueryCursor *self, TSQueryMatch *match); */
  return atom_undefined;
}

ERL_TS_FUNCTION(query_cursor_remove_match_nif) {
  /* TODO: */
  /* void ts_query_cursor_remove_match(TSQueryCursor *self, uint32_t match_id); */
  return atom_undefined;
}

ERL_TS_FUNCTION(query_cursor_next_capture_nif) {
  /* TODO: */
  /*   bool ts_query_cursor_next_capture( */
  /*   TSQueryCursor *self, */
  /*   TSQueryMatch *match, */
  /*   uint32_t *capture_index */
  /* ); */
  return atom_undefined;
}

ERL_TS_FUNCTION(query_cursor_set_max_start_depth_nif) {
  /* TODO: */
  /* void ts_query_cursor_set_max_start_depth(TSQueryCursor *self, uint32_t max_start_depth); */
  return atom_undefined;
}

/**********************/
/* Section - Language */
/**********************/

ERL_TS_FUNCTION(language_copy_nif) {
  /* TODO: */
  /* const TSLanguage *ts_language_copy(const TSLanguage *self); */
  return atom_undefined;
}

ERL_TS_FUNCTION(language_delete_nif) {
  /* TODO: */
  /* void ts_language_delete(const TSLanguage *self); */
  return atom_undefined;
}

ERL_TS_FUNCTION(language_symbol_count_nif) {
  /* uint32_t ts_language_symbol_count(const TSLanguage *self); */
  void *res_tslanguage = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSLanguage, &res_tslanguage));
  const TSLanguage *tslanguage = ((struct_TSLanguage *)res_tslanguage)->val;

  uint32_t result = ts_language_symbol_count(tslanguage);
  return enif_make_uint(env, result);
}

ERL_TS_FUNCTION(language_state_count_nif) {
  /* uint32_t ts_language_state_count(const TSLanguage *self); */
  void *res_tslanguage = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSLanguage, &res_tslanguage));
  const TSLanguage *tslanguage = ((struct_TSLanguage *)res_tslanguage)->val;

  uint32_t result = ts_language_state_count(tslanguage);
  return enif_make_uint(env, result);
}

ERL_TS_FUNCTION(language_symbol_name_nif) {
  /* const char *ts_language_symbol_name(const TSLanguage *self, TSSymbol symbol); */
  void *res_tslanguage = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSLanguage, &res_tslanguage));
  const TSLanguage *tslanguage = ((struct_TSLanguage *)res_tslanguage)->val;

  uint32_t symbol;
  RETURN_BADARG_IF(!enif_get_uint(env, argv[1], &symbol));

  const char *result = ts_language_symbol_name(tslanguage, symbol);
  return enif_make_string(env, result, ERL_NIF_LATIN1);
}

ERL_TS_FUNCTION(language_symbol_for_name_nif) {
  /* TODO: */
  /*   TSSymbol ts_language_symbol_for_name( */
  /*   const TSLanguage *self, */
  /*   const char *string, */
  /*   uint32_t length, */
  /*   bool is_named */
  /* ); */
  return atom_undefined;
}

ERL_TS_FUNCTION(language_field_count_nif) {
  /* uint32_t ts_language_field_count(const TSLanguage *self); */
  void *res_tslanguage = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSLanguage, &res_tslanguage));
  const TSLanguage *tslanguage = ((struct_TSLanguage *)res_tslanguage)->val;

  uint32_t result = ts_language_field_count(tslanguage);
  return enif_make_uint(env, result);
}

ERL_TS_FUNCTION(language_field_name_for_id_nif) {
  /* const char *ts_language_field_name_for_id(const TSLanguage *self, TSFieldId id); */
  void *res_tslanguage = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSLanguage, &res_tslanguage));
  const TSLanguage *tslanguage = ((struct_TSLanguage *)res_tslanguage)->val;

  uint32_t id;
  RETURN_BADARG_IF(!enif_get_uint(env, argv[1], &id));

  const char *result = ts_language_field_name_for_id(tslanguage, id);
  return enif_make_string(env, result, ERL_NIF_LATIN1);
}

ERL_TS_FUNCTION(language_field_id_for_name_nif) {
  /* TSFieldId ts_language_field_id_for_name(const TSLanguage *self, const char *name, uint32_t name_length); */
  void *res_tslanguage = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSLanguage, &res_tslanguage));
  const TSLanguage *tslanguage = ((struct_TSLanguage *)res_tslanguage)->val;

  char *name = NULL;
  uint32_t name_length;
  RETURN_BADARG_IF(!enif_get_list_length(env, argv[1], &name_length));
  name = (char *)enif_alloc(name_length + 1);
  if (!enif_get_string(env, argv[1], name, name_length + 1, ERL_NIF_LATIN1)) {
    enif_free(name);
    return enif_make_badarg(env);
  }
  uint32_t result = ts_language_field_id_for_name(tslanguage, name, name_length);
  return enif_make_uint(env, result);
}

ERL_TS_FUNCTION(language_symbol_type_nif) {
  /* TSSymbolType ts_language_symbol_type(const TSLanguage *self, TSSymbol symbol); */
  void *res_tslanguage = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSLanguage, &res_tslanguage));
  const TSLanguage *tslanguage = ((struct_TSLanguage *)res_tslanguage)->val;

  uint32_t symbol;
  RETURN_BADARG_IF(!enif_get_uint(env, argv[1], &symbol));

  TSSymbolType symbol_type = ts_language_symbol_type(tslanguage, symbol);
  return tssymbol_type_to_atom(env, symbol_type);
}

ERL_TS_FUNCTION(language_version_nif) {
  void *res_language = NULL;
  RETURN_BADARG_IF(!enif_get_resource(env, argv[0], res_TSLanguage, &res_language));
  const TSLanguage *language = ((struct_TSLanguage *)res_language)->val;

  return enif_make_uint(env, ts_language_version(language));
}

ERL_TS_FUNCTION(language_next_state_nif) {
  /* TODO: */
  /* TSStateId ts_language_next_state(const TSLanguage *self, TSStateId state, TSSymbol symbol); */
  return atom_undefined;
}

ERL_TS_FUNCTION(language_name_nif) {
  /* TODO: TREE_SITTER_LANGUAGE_VERSION > 14 */
  /* const char *ts_language_name(const TSLanguage *self); */
  return atom_undefined;
}

/********************************/
/* Section - Lookahead Iterator */
/********************************/

ERL_TS_FUNCTION(lookahead_iterator_new_nif) {
  /* TODO: */
  /* TSLookaheadIterator *ts_lookahead_iterator_new(const TSLanguage *self, TSStateId state); */
  return atom_undefined;
}

ERL_TS_FUNCTION(lookahead_iterator_delete_nif) {
  /* TODO: */
  /* void ts_lookahead_iterator_delete(TSLookaheadIterator *self); */
  return atom_undefined;
}

ERL_TS_FUNCTION(lookahead_iterator_reset_state_nif) {
  /* TODO: */
  /* bool ts_lookahead_iterator_reset_state(TSLookaheadIterator *self, TSStateId state); */
  return atom_undefined;
}

ERL_TS_FUNCTION(lookahead_iterator_reset_nif) {
  /* TODO: */
  /* bool ts_lookahead_iterator_reset(TSLookaheadIterator *self, const TSLanguage *language, TSStateId state); */
  return atom_undefined;
}

ERL_TS_FUNCTION(lookahead_iterator_language_nif) {
  /* TODO: */
  /* const TSLanguage *ts_lookahead_iterator_language(const TSLookaheadIterator *self); */
  return atom_undefined;
}

ERL_TS_FUNCTION(lookahead_iterator_next_nif) {
  /* TODO: */
  /* bool ts_lookahead_iterator_next(TSLookaheadIterator *self); */
  return atom_undefined;
}

ERL_TS_FUNCTION(lookahead_iterator_current_symbol_nif) {
  /* TODO: */
  /* TSSymbol ts_lookahead_iterator_current_symbol(const TSLookaheadIterator *self); */
  return atom_undefined;
}

ERL_TS_FUNCTION(lookahead_iterator_current_symbol_name_nif) {
  /* TODO: */
  /* const char *ts_lookahead_iterator_current_symbol_name(const TSLookaheadIterator *self); */
  return atom_undefined;
}

/*=====================*/
/* For testing purpose */
/*=====================*/
ERL_TS_FUNCTION(tree_sitter_erlang_nif) {
  const TSLanguage *language = tree_sitter_erlang();
  if (!language)
    return mk_error(env, "unable_to_create_language_erlang");
  struct_TSLanguage *res_language =
    enif_alloc_resource(res_TSLanguage, sizeof(struct_TSLanguage));
  res_language->val = language;
  ERL_NIF_TERM term_language = enif_make_resource(env, res_language);
  enif_release_resource(res_language);
  return enif_make_tuple2(env, atom_ok, term_language);
}

static ErlNifFunc nif_funcs[] = {
  ERL_TS_FUNCTION_ARRAY(tree_sitter_erlang, 0),
  ERL_TS_FUNCTION_ARRAY(parser_new, 0),
  ERL_TS_FUNCTION_ARRAY(parser_delete, 1),
  ERL_TS_FUNCTION_ARRAY(parser_language, 1),
  ERL_TS_FUNCTION_ARRAY(parser_set_language, 2),
  ERL_TS_FUNCTION_ARRAY(parser_set_included_ranges, 3),
  ERL_TS_FUNCTION_ARRAY(parser_included_ranges, 1),
  ERL_TS_FUNCTION_ARRAY(parser_parse, 3),
  ERL_TS_FUNCTION_ARRAY(parser_parse_with_options, 4),
  ERL_TS_FUNCTION_ARRAY(parser_parse_string, 2),
  ERL_TS_FUNCTION_ARRAY(parser_reparse_string, 3),
  ERL_TS_FUNCTION_ARRAY(parser_parse_string_encoding, 5),
  ERL_TS_FUNCTION_ARRAY(parser_reset, 1),
  ERL_TS_FUNCTION_ARRAY(parser_set_timeout_micros, 2),
  ERL_TS_FUNCTION_ARRAY(parser_timeout_micros, 1),
  ERL_TS_FUNCTION_ARRAY(parser_set_cancellation_flag, 2),
  ERL_TS_FUNCTION_ARRAY(parser_cancellation_flag, 1),
  ERL_TS_FUNCTION_ARRAY(parser_set_logger, 2),
  ERL_TS_FUNCTION_ARRAY(parser_logger, 1),
  ERL_TS_FUNCTION_ARRAY(parser_print_dot_graphs, 2),
  ERL_TS_FUNCTION_ARRAY(tree_copy, 1),
  ERL_TS_FUNCTION_ARRAY(tree_delete, 1),
  ERL_TS_FUNCTION_ARRAY(tree_root_node, 1),
  ERL_TS_FUNCTION_ARRAY(tree_root_node_with_offset, 3),
  ERL_TS_FUNCTION_ARRAY(tree_language, 1),
  ERL_TS_FUNCTION_ARRAY(tree_included_ranges, 2),
  ERL_TS_FUNCTION_ARRAY(tree_edit, 2),
  ERL_TS_FUNCTION_ARRAY(tree_get_changed_ranges, 3),
  ERL_TS_FUNCTION_ARRAY(tree_print_dot_graph, 2),
  ERL_TS_FUNCTION_ARRAY(node_type, 1),
  ERL_TS_FUNCTION_ARRAY(node_symbol, 1),
  ERL_TS_FUNCTION_ARRAY(node_language, 1),
  ERL_TS_FUNCTION_ARRAY(node_grammar_type, 1),
  ERL_TS_FUNCTION_ARRAY(node_grammar_symbol, 1),
  ERL_TS_FUNCTION_ARRAY(node_start_byte, 1),
  ERL_TS_FUNCTION_ARRAY(node_start_point, 1),
  ERL_TS_FUNCTION_ARRAY(node_end_byte, 1),
  ERL_TS_FUNCTION_ARRAY(node_end_point, 1),
  ERL_TS_FUNCTION_ARRAY(node_string, 1),
  ERL_TS_FUNCTION_ARRAY(node_is_null, 1),
  ERL_TS_FUNCTION_ARRAY(node_is_named, 1),
  ERL_TS_FUNCTION_ARRAY(node_is_missing, 1),
  ERL_TS_FUNCTION_ARRAY(node_is_extra, 1),
  ERL_TS_FUNCTION_ARRAY(node_has_changes, 1),
  ERL_TS_FUNCTION_ARRAY(node_has_error, 1),
  ERL_TS_FUNCTION_ARRAY(node_is_error, 1),
  ERL_TS_FUNCTION_ARRAY(node_parse_state, 1),
  ERL_TS_FUNCTION_ARRAY(node_next_parse_state, 1),
  ERL_TS_FUNCTION_ARRAY(node_parent, 1),
  ERL_TS_FUNCTION_ARRAY(node_child_containing_descendant, 2),
  ERL_TS_FUNCTION_ARRAY(node_child_with_descendant, 2),
  ERL_TS_FUNCTION_ARRAY(node_child, 2),
  ERL_TS_FUNCTION_ARRAY(node_field_name_for_child, 2),
  ERL_TS_FUNCTION_ARRAY(node_field_name_for_named_child, 2),
  ERL_TS_FUNCTION_ARRAY(node_child_count, 1),
  ERL_TS_FUNCTION_ARRAY(node_named_child, 2),
  ERL_TS_FUNCTION_ARRAY(node_named_child_count, 1),
  ERL_TS_FUNCTION_ARRAY(node_child_by_field_name, 2),
  ERL_TS_FUNCTION_ARRAY(node_child_by_field_id, 2),
  ERL_TS_FUNCTION_ARRAY(node_next_sibling, 1),
  ERL_TS_FUNCTION_ARRAY(node_prev_sibling, 1),
  ERL_TS_FUNCTION_ARRAY(node_next_named_sibling, 1),
  ERL_TS_FUNCTION_ARRAY(node_prev_named_sibling, 1),
  ERL_TS_FUNCTION_ARRAY(node_first_child_for_byte, 2),
  ERL_TS_FUNCTION_ARRAY(node_first_named_child_for_byte, 2),
  ERL_TS_FUNCTION_ARRAY(node_descendant_count, 1),
  ERL_TS_FUNCTION_ARRAY(node_descendant_for_byte_range, 3),
  ERL_TS_FUNCTION_ARRAY(node_descendant_for_point_range, 3),
  ERL_TS_FUNCTION_ARRAY(node_named_descendant_for_byte_range, 3),
  ERL_TS_FUNCTION_ARRAY(node_named_descendant_for_point_range, 3),
  ERL_TS_FUNCTION_ARRAY(node_edit, 2),
  ERL_TS_FUNCTION_ARRAY(node_eq, 2),
  ERL_TS_FUNCTION_ARRAY(tree_cursor_new, 1),
  ERL_TS_FUNCTION_ARRAY(tree_cursor_delete, 1),
  ERL_TS_FUNCTION_ARRAY(tree_cursor_reset, 2),
  ERL_TS_FUNCTION_ARRAY(tree_cursor_reset_to, 2),
  ERL_TS_FUNCTION_ARRAY(tree_cursor_current_node, 1),
  ERL_TS_FUNCTION_ARRAY(tree_cursor_current_field_name, 1),
  ERL_TS_FUNCTION_ARRAY(tree_cursor_current_field_id, 1),
  ERL_TS_FUNCTION_ARRAY(tree_cursor_goto_parent, 1),
  ERL_TS_FUNCTION_ARRAY(tree_cursor_goto_next_sibling, 1),
  ERL_TS_FUNCTION_ARRAY(tree_cursor_goto_previous_sibling, 1),
  ERL_TS_FUNCTION_ARRAY(tree_cursor_goto_first_child, 1),
  ERL_TS_FUNCTION_ARRAY(tree_cursor_goto_last_child, 1),
  ERL_TS_FUNCTION_ARRAY(tree_cursor_goto_descendant, 2),
  ERL_TS_FUNCTION_ARRAY(tree_cursor_current_descendant_index, 1),
  ERL_TS_FUNCTION_ARRAY(tree_cursor_current_depth, 1),
  ERL_TS_FUNCTION_ARRAY(tree_cursor_goto_first_child_for_byte, 2),
  ERL_TS_FUNCTION_ARRAY(tree_cursor_goto_first_child_for_point, 2),
  ERL_TS_FUNCTION_ARRAY(tree_cursor_copy, 1),
  ERL_TS_FUNCTION_ARRAY(query_new, 2),
  ERL_TS_FUNCTION_ARRAY(query_delete, 1),
  ERL_TS_FUNCTION_ARRAY(query_pattern_count, 1),
  ERL_TS_FUNCTION_ARRAY(query_capture_count, 1),
  ERL_TS_FUNCTION_ARRAY(query_string_count, 1),
  ERL_TS_FUNCTION_ARRAY(query_start_byte_for_pattern, 2),
  ERL_TS_FUNCTION_ARRAY(query_end_byte_for_pattern, 2),
  ERL_TS_FUNCTION_ARRAY(query_predicates_for_pattern, 3),
  ERL_TS_FUNCTION_ARRAY(query_is_pattern_rooted, 2),
  ERL_TS_FUNCTION_ARRAY(query_is_pattern_non_local, 2),
  ERL_TS_FUNCTION_ARRAY(query_is_pattern_guaranteed_at_step, 2),
  ERL_TS_FUNCTION_ARRAY(query_capture_name_for_id, 2),
  ERL_TS_FUNCTION_ARRAY(query_capture_quantifier_for_id, 3),
  ERL_TS_FUNCTION_ARRAY(query_string_value_for_id, 2),
  ERL_TS_FUNCTION_ARRAY(query_disable_capture, 3),
  ERL_TS_FUNCTION_ARRAY(query_disable_pattern, 2),
  ERL_TS_FUNCTION_ARRAY(query_capture, 2),
  ERL_TS_FUNCTION_ARRAY(query_cursor_new,0),
  ERL_TS_FUNCTION_ARRAY(query_cursor_delete,1),
  ERL_TS_FUNCTION_ARRAY(query_cursor_exec,3),
  ERL_TS_FUNCTION_ARRAY(query_cursor_exec_with_options,4),
  ERL_TS_FUNCTION_ARRAY(query_cursor_did_exceed_match_limit,1),
  ERL_TS_FUNCTION_ARRAY(query_cursor_match_limit,1),
  ERL_TS_FUNCTION_ARRAY(query_cursor_set_match_limit,2),
  ERL_TS_FUNCTION_ARRAY(query_cursor_set_timeout_micros,2),
  ERL_TS_FUNCTION_ARRAY(query_cursor_timeout_micros,1),
  ERL_TS_FUNCTION_ARRAY(query_cursor_set_byte_range,3),
  ERL_TS_FUNCTION_ARRAY(query_cursor_set_point_range,3),
  ERL_TS_FUNCTION_ARRAY(query_cursor_next_match,2),
  ERL_TS_FUNCTION_ARRAY(query_cursor_remove_match,2),
  ERL_TS_FUNCTION_ARRAY(query_cursor_next_capture,3),
  ERL_TS_FUNCTION_ARRAY(query_cursor_set_max_start_depth,2),
  ERL_TS_FUNCTION_ARRAY(language_copy,1),
  ERL_TS_FUNCTION_ARRAY(language_delete,1),
  ERL_TS_FUNCTION_ARRAY(language_symbol_count,1),
  ERL_TS_FUNCTION_ARRAY(language_state_count,1),
  ERL_TS_FUNCTION_ARRAY(language_symbol_name,2),
  ERL_TS_FUNCTION_ARRAY(language_symbol_for_name,4),
  ERL_TS_FUNCTION_ARRAY(language_field_count,1),
  ERL_TS_FUNCTION_ARRAY(language_field_name_for_id,2),
  ERL_TS_FUNCTION_ARRAY(language_field_id_for_name,2),
  ERL_TS_FUNCTION_ARRAY(language_symbol_type,2),
  ERL_TS_FUNCTION_ARRAY(language_version,1),
  ERL_TS_FUNCTION_ARRAY(language_next_state,3),
  ERL_TS_FUNCTION_ARRAY(language_name,1),
  ERL_TS_FUNCTION_ARRAY(lookahead_iterator_new,2),
  ERL_TS_FUNCTION_ARRAY(lookahead_iterator_delete,1),
  ERL_TS_FUNCTION_ARRAY(lookahead_iterator_reset_state,2),
  ERL_TS_FUNCTION_ARRAY(lookahead_iterator_reset,3),
  ERL_TS_FUNCTION_ARRAY(lookahead_iterator_language,1),
  ERL_TS_FUNCTION_ARRAY(lookahead_iterator_next,1),
  ERL_TS_FUNCTION_ARRAY(lookahead_iterator_current_symbol,1),
  ERL_TS_FUNCTION_ARRAY(lookahead_iterator_current_symbol_name,1)
};

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  res_TSParser = enif_open_resource_type(env, NULL, "TSParser", free_TSParser,
                                         ERL_NIF_RT_CREATE, NULL);
  res_TSLanguage = enif_open_resource_type(env, NULL, "TSLanguage", free_TSLanguage,
                                           ERL_NIF_RT_CREATE, NULL);
  res_TSTree = enif_open_resource_type(env, NULL, "TSTree", free_TSTree,
                                       ERL_NIF_RT_CREATE, NULL);
  res_TSQuery = enif_open_resource_type(env, NULL, "TSQuery", free_TSQuery,
                                        ERL_NIF_RT_CREATE, NULL);
  res_TSNode = enif_open_resource_type(env, NULL, "TSNode", free_TSNode,
                                       ERL_NIF_RT_CREATE, NULL);
  res_TSQueryCursor = enif_open_resource_type(env, NULL, "TSQueryCursor", free_TSQueryCursor,
                                       ERL_NIF_RT_CREATE, NULL);
  res_TSTreeCursor = enif_open_resource_type(env, NULL, "TSTreeCursor", free_TSTreeCursor,
                                       ERL_NIF_RT_CREATE, NULL);
  if (!res_TSParser)
    return -1;
  if (!res_TSLanguage)
    return -1;
  if (!res_TSTree)
    return -1;
  if (!res_TSQuery)
    return -1;
  if (!res_TSQueryCursor)
    return -1;
  if (!res_TSTreeCursor)
    return -1;

  atom_ok = mk_atom(env, "ok");
  atom_error = mk_atom(env, "error");
  atom_undefined = mk_atom(env, "undefined");
  atom_true = mk_atom(env, "true");
  atom_false = mk_atom(env, "false");
  atom_row = mk_atom(env, "row");
  atom_column = mk_atom(env, "column");
  atom_start_point = mk_atom(env, "start_point");
  atom_end_point = mk_atom(env, "end_point");
  atom_old_end_point = mk_atom(env, "old_end_point");
  atom_start_byte = mk_atom(env, "start_byte");
  atom_end_byte = mk_atom(env, "end_byte");
  atom_old_end_byte = mk_atom(env, "old_end_byte");
  atom_TSSymbolTypeRegular = mk_atom(env, "type_regular");
  atom_TSSymbolTypeAnonymus = mk_atom(env, "type_anonymus");
  atom_TSSymbolTypeSuperType = mk_atom(env, "type_supertype");
  atom_TSSymbolTypeAuxiliary = mk_atom(env, "type_auxiliary");
  atom_TSQueryErrorNone      = mk_atom(env, "error_none");
  atom_TSQueryErrorSyntax    = mk_atom(env, "error_syntax");
  atom_TSQueryErrorNodeType  = mk_atom(env, "error_node_type");
  atom_TSQueryErrorField     = mk_atom(env, "error_field");
  atom_TSQueryErrorCapture   = mk_atom(env, "error_capture");
  atom_TSQueryErrorStructure = mk_atom(env, "error_structure");
  atom_TSQueryErrorLanguage = mk_atom(env, "error_language");

  return 0;
}

static int upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data,
                   ERL_NIF_TERM load_info) {
  *priv_data = *old_priv_data;
  return 0;
}

static void unload(ErlNifEnv *env, void *priv_data) {
}

ERL_NIF_INIT(erl_ts, nif_funcs, load, NULL, upgrade, unload)
