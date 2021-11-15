#include <stdbool.h>
#include <stddef.h>
#include <wctype.h>

#include <tree_sitter/parser.h>

enum TokenType {
  OCAML_COMMENT,
  COMMENT,
  LINE_MARKER,
};

#define TAKE_WHILE_1(predicate) \
  if (!predicate(lexer->lookahead)) return false; \
  while (predicate(lexer->lookahead)) lexer->advance(lexer, false);

void *tree_sitter_CameLigo_external_scanner_create() { return NULL; }
void tree_sitter_CameLigo_external_scanner_destroy(void *p) {}
void tree_sitter_CameLigo_external_scanner_reset(void *p) {}
unsigned tree_sitter_CameLigo_external_scanner_serialize(void *p, char *buffer) { return 0; }
void tree_sitter_CameLigo_external_scanner_deserialize(void *p, const char *b, unsigned n) {}

bool tree_sitter_CameLigo_external_scanner_scan(
    void *payload,
    TSLexer *lexer,
    const bool *valid_symbols) {
  while (iswspace(lexer->lookahead)) lexer->advance(lexer, true);

  if (lexer->lookahead == '(') {
    lexer->advance(lexer, false);
    if (lexer->lookahead != '*') {
      return false;
    }

    lexer->advance(lexer, false);

    bool after_star = false;
    unsigned nesting_depth = 1;
    for (;;) {
      switch (lexer->lookahead) {
      case '\0':
        return false;
      case '*':
        lexer->advance(lexer, false);
        after_star = true;
        break;
      case ')':
        if (after_star) {
          lexer->advance(lexer, false);
          after_star = false;
          nesting_depth--;
          if (nesting_depth == 0) {
            lexer->result_symbol = OCAML_COMMENT;
            return true;
          }
        } else {
          lexer->advance(lexer, false);
          after_star = false;
          if (lexer->lookahead == '*') {
            nesting_depth++;
            lexer->advance(lexer, false);
          }
        }
        break;
      default:
        lexer->advance(lexer, false);
        after_star = false;
        break;
      }
    }
  } else if (lexer->lookahead == '/') {
    lexer->advance(lexer, false);
    if (lexer->lookahead != '/') {
      return false;
    }

    lexer->advance(lexer, false);

    for (;;) {
      switch (lexer->lookahead) {
      case '\n':
      case '\0':
        lexer->result_symbol = COMMENT;
        return true;
      default:
        lexer->advance(lexer, false);
        break;
      }
    }
  } else if (lexer->lookahead == '#' && !lexer->get_column(lexer)) {
    // Lex line markers so that our comment lexer is happy.
    lexer->advance(lexer, false);
    TAKE_WHILE_1(iswspace);
    TAKE_WHILE_1(iswdigit); // linenum
    TAKE_WHILE_1(iswspace);
    if (lexer->lookahead != '"') return false; // filename
    lexer->advance(lexer, false);
    while (lexer->lookahead != '"') lexer->advance(lexer, false);
    if (lexer->lookahead != '"') return false;
    lexer->advance(lexer, false);
    if (lexer->lookahead != '\n') {
      TAKE_WHILE_1(iswspace);
      TAKE_WHILE_1(iswdigit); // flag
    }
    if (lexer->lookahead != '\n') return false;
    lexer->advance(lexer, false);
    lexer->result_symbol = LINE_MARKER;
    return true;
  }

  return false;
}
