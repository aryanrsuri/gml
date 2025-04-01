# GML Parser & Language TODO List

This list tracks features and improvements needed for the GML parser, AST, and related components based on the current `parser.rs`.

## Core Language Features (Parser & AST)

-   [ ] **`let rec` Bindings:**
    -   [ ] Add `recursive: bool` flag to `ast::Statement::Let`.
    -   [ ] Modify `parse_let_statement` to detect and consume the `rec` keyword after `let` and set the flag.
-   [ ] **`let ... in ...` Expressions:**
    -   [ ] Add `Expression::LetIn { name, annotation, recursive, value, body }` variant to `ast::Expression`.
    -   [ ] Implement `parse_let_in_expression` triggered by `Token::Let` when parsing expressions. Handle optional `rec`.
-   [ ] **Record Literals & Types:**
    -   [ ] Add `ast::Expression::Record(Vec<(Identifier, Expression)>)` (anonymous fields `{ a=1; b=2 }`). Consider `RecordWithType(Identifier, Vec<...>)` if type name is needed for creation (`MyRecord { ... }`).
    -   [ ] Add `ast::TypeAnnotation::Record(Vec<(Identifier, TypeAnnotation)>)` or similar for record types.
    -   [ ] Implement `parse_record_expression` triggered by `Token::LBrace`. Parse `ident = expr` pairs separated by `;`.
    -   [ ] Extend `parse_type_annotation` for record types (e.g., `{ name: string; age: int }`).
    -   [ ] Add `Token::LBrace`, `Token::RBrace`.
-   [ ] **Variant Types & Values (Enums/Sum Types):**
    -   [ ] Add `ast::Expression::Variant(Identifier, Vec<Expression>)`. Consider `QualifiedVariant(TypeName, TagName, Args)` if tags aren't unique globally.
    -   [ ] Add `ast::TypeDefinition::Union { name, params, variants: Vec<UnionVariant> }` (already exists, ensure used).
    *   [ ] Add `ast::UnionVariant { tag, types }` (already exists, ensure used).
    -   [ ] Implement `parse_variant_expression` (e.g., `Some 5`, `Ok "val"`). Needs logic for parsing uppercase identifiers as variant tags, followed by optional arguments.
    -   [ ] Implement `parse_type_definition` for `Token::Type` to handle `type color = Red | Green | Blue of int`. Requires lexer support for `type`, `|`, `of`.
    -   [ ] Extend `parse_type_annotation` to handle defined union types (`color`).
    -   [ ] Add `Token::Type`, `Token::Pipe`, `Token::Of`.
-   [ ] **Member Access (Records):**
    -   [ ] Add `ast::Expression::MemberAccess(Box<Expression>, Identifier)` for `record.field`.
    -   [ ] Add `.` (dot) operator with high precedence (`Precedence::Index`).
    -   [ ] Extend `parse_expression`'s infix loop to handle `Token::Dot`, calling `parse_member_access_expression`.
    -   [ ] Add `Token::Dot`.

## Type System Features (Parser & AST)

-   [ ] **Tuple Type Annotations:**
    -   [ ] Add `ast::TypeAnnotation::Tuple(Vec<TypeAnnotation>)` (already exists, ensure used).
    -   [ ] Extend `parse_type_annotation` to handle `*` between type components (e.g., `int * string * bool`). Needs careful precedence handling with `->`.
    -   [ ] Add `Token::Asterisk` specifically for tuple types if it differs from multiplication.
-   [ ] **Parenthesized Type Annotations:**
    -   [ ] Enhance `parse_primitive_type_annotation` to handle `Token::LParen`, recursively call `parse_type_annotation`, and expect `Token::RParen`. This is crucial for precedence (`(int -> int) -> bool`).
-   [ ] **Type Variables (Generics/Polymorphism):**
    -   [ ] Add `ast::TypeAnnotation::Var(Identifier)` (e.g., for `'a`).
    -   [ ] Add parsing for `'a` syntax (likely `Token::Apostrophe` followed by `Token::Identifier`).
    -   [ ] Add `Token::Apostrophe`.
-   [ ] **Parameterized Types (Generics):**
    -   [ ] Add `ast::TypeAnnotation::GenericApply(Identifier, Vec<TypeAnnotation>)` (e.g., `list<int>`, `option<string>`). Maybe use existing `Product` variant? Rename might be clearer.
    -   [ ] Add parsing for syntax like `int list` or `list<int>`. Choose a syntax and implement. Needs `Token::LessThan`, `Token::GreaterThan` if using `<>`.

## Operators & Expressions (Parser & AST)

-   [ ] **Float Operators:**
    -   [ ] Add distinct tokens (`Token::PlusDot`, `MinusDot`, `AsteriskDot`, `ForwardSlashDot`).
    -   [ ] Add corresponding `ast::Infix` variants.
    -   [ ] Update `parse_infix_expression` to handle these tokens.
-   [ ] **String Concatenation:**
    -   [ ] Choose an operator (`++` or `^`). Add `Token` and `ast::Infix` variant.
    -   [ ] Update `parse_infix_expression`.
-   [ ] **List Append:**
    -   [ ] Choose an operator (`@`). Add `Token` and `ast::Infix` variant.
    -   [ ] Update `parse_infix_expression`. Handle precedence relative to `::` (cons).

## Parser Improvements

-   [ ] **Tuple Parsing Robustness:**
    -   [ ] Review `parse_grouped_expression` and `parse_tuple_expression`. Ensure `(expr)` is parsed as `expr` (grouped) and not a single-element tuple. Ensure correct token consumption, especially around `;` and `)`.
-   [ ] **Sequence Parsing:**
    -   [ ] Clarify where `;` (sequence) is allowed. Currently seems tied to infix parsing. Is `expr1; expr2` allowed at the top level or only within `()` or `begin/end` (if added)? Adjust `parse_infix_expression` or statement parsing as needed.
-   [ ] **Error Recovery:**
    -   [ ] Implement basic error recovery. When an error occurs in `parse_statement` or `parse_expression`, attempt to advance tokens until a likely synchronization point (e.g., `;;`, `let`, `in`, `end`) to allow parsing subsequent code.
-   [ ] **Error Messages:**
    -   [ ] Improve error messages (e.g., use string descriptions instead of raw tokens in `TokenExpected`, `TokenCurrMismatch`, etc.).
-   [ ] **End-of-File Handling:** Ensure robust handling of unexpected EOF in various parsing functions.

## General

-   [ ] **Testing:** Add comprehensive unit and integration tests covering all syntax variants, operator precedences, and error conditions.
-   [ ] **Code Cleanup:** Refactor complex functions (`parse_expression`, `parse_type_annotation`) for clarity if needed. Remove `println!` debugging statements.
