//! @generated file, do not edit by hand, see `xtask/src/codegen.rs`

#![allow(bad_style, missing_docs, unreachable_pub)]
use num_derive::{FromPrimitive, ToPrimitive};
#[doc = r" The kind of syntax node, e.g. `ATOM`, `IF_KW`, or `DOT`."]
#[derive(
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Debug,
    FromPrimitive,
    ToPrimitive
)]
#[repr(u16)]
pub enum SyntaxKind {
    ANON_AFTER = 103u16,
    ANON_AND = 113u16,
    ANON_ANDALSO = 83u16,
    ANN_TYPE = 196u16,
    ANN_VAR = 197u16,
    ANON_RECORD_EXPR = 254u16,
    ANON_RECORD_FIELD_EXPR = 256u16,
    ANON_RECORD_UPDATE_EXPR = 255u16,
    ANONYMOUS_FUN = 273u16,
    ARITY = 276u16,
    ATOM = 1u16,
    ATTR_NAME = 191u16,
    B_GENERATOR = 234u16,
    ANON_BAND = 112u16,
    ANON_BANG = 81u16,
    ANON_BEGIN = 84u16,
    ANON_BEHAVIOR = 35u16,
    ANON_BEHAVIOUR = 33u16,
    BEHAVIOUR_ATTRIBUTE = 160u16,
    BIN_ELEMENT = 219u16,
    BINARY = 218u16,
    BINARY_COMPREHENSION = 228u16,
    BINARY_OP_EXPR = 210u16,
    BIT_SIZE_EXPR = 220u16,
    BIT_TYPE_LIST = 221u16,
    BIT_TYPE_UNIT = 226u16,
    BLOCK_EXPR = 216u16,
    ANON_BNOT = 108u16,
    ANON_BOR = 114u16,
    ANON_BSL = 116u16,
    ANON_BSR = 117u16,
    ANON_BXOR = 115u16,
    CALL = 262u16,
    CALLBACK = 186u16,
    ANON_CALLBACK = 70u16,
    ANON_CASE = 100u16,
    CASE_EXPR = 265u16,
    ANON_CATCH = 78u16,
    CATCH_CLAUSE = 282u16,
    CATCH_EXPR = 207u16,
    CHAR = 137u16,
    CLAUSE_BODY = 205u16,
    ANON_COLON = 4u16,
    ANON_COLON_COLON = 64u16,
    ANON_COLON_EQ = 98u16,
    ANON_COMMA = 30u16,
    COMMENT = 138u16,
    ANON_COMPILE = 47u16,
    COMPILE_OPTIONS_ATTRIBUTE = 166u16,
    CONCATABLES = 308u16,
    COND_MATCH_EXPR = 209u16,
    CR_CLAUSE = 268u16,
    ANON_D_AMP = 92u16,
    ANON_DASH = 7u16,
    ANON_DASH_DASH = 121u16,
    ANON_DASH_GT = 73u16,
    ANON_DEFINE = 28u16,
    ANON_DEPRECATED = 51u16,
    DEPRECATED_ATTRIBUTE = 168u16,
    DEPRECATED_FA = 173u16,
    DEPRECATED_FAS = 172u16,
    DEPRECATED_MODULE = 171u16,
    DEPRECATED_WILDCARD = 57u16,
    DEPRECATION_DESC = 174u16,
    ANON_DIV = 110u16,
    ANON_DOT = 2u16,
    ANON_DOT_DOT = 76u16,
    DOTDOTDOT = 77u16,
    ANON_ELIF = 26u16,
    ANON_ELSE = 20u16,
    ANON_END = 85u16,
    ANON_ENDIF = 22u16,
    ANON_EQ = 79u16,
    ANON_EQ_COLON_EQ = 128u16,
    ANON_EQ_EQ = 122u16,
    ANON_EQ_GT = 97u16,
    ANON_EQ_LT = 124u16,
    ANON_EQ_SLASH_EQ = 129u16,
    ANON_EXPORT = 37u16,
    EXPORT_ATTRIBUTE = 161u16,
    ANON_EXPORT_TYPE = 45u16,
    EXPORT_TYPE_ATTRIBUTE = 165u16,
    EXPR_ARGS = 304u16,
    EXTERNAL_FUN = 272u16,
    FA = 164u16,
    ANON_FEATURE = 53u16,
    FEATURE_ATTRIBUTE = 169u16,
    FIELD_EXPR = 260u16,
    FIELD_TYPE = 261u16,
    ANON_FILE = 49u16,
    FILE_ATTRIBUTE = 167u16,
    FLOAT = 133u16,
    ANON_FUN = 75u16,
    FUN_CLAUSE = 278u16,
    FUN_DECL = 192u16,
    FUN_TYPE = 199u16,
    FUN_TYPE_SIG = 200u16,
    FUNCTION_CLAUSE = 203u16,
    GENERATOR = 233u16,
    ANON_GT = 127u16,
    ANON_GT_EQ = 126u16,
    ANON_GT_GT = 87u16,
    GUARD = 306u16,
    GUARD_CLAUSE = 307u16,
    ANON_IF = 24u16,
    IF_CLAUSE = 264u16,
    IF_EXPR = 263u16,
    ANON_IFDEF = 16u16,
    ANON_IFNDEF = 18u16,
    ANON_IMPORT = 41u16,
    IMPORT_ATTRIBUTE = 162u16,
    ANON_INCLUDE = 8u16,
    ANON_INCLUDE_LIB = 12u16,
    INTEGER = 132u16,
    INTERNAL_FUN = 271u16,
    ANON_LBRACE = 55u16,
    ANON_LBRACK = 39u16,
    LC_EXPRS = 230u16,
    LC_OR_ZC_EXPR = 231u16,
    LIST = 217u16,
    LIST_COMPREHENSION = 227u16,
    ANON_LPAREN = 10u16,
    ANON_LT = 125u16,
    ANON_LT_COLON_DASH = 94u16,
    ANON_LT_COLON_EQ = 96u16,
    ANON_LT_DASH = 93u16,
    ANON_LT_EQ = 95u16,
    ANON_LT_LT = 86u16,
    MACRO_CALL_ARGS = 300u16,
    MACRO_CALL_EXPR = 299u16,
    MACRO_EXPR = 303u16,
    MACRO_LHS = 297u16,
    MACRO_STRING = 302u16,
    MAP_COMPREHENSION = 229u16,
    MAP_EXPR = 240u16,
    MAP_EXPR_UPDATE = 239u16,
    MAP_FIELD = 242u16,
    MAP_GENERATOR = 235u16,
    MATCH_EXPR = 208u16,
    ANON_MAYBE = 105u16,
    MAYBE_EXPR = 288u16,
    MODULE = 189u16,
    ANON_MODULE = 31u16,
    MODULE_ATTRIBUTE = 159u16,
    MULTI_STRING = 176u16,
    NOMINAL = 180u16,
    ANON_NOMINAL = 60u16,
    ANON_NOT = 109u16,
    ANON_OF = 101u16,
    OPAQUE = 181u16,
    ANON_OPAQUE = 62u16,
    ANON_OPTIONAL_CALLBACKS = 43u16,
    OPTIONAL_CALLBACKS_ATTRIBUTE = 163u16,
    ANON_OR = 118u16,
    ANON_ORELSE = 82u16,
    PAREN_EXPR = 215u16,
    PIPE = 198u16,
    ANON_PIPE = 74u16,
    ANON_PIPE_PIPE = 91u16,
    ANON_PLUS = 107u16,
    ANON_PLUS_PLUS = 120u16,
    ANON_POUND = 67u16,
    ANON_POUND_UNDERSCORE = 99u16,
    PP_DEFINE = 157u16,
    PP_ELIF = 156u16,
    PP_ELSE = 153u16,
    PP_ENDIF = 154u16,
    PP_IF = 155u16,
    PP_IFDEF = 151u16,
    PP_IFNDEF = 152u16,
    PP_INCLUDE = 148u16,
    PP_INCLUDE_LIB = 149u16,
    PP_UNDEF = 150u16,
    ANON_QMARK = 106u16,
    ANON_QMARK_EQ = 80u16,
    QUALIFIED_RECORD_EXPR = 251u16,
    QUALIFIED_RECORD_FIELD_EXPR = 253u16,
    QUALIFIED_RECORD_NAME = 250u16,
    QUALIFIED_RECORD_UPDATE_EXPR = 252u16,
    RANGE_TYPE = 201u16,
    ANON_RBRACK = 40u16,
    ANON_RECEIVE = 102u16,
    RECEIVE_AFTER = 270u16,
    RECEIVE_EXPR = 269u16,
    ANON_RECORD = 65u16,
    RECORD_DECL = 184u16,
    RECORD_EXPR = 247u16,
    RECORD_FIELD = 259u16,
    RECORD_FIELD_EXPR = 245u16,
    RECORD_FIELD_NAME = 249u16,
    RECORD_INDEX_EXPR = 244u16,
    RECORD_NAME = 248u16,
    RECORD_UPDATE_EXPR = 246u16,
    ANON_REM = 111u16,
    REMOTE = 213u16,
    REMOTE_MODULE = 214u16,
    REPLACEMENT_CR_CLAUSES = 292u16,
    REPLACEMENT_EXPR_GUARD = 295u16,
    REPLACEMENT_FUNCTION_CLAUSES = 291u16,
    REPLACEMENT_GUARD_AND = 294u16,
    REPLACEMENT_GUARD_OR = 293u16,
    REPLACEMENT_PARENS = 296u16,
    ANON_RPAREN = 11u16,
    ANON_RRACE = 56u16,
    ANON_SEMI = 72u16,
    SHEBANG = 130u16,
    ANON_SLASH = 88u16,
    ANON_SLASH_EQ = 123u16,
    SOURCE_FILE = 142u16,
    SPEC = 185u16,
    ANON_SPEC = 68u16,
    ANON_SSR = 3u16,
    SSR_DEFINITION = 144u16,
    ANON_SSR_MATCH = 5u16,
    SSR_REPLACEMENT = 145u16,
    SSR_WHEN = 146u16,
    ANON_STAR = 89u16,
    STRING = 315u16,
    ANON_TRY = 104u16,
    TRY_AFTER = 281u16,
    TRY_CLASS = 283u16,
    TRY_EXPR = 279u16,
    TRY_STACK = 284u16,
    TUPLE = 238u16,
    ANON_TYPE = 58u16,
    TYPE_ALIAS = 179u16,
    TYPE_GUARDS = 195u16,
    TYPE_NAME = 183u16,
    TYPE_SIG = 194u16,
    UNARY_OP_EXPR = 211u16,
    ANON_UNDEF = 14u16,
    ANON_UNIT = 90u16,
    VAR = 131u16,
    VAR_ARGS = 305u16,
    ANON_WHEN = 6u16,
    WILD_ATTRIBUTE = 190u16,
    ANON_XOR = 119u16,
    WHITESPACE = 316u16,
    ERROR = u16::MAX,
}
use self::SyntaxKind::*;
impl SyntaxKind {
    #[allow(clippy::match_like_matches_macro)]
    pub fn is_keyword(&self) -> bool {
        match self {
            ANON_AFTER
            | ANON_AND
            | ANON_ANDALSO
            | ANON_BAND
            | ANON_BEGIN
            | ANON_BEHAVIOR
            | ANON_BEHAVIOUR
            | ANON_BNOT
            | ANON_BOR
            | ANON_BSL
            | ANON_BSR
            | ANON_BXOR
            | ANON_CALLBACK
            | ANON_CASE
            | ANON_CATCH
            | ANON_COMPILE
            | ANON_DEFINE
            | ANON_DEPRECATED
            | ANON_DIV
            | ANON_ELIF
            | ANON_ELSE
            | ANON_END
            | ANON_ENDIF
            | ANON_EXPORT
            | ANON_EXPORT_TYPE
            | ANON_FEATURE
            | ANON_FILE
            | ANON_FUN
            | ANON_IF
            | ANON_IFDEF
            | ANON_IFNDEF
            | ANON_IMPORT
            | ANON_INCLUDE
            | ANON_INCLUDE_LIB
            | ANON_MAYBE
            | ANON_MODULE
            | ANON_NOMINAL
            | ANON_NOT
            | ANON_OF
            | ANON_OPAQUE
            | ANON_OPTIONAL_CALLBACKS
            | ANON_OR
            | ANON_ORELSE
            | ANON_RECEIVE
            | ANON_RECORD
            | ANON_REM
            | ANON_SPEC
            | ANON_SSR
            | ANON_TRY
            | ANON_TYPE
            | ANON_UNDEF
            | ANON_UNIT
            | ANON_WHEN
            | ANON_XOR => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    pub fn is_punct(&self) -> bool {
        match self {
            ANON_BANG
            | ANON_COLON
            | ANON_COLON_COLON
            | ANON_COLON_EQ
            | ANON_COMMA
            | ANON_D_AMP
            | ANON_DASH
            | ANON_DASH_DASH
            | ANON_DASH_GT
            | ANON_DOT
            | ANON_DOT_DOT
            | ANON_EQ
            | ANON_EQ_COLON_EQ
            | ANON_EQ_EQ
            | ANON_EQ_GT
            | ANON_EQ_LT
            | ANON_EQ_SLASH_EQ
            | ANON_GT
            | ANON_GT_EQ
            | ANON_GT_GT
            | ANON_LBRACE
            | ANON_LBRACK
            | ANON_LPAREN
            | ANON_LT
            | ANON_LT_COLON_DASH
            | ANON_LT_COLON_EQ
            | ANON_LT_DASH
            | ANON_LT_EQ
            | ANON_LT_LT
            | ANON_PIPE
            | ANON_PIPE_PIPE
            | ANON_PLUS
            | ANON_PLUS_PLUS
            | ANON_POUND
            | ANON_POUND_UNDERSCORE
            | ANON_QMARK
            | ANON_QMARK_EQ
            | ANON_RBRACK
            | ANON_RPAREN
            | ANON_RRACE
            | ANON_SEMI
            | ANON_SLASH
            | ANON_SLASH_EQ
            | ANON_SSR_MATCH
            | ANON_STAR => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    pub fn is_literal(&self) -> bool {
        match self {
            ATOM | CHAR | COMMENT | DEPRECATED_WILDCARD | DOTDOTDOT | FLOAT | INTEGER | SHEBANG
            | VAR => true,
            _ => false,
        }
    }
    pub fn is_token(&self) -> bool {
        self.is_keyword() || self.is_punct() || self.is_literal()
    }
}
#[doc = r" Tell emacs to automatically reload this file if it changes"]
#[doc = r" Local Variables:"]
#[doc = r" auto-revert-mode: 1"]
#[doc = r" End:"]
fn _dummy() -> bool {
    false
}
