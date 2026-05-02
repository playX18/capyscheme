use lsp_types::{SemanticTokenModifier, SemanticTokenType, SemanticTokensLegend};

use crate::protocol::SymbolKindFact;

pub(super) fn semantic_tokens_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::NAMESPACE,
            SemanticTokenType::FUNCTION,
            SemanticTokenType::VARIABLE,
            SemanticTokenType::MACRO,
            SemanticTokenType::KEYWORD,
        ],
        token_modifiers: vec![SemanticTokenModifier::DEFINITION],
    }
}

pub(super) fn semantic_token_type(kind: SymbolKindFact) -> u32 {
    match kind {
        SymbolKindFact::Module => 0,
        SymbolKindFact::Function => 1,
        SymbolKindFact::Variable => 2,
        SymbolKindFact::Macro => 3,
        SymbolKindFact::Keyword => 4,
    }
}
