use lsp_types::{Range, Url};
use serde::Deserialize;

pub(super) const EXPAND_ACTION: &str = "capy.lsp.action.expand";

#[derive(Debug, Clone)]
pub(super) struct ActionDefinition {
    pub(super) command: &'static str,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(super) struct ActionCommandArgs {
    pub(super) uri: Url,
    #[serde(default)]
    pub(super) action: String,
    #[serde(default)]
    pub(super) range: Option<Range>,
}

pub(super) fn action_definitions() -> Vec<ActionDefinition> {
    vec![ActionDefinition {
        command: EXPAND_ACTION,
    }]
}

pub(super) fn known_action(action: &str) -> bool {
    action_definitions()
        .into_iter()
        .any(|definition| definition.command == action)
}
