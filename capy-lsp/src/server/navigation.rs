use std::collections::HashSet;

use lsp_types::{Location, Position, Range, Url};

use crate::protocol::{DocumentFacts, LocationFact, ReferenceFact};

use super::util::contains_position;

pub(super) fn selected_reference_definition(
    facts: &DocumentFacts,
    name: &str,
    position: Position,
) -> Option<LocationFact> {
    facts
        .references
        .iter()
        .find(|reference| reference.name == name && contains_position(reference.range, position))
        .and_then(|reference| reference.definition.clone())
}

pub(super) fn selected_symbol_definition(
    facts: &DocumentFacts,
    uri: &Url,
    name: &str,
    position: Position,
) -> Option<LocationFact> {
    facts
        .symbols
        .iter()
        .find(|symbol| symbol.name == name && contains_position(symbol.selection_range, position))
        .map(|symbol| LocationFact {
            uri: uri.clone(),
            range: symbol.selection_range,
        })
}

pub(super) fn reference_targets_definition(
    reference: &ReferenceFact,
    definition: &LocationFact,
) -> bool {
    reference.definition.as_ref().is_some_and(|candidate| {
        candidate.uri == definition.uri && candidate.range == definition.range
    })
}

pub(super) fn push_location(
    locations: &mut Vec<Location>,
    seen: &mut HashSet<(Url, Range)>,
    location: Location,
) {
    if seen.insert((location.uri.clone(), location.range)) {
        locations.push(location);
    }
}
