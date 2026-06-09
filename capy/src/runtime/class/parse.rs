use crate::runtime::{Context, value::Value};
use super::descriptor::ClassDescriptor;
use super::slot::SlotSpec;
use super::table::{ClassListError, SchemeClassSpecError};

pub(crate) struct ParsedSlotSpec<'gc> {
    pub(crate) name: String,
    pub(crate) init_value: Value<'gc>,
    pub(crate) init_keyword: Value<'gc>,
    pub(crate) init_thunk: Value<'gc>,
    pub(crate) scheme_slot_ref: Value<'gc>,
    pub(crate) scheme_slot_set: Value<'gc>,
    pub(crate) scheme_slot_bound: Value<'gc>,
    pub(crate) getter: Value<'gc>,
    pub(crate) setter: Value<'gc>,
    pub(crate) accessor: Value<'gc>,
    pub(crate) immutable: bool,
    pub(crate) initializable: bool,
    pub(crate) settable: bool,
    pub(crate) class_allocated: bool,
}

impl<'gc> ParsedSlotSpec<'gc> {
    fn mutable(ctx: Context<'gc>, name: String) -> Self {
        let keyword = crate::runtime::value::Symbol::from_str(ctx, &name).into();
        Self {
            name,
            init_value: Value::empty(),
            init_keyword: keyword,
            init_thunk: Value::empty(),
            scheme_slot_ref: Value::empty(),
            scheme_slot_set: Value::empty(),
            scheme_slot_bound: Value::empty(),
            getter: Value::new(false),
            setter: Value::new(false),
            accessor: Value::new(false),
            immutable: false,
            initializable: true,
            settable: true,
            class_allocated: false,
        }
    }

    pub(crate) fn as_slot_spec(&self) -> SlotSpec<'_, 'gc> {
        let mut spec = if self.immutable {
            SlotSpec::immutable(self.name.as_str())
        } else {
            SlotSpec::mutable(self.name.as_str())
        }
        .with_init_value(self.init_value)
        .with_init_keyword(self.init_keyword)
        .with_init_thunk(self.init_thunk);
        if !self.initializable {
            spec = spec.without_initargs();
        }
        if !self.settable {
            spec = spec.without_setter();
        }
        if self.class_allocated {
            spec = spec.with_class_allocation();
        }
        spec = spec.with_scheme_slot_procedures(
            self.scheme_slot_ref,
            self.scheme_slot_set,
            self.scheme_slot_bound,
        );
        spec = spec.with_accessor_names(self.getter, self.setter, self.accessor);
        spec
    }
}

pub(crate) fn parse_slot_spec_list<'gc>(
    ctx: Context<'gc>,
    values: Value<'gc>,
) -> Result<Vec<ParsedSlotSpec<'gc>>, SchemeClassSpecError<'gc>> {
    let mut cursor = values;
    let mut specs = Vec::new();
    while !cursor.is_null() {
        if !cursor.is_pair() {
            return Err(SchemeClassSpecError::SlotsNotList(cursor));
        }
        specs.push(parse_slot_spec(ctx, cursor.car())?);
        cursor = cursor.cdr();
    }
    Ok(specs)
}

fn parse_slot_spec<'gc>(
    ctx: Context<'gc>,
    value: Value<'gc>,
) -> Result<ParsedSlotSpec<'gc>, SchemeClassSpecError<'gc>> {
    if let Some(symbol) = value.try_as::<crate::runtime::value::Symbol>() {
        return Ok(ParsedSlotSpec::mutable(ctx, symbol.to_string()));
    }
    if !value.is_pair() {
        return Err(SchemeClassSpecError::SlotNameNotSymbol(value));
    }
    let name_value = value.car();
    let Some(name) = name_value.try_as::<crate::runtime::value::Symbol>() else {
        return Err(SchemeClassSpecError::SlotNameNotSymbol(name_value));
    };
    let mut spec = ParsedSlotSpec::mutable(ctx, name.to_string());
    let mut options = value.cdr();
    while !options.is_null() {
        if !options.is_pair() {
            return Err(SchemeClassSpecError::SlotSpecNotList(options));
        }
        let option = options.car();
        let Some(keyword) = option.try_as::<crate::runtime::value::Keyword>() else {
            return Err(SchemeClassSpecError::SlotOptionNameNotKeyword(option));
        };
        let rest = options.cdr();
        if !rest.is_pair() {
            return Err(SchemeClassSpecError::SlotOptionMissingValue(option));
        }
        let value = rest.car();
        options = rest.cdr();
        match keyword.to_symbol().to_string().as_str() {
            "init-value" => spec.init_value = value,
            "init-keyword" => spec.init_keyword = slot_init_keyword_value(value),
            "init-thunk" => spec.init_thunk = value,
            "slot-ref" => spec.scheme_slot_ref = value,
            "slot-set!" => spec.scheme_slot_set = value,
            "slot-bound?" => spec.scheme_slot_bound = value,
            "getter" => spec.getter = value,
            "setter" => spec.setter = value,
            "accessor" => spec.accessor = value,
            "allocation" => match slot_allocation_value(value).as_deref() {
                Some("instance") => spec.class_allocated = false,
                Some("class") => spec.class_allocated = true,
                _ => return Err(SchemeClassSpecError::UnknownSlotOption(value)),
            },
            "immutable" => {
                spec.immutable = value.as_bool();
                if spec.immutable {
                    spec.settable = false;
                }
            }
            "initializable" => spec.initializable = value.as_bool(),
            "settable" => spec.settable = value.as_bool(),
            _ => return Err(SchemeClassSpecError::UnknownSlotOption(option)),
        }
    }
    Ok(spec)
}

fn slot_allocation_value<'gc>(value: Value<'gc>) -> Option<String> {
    let raw = if let Some(keyword) = value.try_as::<crate::runtime::value::Keyword>() {
        keyword.to_symbol().to_string()
    } else {
        value
            .try_as::<crate::runtime::value::Symbol>()
            .map(|symbol| symbol.to_string())?
    };
    Some(raw.strip_prefix(':').unwrap_or(raw.as_str()).to_owned())
}

fn slot_init_keyword_value<'gc>(value: Value<'gc>) -> Value<'gc> {
    if let Some(keyword) = value.try_as::<crate::runtime::value::Keyword>() {
        keyword.to_symbol().into()
    } else {
        value
    }
}

pub(crate) fn parse_class_list<'gc>(values: Value<'gc>) -> Result<Vec<ClassId>, ClassListError<'gc>> {
    let mut cursor = values;
    let mut ids = Vec::new();
    while !cursor.is_null() {
        if !cursor.is_pair() {
            return Err(ClassListError::NotList(cursor));
        }
        let value = cursor.car();
        let Some(class) = value.try_as::<ClassDescriptor>() else {
            return Err(ClassListError::NotClass(value));
        };
        ids.push(class.id());
        cursor = cursor.cdr();
    }
    Ok(ids)
}

pub(crate) fn parse_scheme_class_shape<'gc>(
    ctx: Context<'gc>,
    name: Gc<'gc, crate::runtime::value::Symbol<'gc>>,
    slot_specs: Value<'gc>,
    direct_supers: Value<'gc>,
) -> Result<(String, Vec<ClassId>, Vec<ParsedSlotSpec<'gc>>), SchemeClassSpecError<'gc>> {
    let slot_specs = parse_slot_spec_list(ctx, slot_specs)?;
    let direct_supers = parse_class_list(direct_supers).map_err(|error| match error {
        ClassListError::NotList(value) => SchemeClassSpecError::SupersNotList(value),
        ClassListError::NotClass(value) => SchemeClassSpecError::SuperNotClass(value),
    })?;
    let name = name.to_string();
    Ok((name, direct_supers, slot_specs))
}

use crate::rsgc::object::ClassId;
use crate::rsgc::Gc;
