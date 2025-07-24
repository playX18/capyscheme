use once_cell::unsync::OnceCell;
use std::collections::HashMap;
use std::{cell::Cell, collections::HashSet, fmt};

use crate::{Error, SyntaxError};
use crate::{ast::*, env::Environment, source::Span};

pub struct SyntaxRules {
    #[allow(dead_code)]
    name: Option<P<Symbol>>,
    ellipsis: P<Symbol>,
    reserved: HashSet<P<Symbol>>,
    literals: HashSet<P<Symbol>>,
    patterns: Vec<P<Datum>>,
    templates: Vec<P<Datum>>,
    lexical_env: P<Environment>,
}

impl SyntaxRules {
    pub fn new(
        name: Option<P<Symbol>>,
        ellipsis: Option<P<Symbol>>,
        literals: HashSet<P<Symbol>>,
        patterns: Vec<P<Datum>>,
        templates: Vec<P<Datum>>,
        lexical_env: P<Environment>,
    ) -> Self {
        let ellipsis = ellipsis.unwrap_or_else(|| Symbol::new("..."));
        let reserved = HashSet::from([ellipsis.clone(), Symbol::new("_")]);
        Self {
            name,
            reserved,
            ellipsis,
            literals,
            patterns,
            templates,
            lexical_env,
        }
    }

    /// Given a pattern, return the set of variables that match the pattern.
    pub fn variables(&self, pattern: &Datum) -> HashSet<P<Symbol>> {
        fn traverse(pattern: &DatumValue, rules: &SyntaxRules, set: &mut HashSet<P<Symbol>>) {
            match pattern {
                DatumValue::Symbol(sym) => {
                    let root = sym.root();
                    if !rules.literals.contains(&root) && !rules.reserved.contains(&root) {
                        set.insert(sym.clone());
                    }
                }

                DatumValue::Pair(car, cdr) => {
                    traverse(&car.value, rules, set);
                    traverse(&cdr.value, rules, set);
                }

                DatumValue::Vector(vec) => {
                    for item in vec {
                        traverse(&item.value, rules, set);
                    }
                }

                _ => {}
            }
        }

        let mut set = HashSet::new();
        traverse(&pattern.value, self, &mut set);
        set
    }

    pub fn instantiate_raw(&self, template: &P<Datum>, matches: &mut Matches) -> P<Datum> {
        match &template.value {
            DatumValue::Symbol(sym) => matches.get(sym, &self.lexical_env),
            DatumValue::Pair(_, _) => {
                let mut res = Vec::new();
                let mut templ = template.clone();

                while let DatumValue::Pair(token, rest) = &templ.value {
                    res.push(self.instantiate_raw(token, matches));
                    templ = rest.clone();
                }

                Datum::make_list_with(
                    &res,
                    self.instantiate_raw(&templ, matches),
                    Some(template.span()),
                )
            }

            DatumValue::Vector(v) => {
                let mut res = Vec::new();
                for item in v {
                    res.push(self.instantiate_raw(item, matches));
                }
                Datum::make_vector(res, Some(template.span()))
            }
            _ => template.clone(),
        }
    }

    pub fn instantiate(
        &self,
        template: &P<Datum>,
        matches: &mut Matches,
        depth: usize,
    ) -> Result<P<Datum>, Box<Error>> {
        let result = match &template.value {
            DatumValue::Symbol(sym) => matches.get(sym, &self.lexical_env),
            DatumValue::Pair(s, rest)
                if s.try_symbol()
                    .filter(|s| s.root() == self.ellipsis)
                    .is_some() =>
            {
                let DatumValue::Pair(car, _) = &rest.value else {
                    return Err(Box::new(Error::syntax(SyntaxError::Expected {
                        span: rest.span(),
                        expected: format!("pair"),
                        found: format!("{rest}"),
                    })));
                };
                println!("instantiate ellipsis: {s} {car} {rest}");
                self.instantiate_raw(car, matches)
            }
            DatumValue::Pair(_, _) => {
                let mut res = Vec::new();
                let mut templ = template.clone();
                let mut repeater = None;
                while let DatumValue::Pair(token, rest) = &templ.value {
                    if let DatumValue::Pair(s, _) = &rest.value
                        && s.try_symbol()
                            .filter(|s| s.root() == self.ellipsis)
                            .is_some()
                    {
                        if token
                            .try_symbol()
                            .filter(|s| s.root() == self.ellipsis)
                            .is_some()
                        {
                            return Err(Box::new(Error::syntax(
                                SyntaxError::MacroMismatchedRepetitionPatterns {
                                    span: token.span(),
                                    expected: format!("{}", s),
                                },
                            )));
                        }
                        println!("FOUND REPEATER: {token}");

                        repeater = Some(token.clone());
                    } else if let DatumValue::Symbol(s) = &token.value
                        && s.root() == self.ellipsis
                    {
                        let Some(repeater_template) = repeater.as_ref() else {
                            return Err(Box::new(Error::syntax(
                                SyntaxError::MacroMismatchedRepetitionPatterns {
                                    span: token.span(),
                                    expected: format!("repetition pattern before ellipsis"),
                                },
                            )));
                        };
                        println!("INSTANTIATE ELLIPSIS {s} {rest} WITH {repeater_template}");
                        matches.instantiate(
                            repeater_template.clone(),
                            self,
                            depth + 1,
                            &mut res,
                        )?;
                        repeater = None;
                    } else {
                        res.push(self.instantiate(token, matches, depth)?);
                    }

                    templ = rest.clone();
                }

                Datum::make_list_with(
                    &res,
                    self.instantiate(&templ, matches, depth)?,
                    Some(template.span()),
                )
            }
            DatumValue::Vector(v) => {
                let mut res = Vec::new();

                for i in 0..v.len() {
                    if i < v.len() - 1
                        && let DatumValue::Symbol(s) = &v[i + 1].value
                        && s.root() == self.ellipsis
                    {
                        if let DatumValue::Symbol(s) = &v[i].value
                            && s.root() == self.ellipsis
                        {
                            return Err(Box::new(Error::syntax(
                                SyntaxError::MacroMismatchedRepetitionPatterns {
                                    span: v[i].span(),
                                    expected: format!("repetition pattern before ellipsis"),
                                },
                            )));
                        }
                    } else if let DatumValue::Symbol(s) = &v[i].value
                        && s.root() == self.ellipsis
                    {
                        if i == 0 {
                            return Err(Box::new(Error::syntax(
                                SyntaxError::MacroMismatchedRepetitionPatterns {
                                    span: v[i].span(),
                                    expected: format!("repetition pattern before ellipsis"),
                                },
                            )));
                        }

                        matches.instantiate(v[i - 1].clone(), self, depth + 1, &mut res)?;
                    } else {
                        res.push(self.instantiate(&v[i], matches, depth)?);
                    }
                }

                Datum::make_vector(res, Some(template.span()))
            }
            _ => template.clone(),
        };

        println!("INSTANTIATE: {template} USING {matches} AT DEPTH {depth} => {result}");
        Ok(result)
    }

    pub fn expand(&self, input: &P<Datum>) -> Result<P<Datum>, Box<Error>> {
        for index in 0..self.patterns.len() {
            println!("try {} on {}", self.patterns[index], input);
            if let Some(mut matches) = self.r#match(&self.patterns[index], input) {
                println!("MATCHED: {}", matches);
                let ins = self.instantiate(&self.templates[index], &mut matches, 0)?;
                println!("INSTANTIATED: {}", ins);
                return Ok(ins);
            }
        }

        Err(Box::new(Error::syntax(SyntaxError::NoExpansion {
            span: input.span(),
        })))
    }

    pub fn r#match(&self, pattern: &P<Datum>, input: &P<Datum>) -> Option<Matches> {
        let mut matches = Matches::new(self.variables(pattern));

        self.match_impl(pattern, input, &mut matches, 0)
            .then_some(matches)
    }

    fn match_impl(
        &self,
        pattern: &P<Datum>,
        input: &P<Datum>,
        matches: &mut Matches,
        depth: usize,
    ) -> bool {
        println!("MATCH: {pattern} WITH: {input} MATCHING {matches}");
        match &pattern.value {
            DatumValue::Symbol(sym) => {
                if self.literals.contains(&sym.root()) {
                    if let DatumValue::Symbol(input_sym) = &input.value {
                        return sym.root() == input_sym.root();
                    } else {
                        return false;
                    }
                } else {
                    matches.put(sym.clone(), input.clone());
                    return true;
                }
            }

            DatumValue::Pair(_, _) => {
                match &input.value {
                    DatumValue::Null | DatumValue::Pair(_, _) => (),
                    _ => return false,
                }

                let mut pat = pattern.clone();
                let mut inp = input.clone();

                while let DatumValue::Pair(token, rest) = &pat.value {
                    if let DatumValue::Symbol(s) = &token.value
                        && s.root() == self.ellipsis
                    {

                        // ignore ellipsis
                    } else {
                        if let DatumValue::Pair(s, tail) = &rest.value
                            && let DatumValue::Symbol(s) = &s.value
                            && { s.root() == self.ellipsis }
                        {
                            // register variable
                            matches.register(self.variables(token), depth + 1);

                            let mut max_match_count = inp.length() - tail.length();

                            while let DatumValue::Pair(car, cdr) = &inp.value
                                && max_match_count > 0
                                && {
                                    let res = self.match_impl(token, car, matches, depth + 1);

                                    res
                                }
                            {
                                max_match_count -= 1;
                                inp = cdr.clone();
                            }
                        } else if let DatumValue::Pair(car, cdr) = &inp.value
                            && self.match_impl(token, car, matches, depth)
                        {
                            inp = cdr.clone();
                        } else {
                            return false;
                        }
                    }

                    pat = rest.clone();
                }

                self.match_impl(&pat, &inp, matches, depth)
            }

            DatumValue::Vector(pattern_vec) => {
                let DatumValue::Vector(input_vec) = &input.value else {
                    return false;
                };

                let mut input_index = 0;

                for pattern_index in 0..pattern_vec.len() {
                    let token = &pattern_vec[pattern_index];

                    if let DatumValue::Symbol(s) = &token.value
                        && s.root() == self.ellipsis
                    {
                        // ignore ellipsis
                        continue;
                    } else {
                        if pattern_index < pattern_vec.len() - 1
                            && let DatumValue::Symbol(s) = &pattern_vec[pattern_index + 1].value
                            && s.root() == self.ellipsis
                        {
                            // register variable
                            matches.register(self.variables(token), depth + 1);

                            let mut max_match_count = input_vec.len() - input_index;

                            while input_index < input_vec.len()
                                && max_match_count > 0
                                && self.match_impl(
                                    token,
                                    &input_vec[input_index],
                                    matches,
                                    depth + 1,
                                )
                            {
                                max_match_count -= 1;
                                input_index += 1;
                            }
                        } else if input_index < input_vec.len()
                            && self.match_impl(token, &input_vec[input_index], matches, depth)
                        {
                            input_index += 1;
                        } else {
                            return false;
                        }
                    }
                }

                input_index == input_vec.len()
            }

            _ => pattern == input,
        }
    }
}

pub enum Node {
    Leaf(P<Datum>),
    Parent(Box<Vec<Node>>),
}

impl Node {
    pub fn new() -> Self {
        Node::Parent(Box::new(Vec::new()))
    }

    pub fn append_child(&mut self, child: Node) {
        match self {
            Node::Parent(children) => {
                children.push(child);
            }

            _ => unreachable!("Cannot append child to a leaf node"),
        }
    }

    pub fn last_child(&self) -> Option<&Node> {
        match self {
            Node::Parent(children) => children.last(),
            Node::Leaf(_) => None,
        }
    }

    pub fn last_mut(&mut self) -> Option<&mut Node> {
        match self {
            Node::Parent(children) => children.last_mut(),
            Node::Leaf(_) => None,
        }
    }

    pub fn num_children(&self) -> usize {
        match self {
            Node::Parent(children) => children.len(),
            Node::Leaf(_) => 0,
        }
    }
}

impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Leaf(datum) => write!(f, "Leaf({:?})", datum),
            Node::Parent(children) => {
                write!(f, "Parent([")?;
                for (i, child) in children.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", child)?;
                }
                write!(f, "])")
            }
        }
    }
}

/// Class `Matches` represents the result of matching an input expression with a template.
/// Objects of class `Matches` contain a mapping from symbols to values in the input expression
/// as well as symbols to generated symbols. Generated symbols are needed to guarantee the
/// hygiene property of Scheme's macros.
pub struct Matches {
    /// Mapping from symbol in the template to the value in the output
    generated_symbols: HashMap<P<Symbol>, P<Symbol>>,
    matched_values: HashMap<P<Symbol>, MatchTree>,
}

impl fmt::Display for Matches {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        let mut sep = "";

        for (sym, tree) in self.matched_values.iter() {
            write!(f, "{sep}{sym} => {tree}")?;
            sep = ", ";
        }

        write!(f, "}}")
    }
}

impl Matches {
    pub fn new(syms: impl IntoIterator<Item = P<Symbol>>) -> Self {
        Self {
            generated_symbols: HashMap::new(),
            matched_values: syms
                .into_iter()
                .map(|sym| (sym.clone(), MatchTree::new()))
                .collect(),
        }
    }

    pub fn get(&mut self, sym: &P<Symbol>, env: &P<Environment>) -> P<Datum> {
        self.matched_values
            .get(sym)
            .and_then(|tree| tree.value())
            .unwrap_or_else(|| {
                if let Some(gensym) = self.generated_symbols.get(sym) {
                    return Datum::new(DatumValue::Symbol(gensym.clone()));
                }

                let gensym = P(Symbol::Generated(sym.clone(), P::downgrade(env)));
                self.generated_symbols.insert(sym.clone(), gensym.clone());
                Datum::new(DatumValue::Symbol(gensym))
            })
    }

    pub fn put(&mut self, sym: P<Symbol>, value: P<Datum>) {
        let Some(tree) = self.matched_values.get_mut(&sym) else {
            return;
        };

        tree.enter(value);
    }

    pub fn register(&mut self, syms: impl IntoIterator<Item = P<Symbol>>, at_depth: usize) {
        for sym in syms {
            if let Some(tree) = self.matched_values.get_mut(&sym) {
                tree.descend_at(at_depth);
            }
        }
    }

    pub fn instantiate(
        &mut self,
        template: P<Datum>,
        rules: &SyntaxRules,
        at_depth: usize,
        appending_to_exprs: &mut Vec<P<Datum>>,
    ) -> Result<(), Box<Error>> {
        let syms = rules.variables(&template);

        for _ in 0..self.num_children(syms.iter().cloned(), at_depth)? {
            appending_to_exprs.push(rules.instantiate(&template, self, at_depth)?);

            for sym in syms.iter() {
                let Some(tree) = self.matched_values.get_mut(sym) else {
                    continue;
                };
                tree.rotate_at(at_depth);
            }
        }

        Ok(())
    }

    pub fn num_children(
        &self,
        syms: impl Iterator<Item = P<Symbol>>,
        at_depth: usize,
    ) -> Result<usize, Box<Error>> {
        let mut res = 0;

        for sym in syms {
            if let Some(tree) = self.matched_values.get(&sym) {
                let s = tree.num_children(at_depth);
                if s > 0 {
                    if res != 0 && res != s {
                        return Err(Box::new(Error::syntax(
                            SyntaxError::MacroMismatchedRepetitionPatterns {
                                span: Span::default(),
                                expected: format!("{} children, found {}", res, s),
                            },
                        )));
                    }

                    res = s;
                }
            }
        }

        Ok(res)
    }
}

/// A match tree is a data structure that is used to construct the value matching a particular
/// pattern variable

pub struct MatchTree {
    #[allow(dead_code)]
    span: Span,
    root: Node,
    depth: usize,
    completed: Cell<bool>,
    positions: OnceCell<Vec<usize>>,
}

impl fmt::Display for MatchTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}; {}; ", self.depth, self.root)?;
        if self.completed.get() {
            for idx in self.positions() {
                write!(f, " {idx}")?;
            }
        }

        write!(f, ")")
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Leaf(datum) => write!(f, "{datum}"),
            Self::Parent(children) => {
                write!(f, "[")?;
                for (i, child) in children.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{child}")?;
                }
                write!(f, "]")
            }
        }
    }
}

impl MatchTree {
    pub fn new() -> Self {
        Self {
            root: Node::new(),
            depth: 0,
            completed: Cell::new(false),
            positions: OnceCell::new(),
            span: Span::default(),
        }
    }

    pub fn positions(&self) -> &[usize] {
        self.positions.get_or_init(|| {
            self.completed.set(true);
            vec![0; self.depth + 1]
        })
    }

    pub fn positions_mut(&mut self) -> &mut [usize] {
        self.positions.get_or_init(|| {
            self.completed.set(true);
            vec![0; self.depth + 1]
        });
        &mut *self.positions.get_mut().unwrap()
    }

    pub fn value(&self) -> Option<P<Datum>> {
        let Some(Node::Parent(children)) = self.current_node(self.depth) else {
            return None;
        };

        let Some(Node::Leaf(datum)) = children.get(self.positions()[self.depth]) else {
            return None;
        };

        Some(datum.clone())
    }

    pub fn tail_node(&self, depth: usize) -> &Node {
        let mut res = &self.root;
        for i in 0..depth {
            match res {
                Node::Parent(children) => {
                    res = children.last().unwrap_or_else(|| {
                        panic!("No children at depth {}", i);
                    });
                }
                Node::Leaf(_) => unreachable!("Expected parent node at depth {}", i),
            }
        }

        res
    }

    pub fn tail_node_mut(&mut self, depth: usize) -> &mut Node {
        let mut res = &mut self.root;
        for i in 0..depth {
            match res {
                Node::Parent(children) => {
                    res = children.last_mut().unwrap_or_else(|| {
                        panic!("No children at depth {}", i);
                    });
                }
                Node::Leaf(_) => unreachable!("Expected parent node at depth {}", i),
            }
        }

        res
    }

    pub fn current_node(&self, depth: usize) -> Option<&Node> {
        let mut res = Some(&self.root);
        for i in 0..depth {
            let Some(Node::Parent(children)) = res else {
                return None;
            };
            res = children.get(self.positions()[i]);
        }

        res
    }

    pub fn enter(&mut self, datum: P<Datum>) {
        self.tail_node_mut(self.depth)
            .append_child(Node::Leaf(datum));
    }

    pub fn num_children(&self, depth: usize) -> usize {
        if depth > self.depth {
            return 0;
        }

        self.current_node(depth)
            .map_or(0, |node| node.num_children())
    }

    pub fn descend_at(&mut self, depth: usize) {
        self.tail_node_mut(depth - 1).append_child(Node::new());
        self.depth = self.depth.max(depth);
    }

    pub fn rotate_at(&mut self, depth: usize) {
        if depth <= self.depth {
            self.positions_mut()[depth] += 1;
            if self.positions()[depth] >= self.current_node(depth).unwrap().num_children() {
                self.positions_mut()[depth] = 0;
            }
        }
    }
}
