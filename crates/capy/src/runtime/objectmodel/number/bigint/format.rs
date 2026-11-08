use super::*;

impl<'gc> fmt::Binary for BigInt<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(
                f,
                "0b{}",
                self.to_string_with_options(&NumberToStringOptions {
                    base: &Base::BIN,
                    ..Default::default()
                })
            )
        } else {
            write!(
                f,
                "{}",
                self.to_string_with_options(&NumberToStringOptions {
                    base: &Base::BIN,
                    ..Default::default()
                })
            )
        }
    }
}

impl<'gc> fmt::Display for BigInt<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // `to_string_with_options` already includes the sign, passing the real
        // polarity here would print every negative as `--...`.
        f.pad_integral(true, "", &self.to_string_with_options(&Default::default()))
    }
}

impl<'gc> fmt::Debug for BigInt<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (base, group_sep, group_size) = if f.alternate() {
            (&Base::HEX, Some(Cow::Borrowed("_")), 4)
        } else {
            (&Base::DEC, Some(Cow::Borrowed(",")), 3)
        };

        let options = NumberToStringOptions {
            base,
            group_sep,
            group_size,
            force_sign: f.sign_plus(),
            plus_sign: Cow::Borrowed("+"),
            minus_sign: Cow::Borrowed("-"),
        };
        let s = self.to_string_with_options(&options);
        if f.alternate() && base.radix() == 16 {
            write!(f, "BigInt(0x{})", s)
        } else {
            write!(f, "BigInt({})", s)
        }
    }
}


#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Base {
    pub digit_space: &'static [char],
    pub digit_map: &'static [(char, u8)],
}

impl Base {
    pub const BIN: Base = Base {
        digit_space: &['0', '1'],
        digit_map: &[('0', 0), ('1', 1)],
    };

    pub const OCT: Base = Base {
        digit_space: &['0', '1', '2', '3', '4', '5', '6', '7'],
        digit_map: &[
            ('0', 0),
            ('1', 1),
            ('2', 2),
            ('3', 3),
            ('4', 4),
            ('5', 5),
            ('6', 6),
            ('7', 7),
        ],
    };

    pub const DEC: Base = Base {
        digit_space: &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'],
        digit_map: &[
            ('0', 0),
            ('1', 1),
            ('2', 2),
            ('3', 3),
            ('4', 4),
            ('5', 5),
            ('6', 6),
            ('7', 7),
            ('8', 8),
            ('9', 9),
        ],
    };

    pub const HEX: Base = Base {
        digit_space: &[
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
        ],
        digit_map: &[
            ('0', 0),
            ('1', 1),
            ('2', 2),
            ('3', 3),
            ('4', 4),
            ('5', 5),
            ('6', 6),
            ('7', 7),
            ('8', 8),
            ('9', 9),
            ('A', 10),
            ('B', 11),
            ('C', 12),
            ('D', 13),
            ('E', 14),
            ('F', 15),
        ],
    };

    pub fn radix(&self) -> usize {
        self.digit_space.len()
    }

    pub fn char_to_digit(&self, c: char) -> Option<u8> {
        self.digit_map
            .iter()
            .find_map(|(ch, v)| if *ch == c { Some(*v) } else { None })
    }

    pub fn of(radix: usize) -> Option<&'static Base> {
        match radix {
            2 => Some(&Base::BIN),
            8 => Some(&Base::OCT),
            10 => Some(&Base::DEC),
            16 => Some(&Base::HEX),
            _ => None,
        }
    }
}

pub struct NumberToStringOptions<'gc> {
    pub base: &'static Base,
    pub group_sep: Option<Cow<'gc, str>>,
    pub group_size: usize,
    pub force_sign: bool,
    pub plus_sign: Cow<'gc, str>,
    pub minus_sign: Cow<'gc, str>,
}

impl<'gc> Default for NumberToStringOptions<'gc> {
    fn default() -> Self {
        Self {
            base: &Base::DEC,
            group_sep: None,
            group_size: 3,
            force_sign: false,
            plus_sign: Cow::Borrowed("+"),
            minus_sign: Cow::Borrowed("-"),
        }
    }
}
impl<'gc> Hash for BigInt<'gc> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for &word in self.words_slice() {
            state.write_u64(word);
        }
    }
}

impl<'gc> PartialEq for BigInt<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == std::cmp::Ordering::Equal
    }
}

impl<'gc> Eq for BigInt<'gc> {}

impl<'gc> PartialOrd for BigInt<'gc> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'gc> Ord for BigInt<'gc> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.negative() != other.negative() {
            if self.negative() {
                return std::cmp::Ordering::Less;
            } else {
                return std::cmp::Ordering::Greater;
            }
        }

        if self.negative() {
            other.compare_digits(self)
        } else {
            self.compare_digits(other)
        }
    }
}

impl<'gc> PartialEq<i64> for BigInt<'gc> {
    fn eq(&self, other: &i64) -> bool {
        self.try_as_i64() == Some(*other)
    }
}
