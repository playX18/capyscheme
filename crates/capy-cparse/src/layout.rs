use crate::types::*;

fn round_up(v: u64, a: u64) -> u64 {
    if a <= 1 { v } else { v.div_ceil(a) * a }
}

impl Unit {
    /// `(size, alignment)` in bytes, or `None` if the type is incomplete or unsupported.
    pub fn size_align(&self, t: &Type) -> Option<(u64, u64)> {
        let abi = &self.abi;
        Some(match t {
            Type::Void => (1, 1),
            Type::Bool => (1, 1),
            Type::Int(k) => {
                let s = abi.int_size(*k);
                (s, if abi.weak_wide_align && s == 8 { 4 } else { s })
            }
            Type::BitInt(n, _) => {
                let bytes = (*n as u64).div_ceil(8);
                let size = if bytes <= 8 {
                    bytes.next_power_of_two()
                } else {
                    round_up(bytes, 8)
                };
                (size, size.min(16))
            }
            Type::Float(k) => (abi.float_size(*k), abi.float_align(*k)),
            Type::Complex(k) => (abi.float_size(*k) * 2, abi.float_align(*k)),
            Type::Ptr(_) | Type::NullPtr => (abi.ptr, abi.ptr),
            Type::Array(e, len) => {
                let (es, ea) = self.size_align(e)?;
                match len {
                    ArrLen::Fixed(n) => (es.checked_mul(*n)?, ea),
                    ArrLen::Incomplete => (0, ea),
                    ArrLen::Variable => return None,
                }
            }
            Type::Func(_) => (1, 1),
            Type::Record(id) => {
                let r = &self.records[*id];
                let l = r.layout.as_ref()?;
                if l.unsupported.is_some() {
                    return None;
                }
                (l.size, l.align)
            }
            Type::Enum(id) => return self.size_align(&self.enum_underlying(*id)),
            Type::Typedef(_) | Type::Atomic(_) => return self.size_align(&self.resolve(t)),
            Type::Vector(_, bytes) => (*bytes, (*bytes).min(16).max(1)),
            Type::VaList => match (abi.arch.as_str(), abi.os.as_str()) {
                ("x86_64", "macos" | "linux" | "freebsd") => (24, 8),
                ("aarch64", "linux") => (32, 8),
                _ => (abi.ptr, abi.ptr),
            },
            Type::Unsupported(_) => return None,
        })
    }

    pub fn enum_underlying(&self, id: EnumId) -> Type {
        let e = &self.enums[id];
        if let Some(t) = &e.fixed {
            return t.clone();
        }
        let (min, max) = e
            .consts
            .iter()
            .fold((0i128, 0i128), |(lo, hi), (_, v)| (lo.min(*v), hi.max(*v)));
        let fits = |k: IntKind, unsigned: bool| {
            let bits = self.abi.int_size(k) * 8;
            if unsigned {
                min >= 0 && max < (1i128 << bits)
            } else {
                min >= -(1i128 << (bits - 1)) && max < (1i128 << (bits - 1))
            }
        };
        if min >= 0 {
            if fits(IntKind::UInt, true) {
                return Type::Int(IntKind::UInt);
            }
            if fits(IntKind::ULong, true) {
                return Type::Int(IntKind::ULong);
            }
            Type::Int(IntKind::ULongLong)
        } else {
            if fits(IntKind::Int, false) {
                return Type::Int(IntKind::Int);
            }
            if fits(IntKind::Long, false) {
                return Type::Int(IntKind::Long);
            }
            Type::Int(IntKind::LongLong)
        }
    }

    /// Computes the layout of a completed record.
    pub fn compute_layout(&self, id: RecId) -> Layout {
        let rec = &self.records[id];
        let is_union = rec.kind == RecKind::Union;
        let mut out = Layout {
            align: 1,
            ..Default::default()
        };
        let mut bits: u64 = 0; // running offset in bits (structs)
        let mut size_bytes: u64 = 0; // running size for unions
        let mut max_align: u64 = 1;
        let n = rec.fields.len();

        for (i, f) in rec.fields.iter().enumerate() {
            let flexible = i + 1 == n && matches!(f.ty, Type::Array(_, ArrLen::Incomplete));
            let Some((fsize, falign)) = self.size_align(&f.ty) else {
                out.unsupported = Some(format!(
                    "field {} has unsupported or incomplete type",
                    f.name.as_deref().unwrap_or("<anonymous>")
                ));
                out.fields.push(FieldLayout::default());
                continue;
            };
            let _ = flexible;
            let packed = rec.packed || f.packed;
            let mut align = if packed { 1 } else { falign };
            if let Some(p) = rec.pack {
                align = align.min(p);
            }
            if let Some(a) = f.align_attr {
                align = align.max(a);
            }

            if let Some(w) = f.bits {
                let w = w as u64;
                if is_union {
                    out.fields.push(FieldLayout {
                        offset: 0,
                        bit: Some(BitLayout {
                            bit_offset: 0,
                            width: w as u32,
                            unit_size: fsize,
                        }),
                    });
                    size_bytes = size_bytes.max(w.div_ceil(8));
                    if w > 0 && f.name.is_some() {
                        max_align = max_align.max(align);
                    }
                    continue;
                }
                let unit_bits = fsize * 8;
                if w == 0 {
                    bits = round_up(bits, (falign * 8).max(1));
                    out.fields.push(FieldLayout {
                        offset: bits / 8,
                        bit: Some(BitLayout {
                            bit_offset: 0,
                            width: 0,
                            unit_size: fsize,
                        }),
                    });
                    continue;
                }
                let ua = (if packed { 1 } else { falign }) * 8;
                if !packed && (bits % ua) + w > unit_bits {
                    bits = round_up(bits, ua);
                }
                let unit_start = if packed {
                    bits / 8
                } else {
                    (bits / ua) * (ua / 8)
                };
                out.fields.push(FieldLayout {
                    offset: unit_start,
                    bit: Some(BitLayout {
                        bit_offset: bits - unit_start * 8,
                        width: w as u32,
                        unit_size: fsize,
                    }),
                });
                bits += w;
                if f.name.is_some() {
                    max_align = max_align.max(align);
                }
                continue;
            }

            if is_union {
                out.fields.push(FieldLayout {
                    offset: 0,
                    bit: None,
                });
                size_bytes = size_bytes.max(fsize);
            } else {
                bits = round_up(bits, align * 8);
                out.fields.push(FieldLayout {
                    offset: bits / 8,
                    bit: None,
                });
                bits += fsize * 8;
            }
            max_align = max_align.max(align);
        }

        if let Some(a) = rec.align_attr {
            max_align = max_align.max(a);
        }
        let raw = if is_union {
            size_bytes
        } else {
            bits.div_ceil(8)
        };
        out.align = max_align;
        out.size = round_up(raw, max_align);
        out
    }
}
