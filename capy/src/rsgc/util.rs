use mmtk::util::Address;

pub fn align_allocation_no_fill(region: Address, alignment: usize, offset: usize) -> Address {
    let mask = (alignment - 1) as isize;
    let neg_off: isize = -(offset as isize);
    let delta = neg_off.wrapping_sub(region.as_usize() as isize) & mask;

    region + delta
}
