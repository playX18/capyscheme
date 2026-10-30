use std::io::Write;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;

pub const EXE_SECTION: &str = "CAPYSTAT";

/// Writes `out`: the bytes of `launcher` followed by `payload`, executable.
pub fn write_executable(launcher: &Path, payload: &[u8], out: &Path) -> std::io::Result<()> {
    let image = std::fs::read(launcher)?;
    #[cfg(target_os = "macos")]
    check_header_room(&image)?;
    let mut file = std::fs::File::create(out)?;
    #[cfg(target_os = "macos")]
    {
        libsui::Macho::from(image)
            .and_then(|macho| macho.write_section(EXE_SECTION, payload.to_vec()))
            .and_then(|macho| macho.build_and_sign(&mut file))
            .map_err(std::io::Error::other)?;
    }
    #[cfg(not(target_os = "macos"))]
    {
        file.write_all(&image)?;
        file.write_all(payload)?;
        file.write_all(&(payload.len() as u64).to_le_bytes())?;
        file.write_all(EXE_SECTION.as_bytes())?;
    }
    file.flush()?;
    drop(file);
    std::fs::set_permissions(out, std::fs::Permissions::from_mode(0o755))
}

#[cfg(target_os = "macos")]
fn check_header_room(image: &[u8]) -> std::io::Result<()> {
    use object::{Object, ObjectSection};
    /// A `LC_SEGMENT_64` with one section: 72 + 80 bytes.
    const NEW_COMMAND_BYTES: u64 = 152;
    let sizeofcmds = image
        .get(20..24)
        .map(|bytes| u32::from_le_bytes(bytes.try_into().expect("four bytes")) as u64)
        .ok_or_else(|| std::io::Error::other("not a Mach-O executable"))?;
    let commands_end = 32 + sizeofcmds;
    let file = object::File::parse(image).map_err(std::io::Error::other)?;
    let first_section = file
        .sections()
        .filter_map(|section| section.file_range().map(|(offset, _)| offset))
        .min()
        .unwrap_or(u64::MAX);
    if first_section < commands_end + NEW_COMMAND_BYTES {
        return Err(std::io::Error::other(
            "the launcher has no room for the bundle's load command (link it with -headerpad)",
        ));
    }
    Ok(())
}
