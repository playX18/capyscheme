use rsgc::Gc;

use crate::{native_fn, runtime::prelude::*};

native_fn!(
    register_io_fns:

    pub ("file-exists?") fn file_exists<'gc>(nctx, path: Gc<'gc, Str<'gc>>) -> bool {
        let metadata = std::fs::metadata(path.to_string());
        nctx.return_(metadata.is_ok())
    }

    pub ("absolute-path-string?") fn is_absolute_path_string<'gc>(nctx, path: Gc<'gc, Str<'gc>>) -> bool {
        let p = std::path::PathBuf::from(path.to_string());
        nctx.return_(p.is_absolute())
    }


);

pub fn init_io<'gc>(ctx: Context<'gc>) {
    register_io_fns(ctx);
}
