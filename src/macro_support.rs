//! Support items for macros.
//!
//! Signatures of the functions defined in this module may change at any time.
//! Use at your own risks.

use std::path::{Path, PathBuf};

use crate::{
    type_expr::TypeInfo, write_definition_file_from_type_infos,
    DefinitionFileOptions,
};

/// Re-export of `linkme` used in `export_all*` macros.
#[cfg(feature = "export-all")]
pub use linkme;

/// Returns the TypeScript source corresponding to the given [`TypeInfo`]s.
///
/// See [`write_definition_file_from_type_infos`] for more information.
pub fn type_infos_to_string(type_infos: &[&'static TypeInfo]) -> String {
    let mut buf = Vec::new();
    let options = DefinitionFileOptions {
        root_namespace: None,
        ..Default::default()
    };

    write_definition_file_from_type_infos(&mut buf, options, type_infos)
        .unwrap();

    String::from_utf8(buf).unwrap()
}

/// Joins to the given `path` starting at the current file (determined by
/// joining `manifest_dir` and `file_path`).
pub fn join_to_given_path(
    manifest_dir: &str,
    file_path: &str,
    path: impl AsRef<Path>,
) -> PathBuf {
    let mut result = PathBuf::from_iter([manifest_dir, file_path]);
    result.pop();
    result.push(path);
    result
}
