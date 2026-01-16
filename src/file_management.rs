//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
//! Contains utilities and file management
use log::{debug, error, trace};

use std::collections::HashMap;
use std::fs;

use std::path::{Path, PathBuf};
use std::ops::Deref;
use std::cell::RefCell;

// A path which we know to be canonical in the filesystem
#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct CanonPath(PathBuf);

impl Deref for CanonPath {
    type Target = PathBuf;
    fn deref(&self) -> &PathBuf {
        &self.0
    }
}

impl From<CanonPath> for PathBuf {
    fn from(from: CanonPath) -> PathBuf {
        from.to_path_buf()
    }
}

impl CanonPath {
    pub fn from_path_buf(from: PathBuf) -> Option<CanonPath> {
        match Self::try_from_path_buf(from) {
            Ok(path) => Some(path),
            Err(err) => {
                debug!("Failed to canonicalize a path; {:?}", err);
                None
            },
        }
    }

    pub fn try_from_path_buf(from: PathBuf) ->
        Result<CanonPath, std::io::Error> {
            trace!("Trying to canonicalize {:?}", from);
            fs::canonicalize(from).map(CanonPath)
        }

    pub fn as_str(&self) -> &str {
        self.0.to_str().unwrap()
    }
    pub fn as_path(&self) -> &Path {
        self.0.as_path()
    }
}

/// This is how we resolve relative paths to in-workspace full paths
#[derive(Debug, Clone)]
pub struct PathResolver {
    // Root is provided by context, who will pass this struct to threads for
    // import resolution
    roots: Vec<PathBuf>,
    include_paths: HashMap<CanonPath, Vec<PathBuf>>,
    #[allow(clippy::type_complexity)]
    cache: RefCell<HashMap<(PathBuf, Option<CanonPath>), Option<CanonPath>>>,
}

impl From<Option<PathBuf>> for PathResolver {
    fn from(path: Option<PathBuf>) -> PathResolver {
        let mut roots = vec![];
        if let Some(path) = path {
            roots.push(path);
        }
        PathResolver {
            roots,
            include_paths: HashMap::default(),
            cache: RefCell::default(),
        }
    }
}

impl PathResolver {
    pub fn add_paths<I>(&mut self, paths: I)
    where
        I: IntoIterator<Item = PathBuf> {
        self.roots.extend(paths);
    }

    pub fn set_include_paths(&mut self,
                             include_paths: &HashMap<CanonPath,
                                                     Vec<PathBuf>>) {
        self.include_paths = include_paths.clone();
    }

    pub fn resolve_under_any_context(&self, path: &Path)
                                     -> Option<CanonPath> {
        for context in self.include_paths.keys() {
            if let Some(cp) = self.resolve_with_maybe_context(
                path, Some(context), None) {
                return Some(cp);
            }
        }
        None
    }

    pub fn resolve_with_maybe_context(&self,
                                      path: &Path,
                                      context: Option<&CanonPath>,
                                      extra_path: Option<&CanonPath>
                                    ) -> Option<CanonPath> {
        self.cache.borrow_mut().entry((path.to_path_buf(), context.cloned()))
            .or_insert_with(
                ||if path.starts_with("./") || path.starts_with("../") {
                    self.resolve_from_relative(path, extra_path)
                } else {
                    self.resolve_with_maybe_context_impl(
                        path, context, extra_path)
                }
            )
            .clone()
    }

    fn resolve_from_relative(&self,
                             relative: &Path,
                             source: Option<&CanonPath>)
                             -> Option<CanonPath> {
        trace!("Resolving {:?} relative to {:?}", relative, source);
        if let Some(source_path) = source {
            if let Some(parent) = source_path.as_path().parent() {
                let to_try = parent.join(relative);
                trace!("Looking in {:?}", to_try);
                return Self::try_path(to_try);
            }
        }
        internal_error!(
            "No source path available when resolving a relative path");
        None
    }

    fn try_path(path: PathBuf) -> Option<CanonPath> {
        if let Ok(info) = fs::metadata(&path) {
            if info.is_file() {
                trace!("Resolved {:?} to {:?}", path,
                        // Fairly sure this can never fail
                        fs::canonicalize(&path).unwrap());
                return CanonPath::from_path_buf(path);
            }
        }
        None
    }

    fn resolve_with_maybe_context_impl(&self,
                                       path: &Path,
                                       context: Option<&CanonPath>,
                                       extra_path: Option<&CanonPath>)
                                       -> Option<CanonPath> {
        // Given some relative info, find a canonical file path
        // NOTE: Right now the relative info is a pathbuf, but this might
        // change later
        // rough priority is include_paths -> workspace_folders > root
        // Obtain workspace
        trace!("Resolving {:?} at {:?} with roots {:?} and includes {:?}",
               path, context, self.roots, self.include_paths);
        let mut roots = vec![];

        if let Some(extra) = context.cloned() {
                // Find the root path is the include path that is the longest
                // pre-path of context, those are the include paths to use
                let mut longest_path: Option<&CanonPath> = None;
                for root_path in self.include_paths.keys() {
                    if extra.starts_with(root_path.as_path()) {
                        if let Some(prev_long_path) = longest_path {
                            if root_path.0.iter().count() >
                                prev_long_path.iter().count() {
                                    longest_path = Some(root_path);
                                }
                        } else {
                            longest_path = Some(root_path);
                        }
                    }
                }
                if let Some(long_path) = longest_path {
                trace!("Longest predecessor was {:?}, adding {:?}", long_path,
                       self.include_paths[long_path]);
                    roots = self.include_paths[long_path].clone();
                }
                roots.extend(self.roots.clone());
                roots.push(extra.parent()?.to_path_buf());
            } else {
                roots.extend(self.roots.clone());
            }
        if let Some(extra) = extra_path {
            roots.push(extra.clone().into());
        }
        // directory order is largely undefined, I think
        for dir in roots {
            for to_try in [dir.join(path), dir.join("1.4").join(path)] {
                trace!("Looking in {:?}", to_try);
                let tried = Self::try_path(to_try.to_owned());
                if tried.is_some() {
                    return tried;
                }
            }
        }
        debug!("Failed to resolve {:?} to a real path", path);

        None
    }
}
