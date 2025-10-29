//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
//! Configuration for the workspace that DLS is operating within and options for
//! tweaking the DLS's behavior itself.

use std::collections::HashMap;
use std::fmt::Debug;
use std::path::PathBuf;

use serde::de::Deserializer;
use serde::{Deserialize, Serialize};

use log::{error, trace};

use crate::lsp_data::SerializeError;

/// Some values in the config can be inferred without an explicit value set by
/// the user. There are no guarantees which values will or will not be passed
/// to the server, so we treat deserialized values effectively as `Option<T>`
/// and use `None` to mark the values as unspecified, otherwise we always use
/// `Specified` variant for the deserialized values. For user-provided `None`
/// values, they must be `Inferred` prior to usage (and can be further
/// `Specified` by the user).
#[derive(Clone, Debug, Serialize)]
pub enum Inferrable<T> {
    /// Explicitly specified value by the user. Retrieved by deserializing a
    /// non-`null` value. Can replace every other variant.
    Specified(T),
    /// Value that's inferred by the server. Can't replace a `Specified` variant.
    Inferred(T),
    /// Marker value that's retrieved when deserializing a user-specified `null`
    /// value. Can't be used alone and has to be replaced by server-`Inferred`
    /// or user-`Specified` value.
    None,
}

// Deserialize as if it's `Option<T>` and use `None` variant if it's `None`,
// otherwise use `Specified` variant for deserialized value.
impl<'de, T: Deserialize<'de>> Deserialize<'de> for Inferrable<T> {
    fn deserialize<D>(deserializer: D) -> Result<Inferrable<T>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value = Option::<T>::deserialize(deserializer)?;
        Ok(match value {
            None => Inferrable::None,
            Some(value) => Inferrable::Specified(value),
        })
    }
}

impl<T> Inferrable<T> {
    pub fn is_none(&self) -> bool {
        matches!(*self, Inferrable::None)
    }
}

impl<T: Clone + Debug> Inferrable<T> {
    /// Combine these inferrable values, preferring our own specified values
    /// when possible, and falling back the given default value.
    pub fn combine_with_default(&self, new: &Self, default: T) -> Self {
        match (self, new) {
            // Don't allow to update a Specified value with an Inferred one
            (&Inferrable::Specified(_), &Inferrable::Inferred(_)) => self.clone(),
            // When trying to update with a `None`, use Inferred variant with
            // a specified default value, as `None` value can't be used directly
            (_, &Inferrable::None) => Inferrable::Inferred(default),
            _ => new.clone(),
        }
    }

    /// Infer the given value if we don't already have an explicitly specified
    /// value.
    pub fn infer(&mut self, value: T) {
        if let Inferrable::Specified(_) = *self {
            trace!("Trying to infer {:?} on a {:?}", value, self);
            return;
        }

        *self = Inferrable::Inferred(value);
    }
}

impl<T> AsRef<T> for Inferrable<T> {
    fn as_ref(&self) -> &T {
        match *self {
            Inferrable::Inferred(ref value) | Inferrable::Specified(ref value) => value,
            // Default values should always be initialized as `Inferred` even
            // before actual inference takes place, `None` variant is only used
            // when deserializing and should not be read directly (via `as_ref`)
            Inferrable::None => unreachable!(),
        }
    }
}

/// When to include a new device analysis into already opened
/// common-code files that already have an active device context
// TODO: support for non-all mode
// NOTE: future synthetic isolated context settings are NOT included here
#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
pub enum DeviceContextMode {
    // By default, every encountered context will be marked as being
    // reported for
    Always,
    // If a device context directly or indirectly imports any file
    // that is not included from an already active device context,
    // we activate the new context
    AnyNew,
    // If the device is in the same or child directory as an activated context,
    // or if an activated context is in a child directory of the device,
    // we active it
    SameModule,
    // Only active a device context if ALL the files it imports do not have any
    // device contexts active
    First,
    // Never active any device contexts automatically
    Never,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[allow(missing_docs)]
#[serde(default)]
pub struct Config {
    pub show_warnings: WarningFrequency,
    /// `true` to analyzes only on save, not on change
    /// Default: `false`.
    pub analyse_on_save: bool,
    pub features: Vec<String>,
    pub all_features: bool,
    pub suppress_imports: bool,
    pub linting_enabled: bool,
    pub lint_cfg_path: Option<PathBuf>,
    pub lint_direct_only: bool,
    pub no_default_features: bool,
    pub server_debug_level: Option<log::LevelFilter>,
    // pub jobs: Option<u32>,
    pub compile_info_path: Option<PathBuf>,
    pub analysis_retain_duration: Option<f64>,
    pub new_device_context_mode: DeviceContextMode,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum WarningFrequency {
    Never,
    Always,
    Once,
}

impl serde::Serialize for WarningFrequency {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer
    {
        serializer.serialize_str(match self {
            WarningFrequency::Never => "never",
            WarningFrequency::Always => "always",
            WarningFrequency::Once => "once",
        })
    }
}

struct WarningFrequencyVisitor;

impl <'de> serde::de::Visitor<'de> for WarningFrequencyVisitor {
    type Value = WarningFrequency;

    fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>)
                 -> std::fmt::Result {
        formatter.write_str("a valid WarningFrequency string \
                             ('always', 'once', 'never')")
    }

    fn visit_str<E>(self, value: &str) -> Result<WarningFrequency, E>
    where
        E: serde::de::Error,
    {
        match value.to_lowercase().as_str() {
            "never" | "false" => Ok(WarningFrequency::Never),
            "once" => Ok(WarningFrequency::Once),
            "always" | "true" => Ok(WarningFrequency::Always),
            _ => Err(serde::de::Error::invalid_value(
                serde::de::Unexpected::Str(value),
                &"Invalid value for WarningFrequency \
                  (valid: never|once|always)"))
        }
    }

    fn visit_bool<E>(self, value: bool) -> Result<WarningFrequency, E>
    where
        E: serde::de::Error,
    {
        if value {
            Ok(WarningFrequency::Always)
        } else {
            Ok(WarningFrequency::Never)
        }
    }
}

impl<'de> Deserialize<'de> for WarningFrequency {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>
    {
        deserializer.deserialize_any(WarningFrequencyVisitor)
    }
}

impl Default for Config {
    fn default() -> Config {
        Config {
            show_warnings: WarningFrequency::Always,
            analyse_on_save: false,
            features: vec![],
            all_features: false,
            suppress_imports: false,
            linting_enabled: true,
            lint_cfg_path: None,
            lint_direct_only: true,
            server_debug_level: None,
            no_default_features: false,
            compile_info_path: None,
            analysis_retain_duration: None,
            new_device_context_mode: DeviceContextMode::Always,
        }
    }
}

lazy_static::lazy_static! {
    #[derive(Debug)]
    pub static ref DEPRECATED_OPTIONS: HashMap<&'static str, Option<&'static str>> = HashMap::default();
}

impl Config {
    #[allow(clippy::ptr_arg)]
    pub fn try_deserialize_vec(
        vals: &Vec<(String, serde_json::value::Value)>,
        dups: &mut std::collections::HashMap<String, Vec<String>>,
        unknowns: &mut Vec<String>,
        deprecated: &mut Vec<String>,
    ) -> Result<Config, SerializeError> {
        #[derive(Clone)]
        struct JsonValue(serde_json::value::Value);

        impl<'de> serde::de::IntoDeserializer<'de, serde_json::Error> for JsonValue {
            type Deserializer = serde_json::value::Value;
            fn into_deserializer(self) -> Self::Deserializer {
                self.0
            }
        }

        use heck::ToSnakeCase;
        let seq = serde::de::value::MapDeserializer::new(
            vals.iter().filter_map(|(k, v)| {
                let snake_case = k.to_snake_case();
                let vec = dups.entry(snake_case.clone()).or_default();
                vec.push(k.to_string());

                if vec.len() == 1 {
                    if DEPRECATED_OPTIONS.contains_key(snake_case.as_str()) {
                        deprecated.push(snake_case.clone());
                    }

                    Some((snake_case, JsonValue(v.to_owned())))
                } else {
                    None
                }
            }));
        match serde_ignored::deserialize(
            seq, |path| unknowns.push(path.to_string())) {
            Ok(conf) => {
                dups.retain(|_, v| v.len() > 1);
                return Ok(conf);
            }
            _ => {
                dups.retain(|_, v| v.len() > 1);
            }
        }
        Err(().into())
    }

    /// try to deserialize a Config from a json value, val is expected to be a
    /// Value::Object, all first level keys of val are converted to snake_case,
    /// duplicated and unknown keys are reported
    pub fn try_deserialize(
        val: &serde_json::value::Value,
        dups: &mut std::collections::HashMap<String, Vec<String>>,
        unknowns: &mut Vec<String>,
        deprecated: &mut Vec<String>,
    ) -> Result<Config, SerializeError> {
        if let serde_json::Value::Object(map) = val {
            return Self::try_deserialize_vec(
                &map.iter().map(|(k,v)|(k.clone(), v.clone())).collect(),
                dups, unknowns, deprecated
            );
        }
        Err(().into())
    }

    /// Join this configuration with the new config.
    pub fn update(&mut self, mut new: Config) {
        if let Some(new_retain) = new.analysis_retain_duration {
            if new_retain < 30.0 {
                error!("Wanted to update analysis retain policy to fresher \
                        than 30 seconds ({} seconds). This is disallowed since \
                        it is very likely to cause the server to discard an \
                        analysis before dependant analysises finish.",
                       new_retain);
                    new.analysis_retain_duration
                    = self.analysis_retain_duration;
            }
        }
        *self = new;
    }

    /// Checks if this config is incomplete, and needs additional values to be inferred.
    pub fn needs_inference(&self) -> bool {
        false
    }
}
