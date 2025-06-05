//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use core::fmt;
use std::collections::HashSet;
use std::str::FromStr;

use strum::EnumString;

use crate::analysis::structure::expressions::DMLString;
use crate::analysis::parsing::tree::LeafToken;
use crate::analysis::FileSpec;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum Provisional {
    // TODO: implement the neccessary checks for explicit params
    ExplicitParamDecl,
}

impl fmt::Display for Provisional {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct ProvisionalsManager {
    pub active_provisionals: HashSet<Provisional>,
    pub duped_provisionals: Vec<DMLString>,
    pub invalid_provisionals: Vec<DMLString>,
}

impl ProvisionalsManager {
    pub fn is_provisional_active(&self, provisional: Provisional) -> bool {
        self.active_provisionals.contains(&provisional)
    }
    fn handle_provisional_add(&mut self, provisional_ast: DMLString) {
        let provisional_str = provisional_ast.val.as_str();
        if let Ok(provisional) = Provisional::from_str(provisional_str) {
            if self.active_provisionals.contains(&provisional) {
                self.duped_provisionals.push(provisional_ast);
            } else {
                self.active_provisionals.insert(provisional);
            }
        } else {
            self.invalid_provisionals.push(provisional_ast);
        }
    }
    pub fn add_provisionals(&mut self,
                            provisionals: &Vec<(LeafToken, Option<LeafToken>)>,
                            file: FileSpec<'_>) {
        for (provisional_leaf, _) in provisionals {
            // No need to report an error for this, will be handled by
            // missing tokens later
            if !provisional_leaf.is_actual() {
                continue;
            }
            self.handle_provisional_add(
                DMLString::from_token(provisional_leaf, file).unwrap());
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_provisionals_parsing() {
        for (s, p) in [
            ("explicit_param_decl", Provisional::ExplicitParamDecl),
        ] {
            assert_eq!(Provisional::from_str(s), Ok(p));
        }
    }
}
