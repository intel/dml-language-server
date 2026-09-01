//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
pub mod methods;
pub mod objects;
pub mod topology;
pub mod types;
pub mod traits;

use crate::analysis::templating::types::DMLType;
use crate::analysis::structure::expressions::DMLString;
use crate::analysis::structure::objects::MaybeAbstract;
use crate::analysis::DMLNamed;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct Declaration {
    pub type_ref: DMLType,
    pub name: DMLString,
}

impl MaybeAbstract for Declaration {
    fn is_abstract(&self) -> bool {
        true
    }
}

impl DMLNamed for Declaration {
    fn name(&self) -> &DMLString {
        &self.name
    }
}
