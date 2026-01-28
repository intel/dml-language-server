//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use log::*;

use crate::span::{Range, ZeroIndexed};
use serde::{Deserialize, Serialize};

use crate::actions::InitActionContext;
use crate::lsp_data::*;
use crate::server::{Output, ResponseError};

#[derive(Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct Tooltip {
    pub contents: Vec<MarkedString>,
    pub range: Range<ZeroIndexed>,
}

// Build a hover tooltip
pub fn tooltip<O: Output>(
    ctx: &mut InitActionContext<O>,
    params: &TextDocumentPositionParams,
) -> Result<Tooltip, ResponseError> {
    let hover_file_path = parse_file_path!(&params.text_document.uri, "hover")?;
    let hover_span = ctx.convert_pos_to_span(hover_file_path, params.position);
    // TODO: sort out how to handle hovers, and what info they should provide
    let contents = vec![];
    trace!("tooltip: contents.len: {}", contents.len());
    Ok(Tooltip { contents, range: hover_span.range })
}
