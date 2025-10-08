//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

use jsonrpc::error::StandardError::InternalError;
use log::{info, error, debug};

use crate::actions::work_pool;
use crate::actions::work_pool::WorkDescription;
use crate::actions::InitActionContext;
use crate::concurrency::{ConcurrentJob, JobToken};
use crate::lsp_data::LSPRequest;
use crate::server;
use crate::server::io::Output;
use crate::server::message::{RawResponse, ResponseError};
use crate::server::{Request, Response};

use crate::actions::requests::*;

/// Timeout time for request responses. By default a LSP client request not
/// responded to after this duration will return a fallback response.
#[cfg(not(test))]
pub const DEFAULT_REQUEST_TIMEOUT: Duration = Duration::from_millis(1500);

// Timeout lengthened to "never" for potentially very slow CI boxes
#[cfg(test)]
pub const DEFAULT_REQUEST_TIMEOUT: Duration = Duration::from_millis(3_600_000);

/// Macro enum `DispatchRequest` packing in various similar `Request` types
macro_rules! define_dispatch_request_enum {
    ($($request_type:ident),*$(,)*) => {
        // Seems ok for a short-lived macro-enum.
        #[allow(clippy::large_enum_variant)]
        pub(crate) enum DispatchRequest {
            $(
                $request_type(Request<$request_type>),
            )*
        }

        $(
            impl From<Request<$request_type>> for DispatchRequest {
                fn from(req: Request<$request_type>) -> Self {
                    DispatchRequest::$request_type(req)
                }
            }
        )*

        impl DispatchRequest {
            fn get_identifier(&self) -> String {
                match self {
                    $(
                        DispatchRequest::$request_type(req) =>
                            $request_type::get_identifier(&req.params),
                    )*
                }
            }

            fn handle<O: Output>(self, ctx: InitActionContext<O>, out: &O) {
                match self {
                $(
                    DispatchRequest::$request_type(req) => {
                        let Request { id, params, received, .. } = req;
                        let timeout = $request_type::timeout();

                        let receiver = work_pool::receive_from_thread(move || {
                            // Checking timeout here can prevent starting expensive work that has
                            // already timed out due to previous long running requests.
                            // Note: done here on the threadpool as pool scheduling may incur
                            // a further delay.
                            if received.elapsed() >= timeout {
                                $request_type::fallback_response()
                            }
                            else {
                                $request_type::handle(ctx, params)
                            }
                        }, WorkDescription($request_type::METHOD));

                        match receiver.recv_timeout(timeout)
                            .unwrap_or_else(|_| $request_type::fallback_response()) {
                            Ok(response) => response.send(id, out),
                            Err(ResponseError::Empty) => {
                                out.custom_failure(id, InternalError, Some("Empty response"))
                            },
                            Err(ResponseError::Message(code, msg)) => {
                                out.failure_message(id, code, msg)
                            }
                        }
                    }
                )*
                }
            }
        }
    }
}

define_dispatch_request_enum!(
    Completion,
    GotoDefinition,
    References,
    DocumentSymbolRequest,
    WorkspaceSymbolRequest,
    HoverRequest,
    GotoImplementation,
    GotoDeclaration,
    DocumentHighlightRequest,
    Rename,
    CodeActionRequest,
    ResolveCompletion,
    Formatting,
    RangeFormatting,
    ExecuteCommand,
    CodeLensRequest,
    GetKnownContextsRequest,
);

/// Provides ability to dispatch requests to a worker thread that will
/// handle the requests sequentially, without blocking stdin.
/// Requests dispatched this way are automatically timed out & avoid
/// processing if have already timed out before starting.
pub(crate) struct Dispatcher<O: Output> {
    sender: mpsc::Sender<(DispatchRequest, InitActionContext<O>, JobToken)>,
}

impl <O: Output> Dispatcher<O> {
    /// Creates a new `Dispatcher` starting a new thread and channel.
    pub(crate) fn new(out: O) -> Self {
        let (sender, receiver) = mpsc::channel::<(DispatchRequest, InitActionContext<O>, JobToken)>();

        thread::Builder::new()
            .name("dispatch-worker".into())
            .spawn(move || {
                while let Ok((request, ctx, token)) = receiver.recv() {
                    request.handle(ctx, &out);
                    drop(token);
                }
            })
            .unwrap();

        Self { sender }
    }

    /// Sends a request to the dispatch-worker thread; does not block.
    pub(crate) fn dispatch<R: Into<DispatchRequest>>(
        &mut self,
        request: R,
        ctx: InitActionContext<O>,
    ) {
        let (job, token) = ConcurrentJob::new();
        let req = request.into();
        ctx.add_job(req.get_identifier(), job);
        if let Err(err) = self.sender.send((req, ctx, token)) {
            debug!("failed to dispatch request: {:?}", err);
        }
    }
}

/// Stdin-non-blocking request logic designed to be packed into a `DispatchRequest`
/// and handled on the `WORK_POOL` via a `Dispatcher`.
pub trait RequestAction: LSPRequest {
    /// Serializable response type.
    type Response: server::Response + Send;

    /// Max duration this request should finish within; also see `fallback_response()`.
    fn timeout() -> Duration {
        DEFAULT_REQUEST_TIMEOUT
    }

    fn request_identifier(params: &str) -> String {
        format!("request-{}-{}", Self::METHOD, params)
    }

    // Get an identifier that mostly-describes this request and parameters
    fn get_identifier(params: &Self::Params) -> String;

    /// Returns a response used in timeout scenarios.
    fn fallback_response() -> Result<Self::Response, ResponseError>;

    /// Request processing logic.
    fn handle<O: Output>(
        ctx: InitActionContext<O>,
        params: Self::Params,
    ) -> Result<Self::Response, ResponseError>;
}

pub type HandleResponseType<O> = fn (&InitActionContext<O>,
                                     RawResponse,
                                     &O);
// Request sent from server to client
pub trait SentRequest: LSPRequest {
    /// Expected deserializable response type
    type Response: for <'a> serde::Deserialize<'a>;

    /// Max duration this request should finish within
    fn timeout() -> Duration {
        DEFAULT_REQUEST_TIMEOUT
    }

    fn handle_response<O: Output>
        (ctx: &InitActionContext<O>, response: RawResponse, out: &O)
    {
        match response.result {
            Ok(val) => {
                match serde_json::from_value(val) {
                    Ok(response_value) => Self::on_response(
                        ctx, response_value, out),
                    Err(response_err) =>
                        error!("Unexpected response value to request: {:?} \
                                (wanted {})",
                               response_err,
                               std::any::type_name::<Self::Response>()),
                }
            },
            Err(error) => {
                info!("Received an error to request on client: {:?}", error);
            }
        }
    }
    fn on_response<O: Output>
        (ctx: &InitActionContext<O>, response: Self::Response, out: &O);
}
