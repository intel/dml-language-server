//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
//! Traits and structs for message handling.

use std::fmt;
use std::marker::PhantomData;
use std::time::Instant;

use jsonrpc::error::{
    standard_error,
    StandardError::{self, InvalidParams, InvalidRequest, ParseError},
};
use log::debug;
use lsp_types::notification::ShowMessage;
use serde::ser::{SerializeStruct, Serializer};
use serde::Deserialize;
use serde::Serialize;
use serde_json::Value;

use crate::actions::{ActionContext, InitActionContext};
use crate::lsp_data::{LSPNotification, LSPRequest, MessageType, ShowMessageParams};
use crate::server::io::Output;


/// A response that just acknowledges receipt of its request.
#[derive(Debug, Serialize)]
pub struct Ack;

/// The lack of a response to a request.
#[derive(Debug)]
pub struct NoResponse;

/// A response to some request.
pub trait Response {
    /// Sends the response along the given output.
    fn send<O: Output>(self, id: RequestId, out: &O);
}

impl Response for NoResponse {
    fn send<O: Output>(self, _id: RequestId, _out: &O) {}
}

impl<R: ::serde::Serialize + fmt::Debug> Response for R {
    fn send<O: Output>(self, id: RequestId, out: &O) {
        out.success(id, &self);
    }
}

/// Wrapper for a response error.
#[derive(Debug)]
pub enum ResponseError {
    /// Error with no special response to the client.
    Empty,
    /// Error with a response to the client.
    Message(Value, String),
}

impl From<()> for ResponseError {
    fn from(_: ()) -> Self {
        ResponseError::Empty
    }
}

/// Some actions can succeed in LSP terms, but succeed only partially in
/// user-space.
/// This response allows an action to send a message to the user
///  or a proper response.
#[derive(Debug)]
pub enum ResponseWithMessage<R: Response> {
    Response(R),
    Warn(R, String),
    Error(R, String),
}

impl<R> Response for ResponseWithMessage<R>
where R: Response + std::fmt::Debug + serde::Serialize {
    fn send<O: Output>(self, id: RequestId, out: &O) {
        match self {
            ResponseWithMessage::Response(r) => out.success(id, &r),
            ResponseWithMessage::Warn(r, s) => {
                out.notify(Notification::<ShowMessage>::new(ShowMessageParams {
                    typ: MessageType::WARNING,
                    message: s,
                }));
                r.send(id, out);
            },
            ResponseWithMessage::Error(r, s) => {
                out.notify(Notification::<ShowMessage>::new(ShowMessageParams {
                    typ: MessageType::ERROR,
                    message: s,
                }));
                r.send(id, out);
            }
        }
    }
}

impl<R> From<R> for ResponseWithMessage<R>
where R: Response + std::fmt::Debug + serde::Serialize {
    fn from(value: R) -> ResponseWithMessage<R> {
        ResponseWithMessage::Response(value)
    }
}

/// An action taken in response to some notification from the client.
/// Blocks stdin whilst being handled.
pub trait BlockingNotificationAction: LSPNotification {
    /// Handles this notification.
    fn handle<O: Output>(_: Self::Params, _: &mut InitActionContext<O>, _: O)
                         -> Result<(), ResponseError>;
}

/// A request that blocks stdin whilst being handled.
pub trait BlockingRequestAction: LSPRequest {
    type Response: Response + fmt::Debug;

    /// Handles request and returns its response. Output is also provided for additional messaging.
    fn handle<O: Output>(
        id: RequestId,
        params: Self::Params,
        ctx: &mut ActionContext<O>,
        out: O,
    ) -> Result<Self::Response, ResponseError>;
}

/// A request ID as defined by language server protocol.
///
/// It only describes valid request IDs -- a case for notification (where `id` is not specified) is
/// not included here.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum RequestId {
    Str(String),
    Num(u64),
    Null,
}

impl fmt::Display for RequestId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RequestId::Str(ref s) => write!(f, "\"{}\"", s),
            RequestId::Num(n) => write!(f, "{}", n),
            RequestId::Null => write!(f, "-"),
        }
    }
}

impl From<RequestId> for Value {
    fn from(value: RequestId) -> Self {
        match value {
            RequestId::Str(s) => Value::from(s),
            RequestId::Num(n) => Value::from(n),
            RequestId::Null => Value::Null,
        }
    }
}

impl From<&RequestId> for Value {
    fn from(value: &RequestId) -> Self {
        match value {
            RequestId::Str(s) => Value::from(s.to_owned()),
            RequestId::Num(n) => Value::from(*n),
            RequestId::Null => Value::Null,
        }
    }
}

impl From<Value> for RequestId {
    fn from(value: Value) -> Self {
        match value {
            Value::String(s) => RequestId::Str(s),
            Value::Number(n) if n.is_u64() =>
                RequestId::Num(n.as_u64().unwrap()),
            _ => RequestId::Null,
        }
    }
}

impl From<String> for RequestId {
    fn from(value: String) -> Self {
        RequestId::Str(value)
    }
}

/// A request that gets JSON serialized in the language server protocol.
pub struct Request<A: LSPRequest> {
    /// The unique request ID.
    pub id: RequestId,
    /// The time the request was received / processed by the main stdin reading thread.
    pub received: Instant,
    /// The extra action-specific parameters.
    pub params: A::Params,
    /// This request's handler action.
    pub action: PhantomData<A>,
}

impl<A: LSPRequest> Request<A> {
    /// Creates a server `Request` structure with given `params`.
    pub fn new(id: RequestId, params: A::Params) -> Request<A> {
        Request { id, received: Instant::now(), params, action: PhantomData }
    }
}

/// A notification that gets JSON serialized in the language server protocol.
#[derive(Debug, PartialEq)]
pub struct Notification<A: LSPNotification> {
    /// The extra action-specific parameters.
    pub params: A::Params,
    /// The action responsible for this notification.
    pub _action: PhantomData<A>,
}

impl<A: LSPNotification> Notification<A> {
    /// Creates a `Notification` structure with given `params`.
    pub fn new(params: A::Params) -> Notification<A> {
        Notification { params, _action: PhantomData }
    }
}

impl<'a, A> From<&'a Request<A>> for RawMessage
where
    A: LSPRequest,
    <A as LSPRequest>::Params: serde::Serialize,
{
    fn from(request: &Request<A>) -> RawMessage {
        let method = <A as LSPRequest>::METHOD.to_owned();

        let params = match serde_json::to_value(&request.params).unwrap() {
            params @ serde_json::Value::Array(_) |
            params @ serde_json::Value::Object(_) |
            // We represent missing params internally by Null.
            params @ serde_json::Value::Null => params,
            _ => unreachable!("Bad parameter type found for {:?} request", method),
        };

        RawMessage { method, id: Value::from(&request.id), params }
    }
}

impl<'a, A> From<&'a Notification<A>> for RawMessage
where
    A: LSPNotification,
    <A as LSPNotification>::Params: serde::Serialize,
{
    fn from(notification: &Notification<A>) -> RawMessage {
        let method = <A as LSPNotification>::METHOD.to_owned();

        let params = match serde_json::to_value(&notification.params).unwrap() {
            params @ serde_json::Value::Array(_) |
            params @ serde_json::Value::Object(_) |
            // We represent missing params internally by Null.
            params @ serde_json::Value::Null => params,
            _ => unreachable!("Bad parameter type found for {:?} request", method),
        };

        RawMessage { method, id: Value::Null, params }
    }
}

impl<A: BlockingRequestAction> Request<A> {
    pub fn blocking_dispatch<O: Output>(
        self,
        ctx: &mut ActionContext<O>,
        out: &O,
    ) -> Result<A::Response, ResponseError> {
        A::handle(self.id, self.params, ctx, out.clone())
    }
}

impl<A: BlockingNotificationAction> Notification<A> {
    pub fn dispatch<O: Output>(self, ctx: &mut InitActionContext<O>, out: O)
                               -> Result<(), ResponseError> {
        A::handle(self.params, ctx, out)?;
        Ok(())
    }
}

impl<A> fmt::Display for Request<A>
where
    A: LSPRequest,
    <A as LSPRequest>::Params: serde::Serialize,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let raw: RawMessage = self.into();
        match serde_json::to_string(&raw) {
            Ok(val) => val.fmt(f),
            Err(_) => Err(fmt::Error),
        }
    }
}

impl<A> fmt::Display for Notification<A>
where
    A: LSPNotification,
    <A as LSPNotification>::Params: serde::Serialize,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let raw: RawMessage = self.into();
        match serde_json::to_string(&raw) {
            Ok(val) => val.fmt(f),
            Err(_) => Err(fmt::Error),
        }
    }
}

fn rpc_error<M: ToString>(code: StandardError, msg: M) -> jsonrpc::Error {
    let mut err = standard_error(code, None);
    err.message = msg.to_string();
    jsonrpc::Error::from(err)
}

#[derive(Debug, PartialEq)]
pub struct RawMessage {
    pub method: String,
    pub id: Value,
    pub params: Value,
}

impl RawMessage {
    pub(crate) fn parse_as_request<'de, R>(&'de self) -> Result<Request<R>, jsonrpc::Error>
    where
        R: LSPRequest,
        <R as LSPRequest>::Params: serde::Deserialize<'de>,
    {
        let params = R::Params::deserialize(&self.params)
            .or_else(|e| {
                // Avoid tedious type errors trying to deserialize `()`.
                if std::mem::size_of::<R::Params>() == 0 {
                    R::Params::deserialize(&Value::Null).map_err(|_| e)
                } else {
                    Err(e)
                }
            })
            .map_err(|e| {
                debug!("error when parsing as request: {}", e);
                rpc_error(InvalidParams, e.to_string())
            })?;

            let parsed_id = match &self.id {
                Value::Number(n) if n.is_u64() =>
                    Some(RequestId::Num(n.as_u64().unwrap())),
                Value::String(ref s) => Some(RequestId::Str(s.to_string())),
                _ => None,
            };

        match parsed_id {
            Some(id) => Ok(Request { id, params, received: Instant::now(), action: PhantomData }),
            None => Err(rpc_error(InvalidRequest, "Failed to parse Id")),
        }
    }

    pub(crate) fn parse_as_notification<'de, T>(
        &'de self,
    ) -> Result<Notification<T>, jsonrpc::Error>
    where
        T: LSPNotification,
        <T as LSPNotification>::Params: serde::Deserialize<'de>,
    {
        let params = T::Params::deserialize(&self.params).map_err(|e| {
            debug!("error when parsing as notification: {}", e);
            rpc_error(InvalidParams, e)
        })?;

        Ok(Notification { params, _action: PhantomData })
    }

    pub(crate) fn try_parse(ls_command: serde_json::Value)
    -> Result<RawMessage, jsonrpc::Error> {
        // Per JSON-RPC/LSP spec, Requests must have ID, whereas Notifications cannot.
        let id = ls_command
            .get("id")
            .map_or(Value::Null, |id| serde_json::from_value(id.to_owned()).unwrap());

        // Guaranteed by caller
        let method = ls_command.get("method").unwrap();

        let method = method.as_str().ok_or_else(||
            rpc_error(InvalidRequest, "Method is not a string"))?
            .to_owned();

        // Representing a missing parameter as Null internally instead of `None`,
        // (Null being unused value of param by the JSON-RPC 2.0 spec)
        // in order to unify the type handling –- now the parameter type implements
        // `Deserialize`.
        let params = match ls_command.get("params").map(ToOwned::to_owned) {
            Some(params @ serde_json::Value::Object(..))
            | Some(params @ serde_json::Value::Array(..)) => params,
            // Null as input value is not allowed by JSON-RPC 2.0,
            // but including it for robustness.
            Some(serde_json::Value::Null) | None => serde_json::Value::Null,
            Some(v) => return Err(rpc_error(InvalidRequest,
                format!("Unsupported parameter type: {v}"))),
        };

        Ok(RawMessage { method, id, params })
    }
}

// Added so we can prepend with extra constant `"jsonrpc": "2.0"` key.
// Should be resolved once <https://github.com/serde-rs/serde/issues/760> is fixed.
impl Serialize for RawMessage {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let serialize_id = !matches!(self.id, Value::Null);
        let serialize_params = self.params.is_array() || self.params.is_object();

        let len = 2 + if serialize_id { 1 } else { 0 } + if serialize_params { 1 } else { 0 };
        let mut msg = serializer.serialize_struct("RawMessage", len)?;
        msg.serialize_field("jsonrpc", "2.0")?;
        msg.serialize_field("method", &self.method)?;
        // Notifications don't have Id specified
        if serialize_id {
            msg.serialize_field("id", &self.id)?;
        }
        if serialize_params {
            msg.serialize_field("params", &self.params)?;
        }
        msg.end()
    }
}

#[derive(Debug, PartialEq, Deserialize)]
pub struct ErrorResponse {
    code: i32,
    message: String,
    data: Option<Value>,
}

pub type ResultOrError = Result<Value, ErrorResponse>;

#[derive(Debug, PartialEq)]
pub struct RawResponse {
    pub id: RequestId,
    pub result: ResultOrError,
}

impl RawResponse {
    pub(crate) fn try_parse(mut ls_command: serde_json::Value)
                 -> Result<RawResponse, jsonrpc::Error> {
        // Get the ID (required in responses)
        let id = ls_command.get("id")
            .ok_or_else(||rpc_error(InvalidRequest, "No ID in response"))?
            .to_owned();

        let parsed_id = match id {
            Value::Number(n) if n.is_u64() =>
                Some(RequestId::Num(n.as_u64().unwrap())),
            Value::String(ref s) => Some(RequestId::Str(s.to_string())),
            _ => None,
        };

        let parsed_id = match parsed_id {
            Some(id) => id,
            None => return Err(rpc_error(InvalidRequest, "Failed to parse Id")),
        };

        let result = ls_command.get_mut("result").map(
            |r|r.take());
        let error = ls_command.get_mut("error").map(
            |val|ErrorResponse::deserialize(val.take()));
        let resultorerror = match (result, error) {
            (Some(_), Some(_)) =>
                return Err(rpc_error(InvalidRequest,
                                     "Both 'result' and 'error' \
                                      specified in response")),
            (None, None) =>
                return Err(rpc_error(InvalidRequest,
                                     "Neither 'result' and 'error' \
                                      specified in response")),
            (Some(result), None) =>
                Ok(result),
            (None, Some(error)) =>
                Err(error.map_err(|e|rpc_error(ParseError, e))?),
        };

        Ok(RawResponse { id: parsed_id, result: resultorerror })
    }
}

pub enum RawMessageOrResponse {
    Message(RawMessage),
    Response(RawResponse),
}

impl RawMessageOrResponse {
    pub(crate) fn try_parse(msg: &str) ->
        Result<RawMessageOrResponse, jsonrpc::Error> {
            let ls_command: serde_json::Value =
                serde_json::from_str(msg)
                .map_err(|e| rpc_error(ParseError, e))?;
            if ls_command.get("method").is_some() {
                RawMessage::try_parse(ls_command)
                    .map(|raw|RawMessageOrResponse::Message(raw))
            } else {
                RawResponse::try_parse(ls_command)
                    .map(|raw|RawMessageOrResponse::Response(raw))
            }
        }

    pub fn is_message(&self) -> bool {
        matches!(self, RawMessageOrResponse::Message(_))
    }

    pub fn is_response(&self) -> bool {
        matches!(self, RawMessageOrResponse::Response(_))
    }

    #[allow(dead_code)]
    pub(crate) fn as_message(&self) -> Option<&RawMessage> {
        match self {
            RawMessageOrResponse::Message(mess) => Some(mess),
            _ => None,
        }
    }
    #[allow(dead_code)]
    pub(crate) fn as_response(&self) -> Option<&RawResponse> {
        match self {
            RawMessageOrResponse::Response(resp) => Some(resp),
            _ => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::server::notifications;
    use lsp_types::InitializedParams;
    use serde_json::json;

    #[test]
    fn test_parse_as_notification() {
        let raw = RawMessage {
            method: "initialize".to_owned(),
            id: Value::Null,
            params: serde_json::Value::Object(serde_json::Map::new()),
        };
        let notification: Notification<notifications::Initialized> =
            raw.parse_as_notification().unwrap();

        let expected = Notification::<notifications::Initialized>::new(InitializedParams {});

        assert_eq!(notification.params, expected.params);
        assert_eq!(notification._action, expected._action);
    }

    // See <http://www.jsonrpc.org/specification#request_object>.
    #[test]
    fn raw_message_parses_valid_jsonrpc_request_with_string_id() {
        let raw_json = json!({
            "jsonrpc": "2.0",
            "id": "abc",
            "method": "someRpcCall",
        })
        .to_string();

        let expected_msg = RawMessage {
            method: "someRpcCall".to_owned(),
            id: Value::from("abc"),
            // Missing parameters are represented internally as Null.
            params: serde_json::Value::Null,
        };

        let raw = RawMessageOrResponse::try_parse(&raw_json).unwrap();
        let parsed = raw.as_message().unwrap();
        assert_eq!(&expected_msg, parsed);
    }

    #[test]
    fn raw_message_parses_valid_jsonrpc_request_with_numeric_id() {
        let raw_json = json!({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "someRpcCall",
        })
        .to_string();

        let expected_msg = RawMessage {
            method: "someRpcCall".to_owned(),
            id: Value::from(1),
            // Missing parameters are represented internally as Null.
            params: serde_json::Value::Null,
        };
        let raw = RawMessageOrResponse::try_parse(&raw_json).unwrap();
        let parsed = raw.as_message().unwrap();
        assert_eq!(&expected_msg, parsed);
    }

    #[test]
    fn raw_message_with_string_id_parses_into_request() {
        #[derive(Debug)]
        enum DummyRequest {}
        impl LSPRequest for DummyRequest {
            type Params = ();
            type Result = ();
            const METHOD: &'static str = "dummyRequest";
        }

        let raw_msg = RawMessage {
            method: "dummyRequest".to_owned(),
            id: Value::from("abc"),
            params: serde_json::Value::Null,
        };

        let request: Request<DummyRequest> = raw_msg
            .parse_as_request()
            .expect("RawMessage with string id should parse into request");
        assert_eq!(RequestId::Str("abc".to_owned()), request.id)
    }

    #[test]
    fn serialize_message_no_params() {
        #[derive(Debug)]
        enum DummyNotification {}

        impl LSPNotification for DummyNotification {
            type Params = ();
            const METHOD: &'static str = "dummyNotification";
        }

        let notif = Notification::<DummyNotification>::new(());
        let raw = format!("{}", notif);
        let deser: serde_json::Value = serde_json::from_str(&raw).unwrap();

        assert!(match deser.get("params") {
            Some(serde_json::Value::Array(arr)) if arr.is_empty() => true,
            Some(serde_json::Value::Object(map)) if map.is_empty() => true,
            None => true,
            _ => false,
        });
    }

    #[test]
    fn serialize_message_empty_params() {
        #[derive(Debug)]
        enum DummyNotification {}
        #[derive(Serialize, Deserialize)]
        struct EmptyParams {}

        impl LSPNotification for DummyNotification {
            type Params = EmptyParams;
            const METHOD: &'static str = "dummyNotification";
        }

        let notif = Notification::<DummyNotification>::new(EmptyParams {});
        let raw = format!("{}", notif);
        let deser: serde_json::Value = serde_json::from_str(&raw).unwrap();

        assert_eq!(*deser.get("params").unwrap(), json!({}));
    }

    #[test]
    fn deserialize_message_empty_params() {
        let msg = r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#;
        let raw = RawMessageOrResponse::try_parse(msg).unwrap();
        let parsed = raw.as_message().unwrap();
        parsed.parse_as_notification::<notifications::Initialized>().unwrap();
    }

    #[test]
    fn simple_request_response() {
        let raw_json = json!({
            "jsonrpc": "2.0",
            "id": "abc",
            "result": {
                "this" : 0,
                "really" : "could",
                "be" : ["anything"]
            }
        })
        .to_string();

        let expected_resp = RawResponse {
            id: RequestId::from("abc".to_string()),
            result: ResultOrError::Ok(json!({
                "this" : 0,
                "really" : "could",
                "be" : ["anything"]
            }))
        };

        let raw = RawMessageOrResponse::try_parse(&raw_json).unwrap();
        let parsed = raw.as_response().unwrap();
        assert_eq!(&expected_resp, parsed);
    }
    #[test]
    fn simple_request_error() {
        let raw_json = json!({
            "jsonrpc": "2.0",
            "id": "abc",
            "error": {
                "code" : 0,
                "message" : "oh no!",
            }
        })
        .to_string();

        let expected_resp = RawResponse {
            id: RequestId::from("abc".to_string()),
            result: ResultOrError::Err(
                ErrorResponse {
                    code: 0,
                    message: "oh no!".to_string(),
                    data: None,
                }
            ),
        };

        let raw = RawMessageOrResponse::try_parse(&raw_json).unwrap();
        let parsed = raw.as_response().unwrap();
        assert_eq!(&expected_resp, parsed);
    }
}
