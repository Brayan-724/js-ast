use crate::abstractor::{JsNode, JsNodeExpression, JsNodeLabel};

#[derive(Debug, Clone)]
pub enum JsNodeStatement {
    If {
        condition: Box<JsNodeExpression>,
        inner: Box<JsNode>,
        otherwise: Box<Option<JsNode>>,
    },
    While {
        condition: Box<JsNodeExpression>,
        inner: Box<JsNode>,
    },
    Break {
        label: Option<JsNodeLabel>,
    },
}
