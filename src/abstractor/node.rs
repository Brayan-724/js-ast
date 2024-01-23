use crate::{
    abstractor::{JsNodeExpression, JsNodeMeta, JsNodeStatement, JsToken},
    JsNodeProcessable,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct JsNodeLabel(Box<str>);

#[derive(Debug, Clone)]
pub enum JsNode {
    Program {
        child: Box<[JsNode]>,
        meta: JsNodeMeta,
    },
    Block {
        label: Option<JsNodeLabel>,
        children: Box<[JsNode]>,
        meta: JsNodeMeta,
    },
    VariableDeclaration {
        key: JsToken,
        value: Box<JsNode>,
        meta: JsNodeMeta,
    },
    Expression {
        expression: JsNodeExpression,
        meta: JsNodeMeta,
    },
    Statement {
        statement: JsNodeStatement,
        meta: JsNodeMeta,
    },
}

impl JsNode {
    pub fn get_meta(&self) -> &JsNodeMeta {
        match self {
            JsNode::Program { meta, .. } => meta,
            JsNode::Block { meta, .. } => meta,
            JsNode::VariableDeclaration { meta, .. } => meta,
            JsNode::Expression { meta, .. } => meta,
            JsNode::Statement { meta, .. } => meta,
        }
    }
}

impl JsNodeProcessable for JsNode {}
