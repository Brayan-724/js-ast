use crate::abstractor::{JsNode, JsNodeExpression, JsNodeLabel};
use crate::runtime::{JsNodeProcessable, JsRuntime, JsValue};
use std::sync::{Arc, Mutex};

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

impl JsNodeProcessable for JsNodeStatement {
    fn process(&self, context: &JsRuntime) -> Arc<Mutex<JsValue>> {
        match self {
            JsNodeStatement::If {
                condition,
                inner,
                otherwise,
            } => {
                let condition = context.process_node_expression(condition);
                if condition.lock().unwrap().is_truthy() {
                    context.process_node(inner);
                } else if otherwise.is_some() {
                    context.process_node(&Option::as_ref(otherwise).unwrap());
                }
            }
            JsNodeStatement::While { condition, inner } => loop {
                let condition = context.process_node_expression(condition);
                if condition.lock().unwrap().is_truthy() {
                    context.process_node(inner);
                } else {
                    break;
                }
            },

            JsNodeStatement::Break { .. } => (),
        }
        Arc::new(Mutex::new(JsValue::Undefined))
    }
}
