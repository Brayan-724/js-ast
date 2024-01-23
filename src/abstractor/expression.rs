use crate::{
    abstractor::{JsToken, JsTokenLiteral},
    JsNodeProcessable, JsRuntime, JsValue,
};
use std::sync::{Mutex, Arc};

#[derive(Debug, Clone)]
pub enum JsNodeExpressionBinary {
    Add,
    Divide,
    Mult,
    Sub,

    // Conditionals
    NotStrictlyEqual,
    LessThan,
}

#[derive(Debug, Clone)]
pub enum JsNodeExpression {
    Raw {
        children: Box<[JsToken]>,
    },
    FunctionCall {
        call: Box<JsNodeExpression>,
        arguments: Box<[JsNodeExpression]>,
    },
    Assigment {
        variable: JsToken,
        value: Box<JsNodeExpression>,
    },
    BinaryOp {
        kind: JsNodeExpressionBinary,
        left: Box<JsNodeExpression>,
        right: Box<JsNodeExpression>,
    },
    Variable {
        name: JsToken,
    },
    Member {
        from: Box<JsNodeExpression>,
        member: Box<JsNodeExpression>,
    },
    Literal {
        value: JsTokenLiteral,
    },
}

impl JsNodeProcessable for JsNodeExpression {
    fn process(&self, context: &JsRuntime) -> Arc<Mutex<JsValue>> {
        match self {
            JsNodeExpression::Raw { .. } => unreachable!(),
            JsNodeExpression::BinaryOp { kind, left, right } => {
                let left = left.process(context);
                let right = right.process(context);

                let left = left.lock().unwrap();
                let right = right.lock().unwrap();

                Arc::new(Mutex::new(JsRuntime::binary_op(kind, &left, &right)))
            }
            JsNodeExpression::Literal { value } => Arc::new(Mutex::new(JsValue::from(value))),
            JsNodeExpression::Variable { name } => {
                let key = match name {
                    JsToken::Variable(k, ..) => k.clone(),
                    _ => unreachable!(),
                };

                context.context
                    .lock()
                    .unwrap()
                    .variables
                    .get(&key)
                    .cloned()
                    .unwrap_or(Arc::new(Mutex::new(JsValue::Undefined)))
            }
            JsNodeExpression::Assigment { variable, value } => {
                let JsToken::Variable(name, ..) = variable else {
                    unreachable!();
                };

                let value = self.process_node_expression(value);

                self.context
                    .lock()
                    .unwrap()
                    .variables
                    .insert(name.clone(), value.clone());

                value
            }
            JsNodeExpression::Member { from, member } => {
                let from = self.process_node_expression(from);
                let from = from.lock().unwrap();

                let member = self.process_node_expression(member);

                if let Some(o) = from.as_object() {
                    let member = member.lock().unwrap();
                    o.get(&Box::from(
                        member
                            .as_string()
                            .expect("String should be used as object indexer")
                            .to_owned(),
                    ))
                    .cloned()
                    .unwrap_or(Arc::new(Mutex::new(JsValue::Undefined)))
                } else if let Some(s) = from.as_string() {
                    let member = member.lock().unwrap();
                    if let Some(member) = member.as_string() {
                        match member.as_str() {
                            "length" => Arc::new(Mutex::new(JsValue::Number(s.len() as f64))),
                            _ => Arc::new(Mutex::new(JsValue::Undefined)),
                        }
                    } else if let Some(n) = member.as_number() {
                        if n <= &0.0 {
                            Arc::new(Mutex::new(JsValue::Undefined))
                        } else {
                            let idx = n.abs().round() as usize;
                            let c = s
                                .chars()
                                .nth(idx)
                                .map(|c| JsValue::String(c.to_string()))
                                .unwrap_or(JsValue::Undefined);

                            Arc::new(Mutex::new(c))
                        }
                    } else {
                        Arc::new(Mutex::new(JsValue::Undefined))
                    }
                } else {
                    panic!("Cannot access to a member of a non-object variable: {from:#?}")
                }
            }
            JsNodeExpression::FunctionCall { call, arguments } => {
                let call = self.process_node_expression(call);
                let call = call.lock().unwrap();
                if let Some(f) = call.as_function() {
                    match f {
                        JsValueFunction::Binding(f) => {
                            let a = arguments
                                .iter()
                                .map(|arg| self.process_node_expression(arg))
                                .collect::<Vec<Arc<Mutex<JsValue>>>>();
                            f(a)
                        }
                    }
                } else {
                    panic!("Is not a function")
                }
            }
        }

    }
}
