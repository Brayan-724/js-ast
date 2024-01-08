use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use crate::{
    JsNode, JsNodeExpression, JsNodeExpressionBinary, JsNodeStatement, JsToken, JsTokenLiteral,
};

type Fp = Arc<Box<dyn Fn(Vec<Arc<Mutex<JsValue>>>) -> Arc<Mutex<JsValue>> + Send + Sync>>;

#[derive(Clone)]
pub enum JsValueFunction {
    Binding(Fp),
    // UserDefined
}

impl JsValueFunction {
    pub fn as_binding(self) -> Option<Fp> {
        match self {
            Self::Binding(f) => Some(f),
            _ => None,
        }
    }
}

impl std::fmt::Debug for JsValueFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<native code>")
    }
}

#[derive(Debug, Clone)]
pub enum JsValue {
    Undefined,
    Number(f64),
    String(String),
    Boolean(bool),
    Object(HashMap<Box<str>, Arc<Mutex<JsValue>>>),
    Function(JsValueFunction),
}

impl From<&JsTokenLiteral> for JsValue {
    fn from(value: &JsTokenLiteral) -> Self {
        match value {
            JsTokenLiteral::Boolean(b) => JsValue::Boolean(*b),
            JsTokenLiteral::Number(n) => JsValue::Number(*n),
            JsTokenLiteral::String(s) => JsValue::String(s.clone()),
        }
    }
}

impl JsValue {
    pub fn is_truthy(&self) -> bool {
        match self {
            JsValue::Undefined => false,
            JsValue::Boolean(b) => *b,
            JsValue::Number(n) => *n != 0.0,
            JsValue::String(s) => !s.is_empty(),
            JsValue::Object(o) => !o.is_empty(),
            JsValue::Function(..) => true,
        }
    }

    pub fn as_number(&self) -> Option<&f64> {
        match self {
            JsValue::Number(s) => Some(s),
            _ => None,
        }
    }
    pub fn as_string(&self) -> Option<&String> {
        match self {
            JsValue::String(s) => Some(s),
            _ => None,
        }
    }
    pub fn as_object(&self) -> Option<&HashMap<Box<str>, Arc<Mutex<JsValue>>>> {
        match self {
            JsValue::Object(o) => Some(o),
            _ => None,
        }
    }
    pub fn as_function(&self) -> Option<&JsValueFunction> {
        match self {
            JsValue::Function(f) => Some(f),
            _ => None,
        }
    }
}

impl std::fmt::Display for JsValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JsValue::Undefined => f.write_str("undefined"),
            JsValue::Boolean(true) => f.write_str("true"),
            JsValue::Boolean(false) => f.write_str("false"),
            JsValue::Number(n) => f.write_fmt(format_args!("{n}")),
            JsValue::String(s) => f.write_str(s),
            JsValue::Object(..) => f.write_str("[object Object]"),
            JsValue::Function(..) => f.write_str("function () { [native code] }"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct JsContext {
    variables: HashMap<Box<str>, Arc<Mutex<JsValue>>>,
}

impl JsContext {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct JsRuntime {
    ast: JsNode,
    lines: Box<[JsNode]>,
    pointer: usize,
    context: Arc<Mutex<JsContext>>,
}

impl JsRuntime {
    pub fn new(ast: JsNode) -> JsRuntime {
        let lines = match &ast {
            JsNode::Program { child, .. } => child.clone(),
            _ => panic!("The runtime should have JsNode::Program as root"),
        };

        JsRuntime {
            ast,
            lines,
            pointer: 0,
            context: Arc::new(Mutex::new(JsContext::new())),
        }
    }

    pub fn assign_variable(&self, key: Box<str>, value: Arc<Mutex<JsValue>>) {
        self.context.lock().unwrap().variables.insert(key, value);
    }

    pub fn binary_op(kind: &JsNodeExpressionBinary, left: &JsValue, right: &JsValue) -> JsValue {
        use JsNodeExpressionBinary::*;
        use JsValue::*;

        match kind {
            Add => match (left, right) {
                (Undefined, Undefined) => Undefined,
                (Number(l), Number(r)) => Number(l + r),
                _ => todo!(),
            },
            Divide => match (left, right) {
                (Undefined, Undefined) => Undefined,
                (Number(l), Number(r)) => Number(l / r),
                _ => todo!(),
            },
            Mult => match (left, right) {
                (Undefined, Undefined) => Undefined,
                (Number(l), Number(r)) => Number(l * r),
                _ => todo!(),
            },
            Sub => match (left, right) {
                (Undefined, Undefined) => Undefined,
                (Number(l), Number(r)) => Number(l - r),
                _ => todo!(),
            },

            LessThan => match (left, right) {
                (Undefined, Undefined) => Boolean(false),
                (Number(l), Number(r)) => Boolean(l < r),
                _ => todo!(),
            },

            NotStrictlyEqual => match (left, right) {
                (Undefined, Undefined) => Boolean(true),
                (Undefined, _) => Boolean(false),
                (_, Undefined) => Boolean(false),
                (Number(l), Number(r)) => Boolean(l != r),
                (String(l), String(r)) => Boolean(l != r),
                (l, r) => todo!("{l:?} !== {r:?}"),
            },
        }
    }

    pub fn process_node_expression(&self, expression: &JsNodeExpression) -> Arc<Mutex<JsValue>> {
        match expression {
            JsNodeExpression::Raw { .. } => unreachable!(),
            JsNodeExpression::BinaryOp { kind, left, right } => {
                let left = self.process_node_expression(left);
                let right = self.process_node_expression(right);

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

                self.context
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

    pub fn process_node(&self, node: &JsNode) -> Arc<Mutex<JsValue>> {
        match node {
            JsNode::VariableDeclaration { key, value, .. } => {
                let key = match key {
                    JsToken::Variable(k, ..) => k.clone(),
                    _ => unreachable!(),
                };

                if self.context.lock().unwrap().variables.contains_key(&key) {
                    panic!("Variable already declared");
                }

                let value = self.process_node(value);

                self.context
                    .lock()
                    .unwrap()
                    .variables
                    .insert(key, value.clone());

                Arc::new(Mutex::new(JsValue::Undefined))
            }
            JsNode::Expression { expression, .. } => self.process_node_expression(expression),
            JsNode::Block { children, .. } => {
                for node in children.iter() {
                    self.process_node(node);
                }

                Arc::new(Mutex::new(JsValue::Undefined))
            }
            JsNode::Program { .. } => unreachable!("Program inside a program"),
            JsNode::Statement { statement, .. } => match statement {
                JsNodeStatement::If {
                    condition,
                    inner,
                    otherwise,
                } => {
                    let condition = self.process_node_expression(condition);
                    if condition.lock().unwrap().is_truthy() {
                        self.process_node(inner);
                    } else if otherwise.is_some() {
                        self.process_node(&Option::as_ref(otherwise).unwrap());
                    }

                    Arc::new(Mutex::new(JsValue::Undefined))
                }
                JsNodeStatement::While { condition, inner } => {
                    loop {
                        let condition = self.process_node_expression(condition);
                        if condition.lock().unwrap().is_truthy() {
                            self.process_node(inner);
                        } else {
                            break;
                        }
                    }

                    Arc::new(Mutex::new(JsValue::Undefined))
                }
            },
        }
    }

    pub fn next_iterate(&mut self) -> Option<()> {
        if self.pointer >= self.lines.len() {
            return None;
        }

        let line = self.lines[self.pointer].clone();
        self.process_node(&line);

        self.pointer += 1;

        Some(())
    }
}
