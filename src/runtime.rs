use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use crate::{JsNode, JsNodeExpression, JsNodeExpressionBinary, JsToken, JsTokenLiteral};

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
    Object(HashMap<Box<str>, Arc<Mutex<JsValue>>>),
    Function(JsValueFunction),
}

impl From<&JsTokenLiteral> for JsValue {
    fn from(value: &JsTokenLiteral) -> Self {
        match value {
            JsTokenLiteral::Number(n) => JsValue::Number(*n),
            JsTokenLiteral::String(s) => JsValue::String(s.clone()),
        }
    }
}

impl JsValue {
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
        match kind {
            JsNodeExpressionBinary::Add => match (left, right) {
                (JsValue::Undefined, JsValue::Undefined) => JsValue::Undefined,
                (JsValue::Number(l), JsValue::Number(r)) => JsValue::Number(l + r),
                _ => todo!(),
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
            JsNodeExpression::Assigment { .. } => todo!(),
            JsNodeExpression::Member { from, member } => {
                let from = self.process_node_expression(from);
                let from = from.lock().unwrap();

                if let Some(o) = from.as_object() {
                    let member = self.process_node_expression(member);
                    let member = member.lock().unwrap();
                    o.get(&Box::from(
                        member
                            .as_string()
                            .expect("String should be used as object indexer")
                            .to_owned(),
                    ))
                    .cloned()
                    .unwrap_or(Arc::new(Mutex::new(JsValue::Undefined)))
                } else {
                    panic!("Cannot access to a member of a non-object variable")
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
            JsNode::Block { .. } => todo!(),
            JsNode::Program { .. } => unreachable!("Program inside a program"),
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
