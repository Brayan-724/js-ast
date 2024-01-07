// #[derive(Debug, Clone)]
// pub enum JsAstNode {
//     Block(Box<[JsNode]>),
//     VariableDeclaration(JsToken, JsNode)
// }

use crate::{JsToken, JsTokenKeyword, JsTokenLiteral, JsTokenPunctuation};

#[derive(Debug, Clone)]
pub struct JsNodeMeta {}

#[derive(Debug, Clone)]
pub enum JsNodeExpressionBinary {
    Add,
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
        variable: Box<JsNodeExpression>,
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

#[derive(Debug, Clone)]
pub enum JsNode {
    Program {
        child: Box<[JsNode]>,
        meta: JsNodeMeta,
    },
    Block {
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
}

impl JsNode {
    pub fn get_meta(&self) -> &JsNodeMeta {
        match self {
            JsNode::Program { meta, .. } => meta,
            JsNode::Block { meta, .. } => meta,
            JsNode::VariableDeclaration { meta, .. } => meta,
            JsNode::Expression { meta, .. } => meta,
        }
    }
}

#[derive(Debug, Clone)]
pub struct JsAbstractor {
    tokens: Box<[JsToken]>,
    pointer: usize,
}

#[derive(Debug, Clone)]
pub enum JsAbstractorError {
    None,
    ExpectedToken(&'static str),
    UnexpectedToken(&'static str),
}

impl std::fmt::Display for JsAbstractorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JsAbstractorError::None => Ok(()),
            JsAbstractorError::ExpectedToken(tk) => {
                f.write_str("[Abstractor] ExpectedToken \"")?;
                f.write_str(tk)?;
                f.write_str("\"")
            }
            JsAbstractorError::UnexpectedToken(tk) => {
                f.write_str("[Abstractor] UnexpectedToken \"")?;
                f.write_str(tk)?;
                f.write_str("\"")
            }
        }
    }
}

impl std::error::Error for JsAbstractorError {}

impl JsAbstractor {
    pub fn new_from_tokens(tokens: Box<[JsToken]>) -> Self {
        Self { tokens, pointer: 0 }
    }

    pub(crate) fn expr_end_paren_impl(token: &JsToken) -> bool {
        matches!(
            token,
            JsToken::Punctuation(JsTokenPunctuation::RightParen, _)
        )
    }

    pub fn expr<E>(&mut self, mut end: E) -> Result<JsNode, JsAbstractorError>
    where
        E: FnMut(&JsToken) -> bool,
    {
        let mut last: Option<JsNodeExpression> = None;
        while self.pointer < self.tokens.len() {
            let tk = &self.tokens[self.pointer];
            if end(tk) {
                break;
            }

            match tk {
                JsToken::Keyword(JsTokenKeyword::Const, ..) => {
                    return Err(JsAbstractorError::None);
                }
                tk @ JsToken::Variable(..) => {
                    last = Some(JsNodeExpression::Variable { name: tk.clone() });
                }
                JsToken::Punctuation(JsTokenPunctuation::Dot, _)
                    if matches!(
                        last,
                        Some(
                            JsNodeExpression::Variable { .. }
                                | JsNodeExpression::Member { .. }
                                | JsNodeExpression::FunctionCall { .. }
                        )
                    ) =>
                {
                    self.pointer += 1;
                    let tk = &self.tokens[self.pointer];
                    let tk = match tk {
                        JsToken::Variable(t, ..) => JsTokenLiteral::String(t.to_string()),
                        _ => {
                            return Err(JsAbstractorError::UnexpectedToken(
                                "Should have member name",
                            ))
                        }
                    };
                    let tk = JsNodeExpression::Literal { value: tk };
                    last = Some(JsNodeExpression::Member {
                        from: Box::from(last.unwrap()),
                        member: Box::from(tk),
                    })
                }
                JsToken::Punctuation(JsTokenPunctuation::Dot, _) => {
                    return Err(JsAbstractorError::UnexpectedToken("."))
                }
                JsToken::Punctuation(JsTokenPunctuation::LeftParen, _)
                    if matches!(
                        last,
                        Some(
                            JsNodeExpression::Variable { .. }
                                | JsNodeExpression::Member { .. }
                                | JsNodeExpression::FunctionCall { .. }
                        )
                    ) =>
                {
                    self.pointer += 1;
                    let tk = self.expr(JsAbstractor::expr_end_paren_impl);
                    match tk {
                        Ok(JsNode::Expression { expression, .. }) => {
                            last = Some(JsNodeExpression::FunctionCall {
                                call: Box::from(last.unwrap().clone()),
                                arguments: Box::from([expression]),
                            })
                        }
                        Err(JsAbstractorError::None) => {
                            last = Some(JsNodeExpression::FunctionCall {
                                call: Box::from(last.unwrap().clone()),
                                arguments: Box::from([]),
                            })
                        }
                        Ok(..) => return Err(JsAbstractorError::ExpectedToken("expression")),
                        Err(e) => return Err(e),
                    }
                }
                JsToken::Punctuation(JsTokenPunctuation::Plus, ..) if last.is_some() => {
                    self.pointer += 1;
                    let tk = &self.tokens[self.pointer];
                    let tk = JsNodeExpression::Variable { name: tk.clone() };
                    last = Some(JsNodeExpression::BinaryOp {
                        kind: JsNodeExpressionBinary::Add,
                        left: Box::from(last.unwrap()),
                        right: Box::from(tk),
                    })
                }
                JsToken::Literal(l, _) => {
                    last = Some(JsNodeExpression::Literal { value: l.clone() })
                }
                _ => (),
            }

            self.pointer += 1;
        }

        if let Some(last) = last {
            Ok(JsNode::Expression {
                expression: last,
                meta: JsNodeMeta {},
            })
        } else {
            Err(JsAbstractorError::None)
        }
    }

    pub fn expr_decl(&mut self) -> Result<JsNode, JsAbstractorError> {
        self.expr(|token| {
            matches!(
                token,
                JsToken::Punctuation(JsTokenPunctuation::Semicolon, _)
            )
        })
    }

    pub fn var_decl(&mut self) -> Result<JsNode, JsAbstractorError> {
        if self.pointer >= self.tokens.len() {
            return Err(JsAbstractorError::None);
        }

        let curr_token = &self.tokens[self.pointer];

        if let JsToken::Keyword(JsTokenKeyword::Const, _) = curr_token {
            self.pointer += 1;
            let Some(key) = self.tokens.get(self.pointer) else {
                return Err(JsAbstractorError::ExpectedToken("JsToken::Variable"));
            };
            let key = key.clone();
            self.pointer += 1;
            if self.tokens.get(self.pointer).is_none() {
                return Err(JsAbstractorError::ExpectedToken(
                    "JsToken::Punctuation(JsTokenPunctuation::Equal)",
                ));
            }
            self.pointer += 1;
            let value = match self.expr_decl() {
                Ok(v) => v,
                Err(JsAbstractorError::None) => {
                    return Err(JsAbstractorError::ExpectedToken("JsNode::Expression"))
                }
                Err(e) => return Err(e),
            };

            self.pointer += 1;

            return Ok(JsNode::VariableDeclaration {
                key,
                value: Box::from(value),
                meta: JsNodeMeta {},
            });
        }

        Err(JsAbstractorError::None)
    }

    pub fn next_iterate(&mut self) -> Result<JsNode, JsAbstractorError> {
        if self.pointer >= self.tokens.len() {
            return Err(JsAbstractorError::None);
        }

        let mut t = self.var_decl();

        if let Err(JsAbstractorError::None) = t {
            t = self.expr_decl();
        }

        if let Err(ref e) = t {
            self.pointer += 1;
            eprintln!("{e}");
        }

        t
    }

    pub fn build_ast(&mut self) -> JsNode {
        let lines = self.collect::<Vec<JsNode>>();

        JsNode::Program {
            child: Box::from(lines),
            meta: JsNodeMeta {},
        }
    }
}
impl std::iter::Iterator for JsAbstractor {
    type Item = JsNode;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.pointer >= self.tokens.len() {
                return None;
            }

            if let Ok(token) = self.next_iterate() {
                return Some(token);
            }
        }
    }
}
