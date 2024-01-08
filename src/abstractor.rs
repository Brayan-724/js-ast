// #[derive(Debug, Clone)]
// pub enum JsAstNode {
//     Block(Box<[JsNode]>),
//     VariableDeclaration(JsToken, JsNode)
// }

use crate::{JsToken, JsTokenKeyword, JsTokenLiteral, JsTokenPunctuation};

macro_rules! otherwise_error {
    ($carrier:ident, $fn:expr) => {
        if let Err(JsAbstractorError::None) = $carrier {
            $carrier = $fn;
        }
    };
}

#[derive(Debug, Clone)]
pub struct JsNodeMeta {}

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

    pub(crate) fn expr_end_paren_impl(token: &JsToken) -> (bool, u8) {
        match token {
            JsToken::Punctuation(JsTokenPunctuation::RightParen, _) => (true, 1),
            _ => (false, 0),
        }
    }

    pub(crate) fn expr_end_bracket_impl(token: &JsToken) -> (bool, u8) {
        match token {
            JsToken::Punctuation(JsTokenPunctuation::RightBracket, _) => (true, 1),
            _ => (false, 0),
        }
    }

    pub(crate) fn expr_end_paren_or_coma_impl(token: &JsToken) -> (bool, u8) {
        match token {
            JsToken::Punctuation(JsTokenPunctuation::RightParen, _) => (true, 1),
            JsToken::Punctuation(JsTokenPunctuation::Coma, _) => (true, 2),
            _ => (false, 0),
        }
    }

    pub fn impl_expr_token_variable(
        &self,
        token: &JsToken,
    ) -> Result<JsNodeExpression, JsAbstractorError> {
        Ok(JsNodeExpression::Variable {
            name: token.clone(),
        })
    }

    pub fn impl_expr_token_punctuation_dot(
        &mut self,
        last: &Option<JsNodeExpression>,
    ) -> Result<JsNodeExpression, JsAbstractorError> {
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
        Ok(JsNodeExpression::Member {
            from: Box::from(last.clone().unwrap()),
            member: Box::from(tk),
        })
    }

    pub fn impl_expr_token_punctuation_leftparen(
        &mut self,
        last: &Option<JsNodeExpression>,
    ) -> Result<JsNodeExpression, JsAbstractorError> {
        let mut arguments = vec![];

        loop {
            self.pointer += 1;
            let tk = self.expr(JsAbstractor::expr_end_paren_or_coma_impl);

            match tk {
                Ok((JsNode::Expression { expression, .. }, 2)) => {
                    arguments.push(expression);
                }
                Ok((JsNode::Expression { expression, .. }, 1)) => {
                    arguments.push(expression);
                    break Ok(JsNodeExpression::FunctionCall {
                        call: Box::from(last.clone().unwrap()),
                        arguments: Box::from(arguments),
                    });
                }
                Err(JsAbstractorError::None) => {
                    break Ok(JsNodeExpression::FunctionCall {
                        call: Box::from(last.clone().unwrap()),
                        arguments: Box::from([]),
                    });
                }
                Ok(..) => return Err(JsAbstractorError::ExpectedToken("expression")),
                Err(e) => return Err(e),
            }
        }
    }

    pub fn expr<E>(&mut self, mut end: E) -> Result<(JsNode, u8), JsAbstractorError>
    where
        E: FnMut(&JsToken) -> (bool, u8),
    {
        let mut code = 0;
        let mut last: Option<JsNodeExpression> = None;
        let mut awaiting_assignment: Option<JsToken> = None;
        let mut binary_last: Option<JsNodeExpression> = None;
        let mut binary_op: Option<JsNodeExpressionBinary> = None;
        while self.pointer < self.tokens.len() {
            let tk = &self.tokens[self.pointer];
            let e = end(tk);
            if e.0 {
                code = e.1;
                break;
            }

            let next = match tk {
                JsToken::Keyword(..) => {
                    return Err(JsAbstractorError::None);
                }
                tk @ JsToken::Variable(..) => Some(self.impl_expr_token_variable(tk)?),
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
                    Some(self.impl_expr_token_punctuation_dot(&last)?)
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
                    Some(self.impl_expr_token_punctuation_leftparen(&last)?)
                }

                JsToken::Punctuation(JsTokenPunctuation::LeftBracket, _) if last.is_some() => {
                    self.pointer += 1;
                    let tk = self.expr(JsAbstractor::expr_end_bracket_impl)?.0;
                    let JsNode::Expression { expression: tk, .. } = tk else {
                        return Err(JsAbstractorError::ExpectedToken("JsNode::Expression"));
                    };
                    Some(JsNodeExpression::Member {
                        from: Box::from(last.clone().unwrap()),
                        member: Box::from(tk),
                    })
                }
                JsToken::Punctuation(JsTokenPunctuation::Plus, ..) if last.is_some() => {
                    binary_last = last;
                    binary_op = Some(JsNodeExpressionBinary::Add);
                    None
                }
                JsToken::Punctuation(JsTokenPunctuation::Star, ..) if last.is_some() => {
                    binary_last = last;
                    binary_op = Some(JsNodeExpressionBinary::Mult);
                    None
                }
                JsToken::Punctuation(JsTokenPunctuation::Slash, ..) if last.is_some() => {
                    binary_last = last;
                    binary_op = Some(JsNodeExpressionBinary::Divide);
                    None
                }
                JsToken::Punctuation(JsTokenPunctuation::Hyphen, ..) if last.is_some() => {
                    binary_last = last;
                    binary_op = Some(JsNodeExpressionBinary::Sub);
                    None
                }

                JsToken::Punctuation(JsTokenPunctuation::LessThan, ..) if last.is_some() => {
                    binary_last = last;
                    binary_op = Some(JsNodeExpressionBinary::LessThan);
                    None
                }

                JsToken::Punctuation(JsTokenPunctuation::Bang, ..) if last.is_some() => {
                    self.pointer += 1;
                    let curr_token = &self.tokens[self.pointer];
                    if matches!(
                        curr_token,
                        JsToken::Punctuation(JsTokenPunctuation::Equal, ..)
                    ) {
                        binary_last = last;

                        self.pointer += 1;
                        let curr_token = &self.tokens[self.pointer];

                        if matches!(
                            curr_token,
                            JsToken::Punctuation(JsTokenPunctuation::Equal, ..)
                        ) {
                            binary_op = Some(JsNodeExpressionBinary::NotStrictlyEqual);
                        } else {
                            todo!("Binary operator: !=")
                        }
                    } else {
                        todo!("Unary operator: !")
                    }

                    None
                }

                JsToken::Punctuation(JsTokenPunctuation::Equal, ..)
                    if matches!(last, Some(JsNodeExpression::Variable { .. },)) =>
                {
                    let JsNodeExpression::Variable { name } = last.unwrap() else {
                        unreachable!();
                    };

                    awaiting_assignment = Some(name);

                    None
                }

                JsToken::Literal(l, _) => Some(JsNodeExpression::Literal { value: l.clone() }),
                tk => todo!("{tk:#?}"),
            };

            // let next = if let Some(next) = next {
            //         Some(next)
            // } else {
            //     None
            // };

            last = next;

            self.pointer += 1;
        }

        if let Some(last) = last {
            let last = if let Some(binary_op) = binary_op {
                JsNodeExpression::BinaryOp {
                    kind: binary_op,
                    left: Box::from(binary_last.unwrap()),
                    right: Box::from(last),
                }
            } else {
                last
            };
            let last = if let Some(awaiting_assignment) = awaiting_assignment {
                JsNodeExpression::Assigment {
                    variable: awaiting_assignment,
                    value: Box::new(last),
                }
            } else {
                last
            };

            Ok((
                JsNode::Expression {
                    expression: last,
                    meta: JsNodeMeta {},
                },
                code,
            ))
        } else {
            Err(JsAbstractorError::None)
        }
    }

    pub fn expr_decl(&mut self) -> Result<JsNode, JsAbstractorError> {
        self.expr(|token| {
            (
                matches!(
                    token,
                    JsToken::Punctuation(JsTokenPunctuation::Semicolon, _)
                ),
                0,
            )
        })
        .map(|a| a.0)
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

    pub fn block_expr(&mut self) -> Result<JsNode, JsAbstractorError> {
        if self.pointer >= self.tokens.len() {
            return Err(JsAbstractorError::None);
        }

        let curr_token = &self.tokens[self.pointer];

        if let JsToken::Punctuation(JsTokenPunctuation::LeftBrace, _) = curr_token {
            let mut children = vec![];

            loop {
                self.pointer += 1;
                if self.pointer >= self.tokens.len() {
                    return Err(JsAbstractorError::None);
                }

                let curr_token = &self.tokens[self.pointer];

                if let JsToken::Punctuation(JsTokenPunctuation::RightBrace, ..) = curr_token {
                    self.pointer += 1;
                    return Ok(JsNode::Block {
                        children: Box::from(children),
                        meta: JsNodeMeta {},
                    });
                }

                let a = match self.process_line() {
                    Ok(a) => a,
                    Err(JsAbstractorError::None) => continue,
                    Err(e) => return Err(e),
                };
                children.push(a);
            }
        }

        Err(JsAbstractorError::None)
    }

    pub fn block_or_inline(&mut self) -> Result<JsNode, JsAbstractorError> {
        self.block_expr().or_else(|_| self.expr_decl())
    }

    pub fn if_statement(&mut self) -> Result<JsNode, JsAbstractorError> {
        if self.pointer >= self.tokens.len() {
            return Err(JsAbstractorError::None);
        }

        let curr_token = &self.tokens[self.pointer];

        if let JsToken::Keyword(JsTokenKeyword::If, _) = curr_token {
            self.pointer += 1;
            if !matches!(
                self.tokens.get(self.pointer),
                Some(JsToken::Punctuation(JsTokenPunctuation::LeftParen, ..))
            ) {
                return Err(JsAbstractorError::ExpectedToken(
                    "JsToken::Punctuation(JsTokenPunctuation::LeftParen)",
                ));
            }
            self.pointer += 1;
            let condition = self.expr(JsAbstractor::expr_end_paren_impl)?.0;
            let condition = match condition {
                JsNode::Expression { expression, .. } => expression,
                _ => unreachable!(),
            };

            self.pointer += 1;
            let inner = match self.block_or_inline() {
                Ok(v) => v,
                Err(JsAbstractorError::None) => {
                    return Err(JsAbstractorError::ExpectedToken(
                        "JsNode::Block or JsNode::Expression",
                    ))
                }
                Err(e) => return Err(e),
            };

            let else_case = if matches!(
                self.tokens.get(self.pointer),
                Some(JsToken::Keyword(JsTokenKeyword::Else, ..))
            ) {
                self.pointer += 1;
                let mut t = Err(JsAbstractorError::None);
                otherwise_error!(t, self.if_statement());
                otherwise_error!(t, self.block_or_inline());
                t.ok()
            } else {
                None
            };

            return Ok(JsNode::Statement {
                statement: JsNodeStatement::If {
                    condition: Box::from(condition),
                    inner: Box::from(inner),
                    otherwise: Box::from(else_case),
                },
                meta: JsNodeMeta {},
            });
        }

        Err(JsAbstractorError::None)
    }

    pub fn while_statement(&mut self) -> Result<JsNode, JsAbstractorError> {
        if self.pointer >= self.tokens.len() {
            return Err(JsAbstractorError::None);
        }

        let curr_token = &self.tokens[self.pointer];

        if let JsToken::Keyword(JsTokenKeyword::While, _) = curr_token {
            self.pointer += 1;
            if !matches!(
                self.tokens.get(self.pointer),
                Some(JsToken::Punctuation(JsTokenPunctuation::LeftParen, ..))
            ) {
                return Err(JsAbstractorError::ExpectedToken(
                    "JsToken::Punctuation(JsTokenPunctuation::LeftParen)",
                ));
            }
            self.pointer += 1;
            let condition = self.expr(JsAbstractor::expr_end_paren_impl)?.0;
            let condition = match condition {
                JsNode::Expression { expression, .. } => expression,
                _ => unreachable!(),
            };

            self.pointer += 1;
            let inner = match self.block_or_inline() {
                Ok(v) => v,
                Err(JsAbstractorError::None) => {
                    return Err(JsAbstractorError::ExpectedToken(
                        "JsNode::Block or JsNode::Expression",
                    ))
                }
                Err(e) => return Err(e),
            };

            return Ok(JsNode::Statement {
                statement: JsNodeStatement::While {
                    condition: Box::from(condition),
                    inner: Box::from(inner),
                },
                meta: JsNodeMeta {},
            });
        }

        Err(JsAbstractorError::None)
    }

    pub fn process_line(&mut self) -> Result<JsNode, JsAbstractorError> {
        let mut t = Err(JsAbstractorError::None);
        otherwise_error!(t, self.var_decl());
        otherwise_error!(t, self.if_statement());
        otherwise_error!(t, self.while_statement());
        otherwise_error!(t, self.expr_decl());

        t
    }

    pub fn next_iterate(&mut self) -> Result<JsNode, JsAbstractorError> {
        if self.pointer >= self.tokens.len() {
            return Err(JsAbstractorError::None);
        }

        let t = self.process_line();

        if let Err(ref e) = t {
            self.pointer += 1;
            match e {
                JsAbstractorError::None => (),
                e => eprintln!("{e}"),
            }
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
