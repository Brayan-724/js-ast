#[derive(Debug, Clone)]
pub enum JsTokenKeyword {
    Const,
    // Let,
    // Function,
}

impl TryFrom<&str> for JsTokenKeyword {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "const" => Ok(Self::Const),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum JsTokenPunctuation {
    LeftParen,  // (
    RightParen, // )
    // LeftBrace,     // {
    // RightBrace,    // }
    Coma,      // ,
    Semicolon, // ;
    Dot,       // .
    Equal,     // =
    // EqualEqual,    // ==
    // MoreThan,      // >
    // MoreThanEqual, // >=
    // LessThan,      // <
    // LessThanEqual, // <=
    Plus, // +
}

impl TryFrom<&str> for JsTokenPunctuation {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "(" => Ok(Self::LeftParen),
            ")" => Ok(Self::RightParen),
            "," => Ok(Self::Coma),
            ";" => Ok(Self::Semicolon),
            "." => Ok(Self::Dot),
            "=" => Ok(Self::Equal),
            "+" => Ok(Self::Plus),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum JsTokenLiteral {
    Number(f64), // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number#number_encoding
                 // String(Box<str>),
}

impl JsTokenLiteral {
    pub fn is_number(value: &str) -> bool {
        if value.is_empty() {
            return false;
        }

        let mut has_dot = false;
        let mut has_num = false;

        for (idx, c) in value.chars().enumerate() {
            match c {
                // Verify unique dot
                '.' if has_dot => return false,
                '.' => has_dot = true,

                // Handle negative
                '-' if idx == 0 => (),

                '0'..='9' => has_num = true,
                _ => return false,
            }
        }

        return has_num;
    }
}

impl TryFrom<&str> for JsTokenLiteral {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if JsTokenLiteral::is_number(value) {
            // is_number validate to f64
            Ok(JsTokenLiteral::Number(value.parse().unwrap()))
        } else {
            Err(())
        }
    }
}

#[derive(Clone)]
pub struct JsTokenMeta {
    line: usize,
    column: usize,
}

impl std::fmt::Debug for JsTokenMeta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.line, self.column))
    }
}

#[derive(Debug, Clone)]
pub enum JsToken {
    // Order in importance
    Punctuation(JsTokenPunctuation, JsTokenMeta),
    Keyword(JsTokenKeyword, JsTokenMeta),
    Literal(JsTokenLiteral, JsTokenMeta),
    Variable(Box<str>, JsTokenMeta),
}

impl JsToken {
    fn try_from(value: &str, meta: JsTokenMeta) -> Option<Self> {
        if let Ok(token) = JsTokenPunctuation::try_from(value) {
            Some(Self::Punctuation(token, meta))
        } else if let Ok(token) = JsTokenKeyword::try_from(value) {
            Some(Self::Keyword(token, meta))
        } else if let Ok(token) = JsTokenLiteral::try_from(value) {
            Some(Self::Literal(token, meta))
        } else {
            Some(Self::Variable(Box::from(value), meta))
        }
    }
}

#[derive(Debug, Clone)]
pub struct JsTokenizer {
    source_code: Box<str>,
    chars: Box<[char]>,
    buffering_started: bool,
    last_pointer: usize,
    pointer: usize,

    line: usize,
    column: usize,
}

impl JsTokenizer {
    pub fn new_from_text(source_code: Box<str>) -> Self {
        let chars = Box::from(source_code.chars().collect::<Vec<char>>());

        Self {
            source_code,
            chars,
            buffering_started: false,
            last_pointer: 0,
            pointer: 0,

            line: 1,
            column: 0,
        }
    }

    #[inline(always)]
    fn move_pointer(&mut self, amount: i8) {
        if amount.is_negative() {
            let amount = amount.abs() as usize;

            if amount > self.column {
                self.line -= 1;
                // FIXME: URGENT, Precise column changing
                self.column = 0;
            } else {
                self.column -= amount;
            }
            self.pointer -= amount;
        } else {
            let amount = amount as usize;

            self.column += amount;
            self.pointer += amount;
        }
    }

    /// Standarize meta generation
    #[inline(always)]
    #[must_use = "This standarize the meta generation"]
    const fn get_meta(&self) -> JsTokenMeta {
        JsTokenMeta {
            line: self.line,
            column: self.column,
        }
    }

    fn next_iterate(&mut self) -> Option<JsToken> {
        let curr_char = self.chars[self.pointer];

        match curr_char {
            'a'..='z' | 'A'..='Z' => {
                self.move_pointer(1);
            }
            '0'..='9' => {
                self.move_pointer(1);
            }
            _ => {
                if curr_char == '\n' {
                    let token = Some(JsToken::Punctuation(
                        JsTokenPunctuation::Semicolon,
                        self.get_meta(),
                    ));
                    self.line += 1;
                    self.column = 0;
                    self.pointer += 1;
                    self.last_pointer = self.pointer;
                    return token;
                }

                let token = self.analize_token();

                self.buffering_started = false;
                self.last_pointer = self.pointer
                    + if self.last_pointer == self.pointer {
                        self.move_pointer(1);
                        1
                    } else {
                        0
                    };

                return token;
            }
        }
        None
    }

    fn analize_token(&mut self) -> Option<JsToken> {
        let length = self.pointer - self.last_pointer;

        // if length <= 0 {
        //     return None;
        // }
        //
        // let pointer = self.pointer;
        let pointer_change = if length <= 0 {
            if self.pointer < self.source_code.len() {
                1
            } else {
                return None;
            }
        } else {
            0
        };

        let token = &self.source_code[self.last_pointer..self.pointer + pointer_change];
        let token = token.trim();

        if token.is_empty() {
            return None;
        }

        JsToken::try_from(token, self.get_meta())
    }
}

impl std::iter::Iterator for JsTokenizer {
    type Item = JsToken;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.pointer >= self.source_code.len() {
                return None;
            }

            if let Some(token) = self.next_iterate() {
                return Some(token);
            };
        }
    }
}

// #[derive(Debug, Clone)]
// pub enum JsAstNode {
//     Block(Box<[JsNode]>),
//     VariableDeclaration(JsToken, JsNode)
// }

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
        members: Box<[JsNodeExpression]>,
    },
    Literal {
        value: JsTokenLiteral,
    },
}

#[derive(Debug, Clone)]
pub enum JsNode {
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
                    let tk = JsNodeExpression::Variable { name: tk.clone() };
                    last = Some(JsNodeExpression::Member {
                        members: Box::from([last.unwrap(), tk]),
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
