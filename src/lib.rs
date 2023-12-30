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

#[derive(Debug, Clone)]
pub struct JsTokenMeta {
    line: usize,
    column: usize,
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
                    self.line += 1;
                    self.column = 0;
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

        let meta = JsTokenMeta {
            line: self.line,
            column: self.last_pointer
        };

        JsToken::try_from(token, meta)
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

// pub enum JsNode {}

// pub struct JsAstBuilder {
//     source_code: Box<str>,
// }
//
// impl JsAstBuilder {
//     pub fn new_from_text(source_code: Box<str>) -> Self {
//         Self { source_code }
//     }
// }
// impl std::iter::Iterator for JsAstBuilder {
//     type Item = JsNode;
//     fn next(&mut self) -> Option<Self::Item> {
//         None
//     }
// }
