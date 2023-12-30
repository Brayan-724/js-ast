use js_ast::{JsToken, JsTokenKeyword, JsTokenPunctuation, JsTokenLiteral};

const FILE_CONTENT: &'static str = include_str!("./test.js");

fn main() {
    let ast = js_ast::JsTokenizer::new_from_text(Box::from(FILE_CONTENT));
    for token in ast {
        // println!("{token:?}");
        match token {
            JsToken::Keyword(JsTokenKeyword::Const, _) => print!("const "),
            JsToken::Punctuation(JsTokenPunctuation::LeftParen, _) => print!("("),
            JsToken::Punctuation(JsTokenPunctuation::RightParen, _) => print!(")"),
            JsToken::Punctuation(JsTokenPunctuation::Dot, _) => print!("."),
            JsToken::Punctuation(JsTokenPunctuation::Equal, _) => print!(" = "),
            JsToken::Punctuation(JsTokenPunctuation::Semicolon, _) => print!(";\n"),
            JsToken::Punctuation(JsTokenPunctuation::Plus, _) => print!(" + "),
            JsToken::Literal(JsTokenLiteral::Number(n), _) => print!("{n}"),
            JsToken::Variable(var, _) => print!("{var}"),
        }
    }
}
