use js_ast::{JsAbstractor, JsToken, JsTokenizer};

const FILE_CONTENT: &'static str = include_str!("./test.js");

fn main() {
    let tokenizer = JsTokenizer::new_from_text(Box::from(FILE_CONTENT));
    let tokens = tokenizer.collect::<Vec<JsToken>>();
    let ast = JsAbstractor::new_from_tokens(tokens.into());

    for token in ast {
        println!("{token:#?}");
    }
}
