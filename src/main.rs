use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use js_ast::{JsAbstractor, JsRuntime, JsToken, JsTokenizer, JsValue};

const FILE_CONTENT: &'static str = include_str!("./test.js");

fn console_log(args: Vec<Arc<Mutex<JsValue>>>) -> Arc<Mutex<JsValue>> {
    println!("{args:#?}");
    Arc::new(Mutex::new(JsValue::Undefined))
}

fn main() {
    let tokenizer = JsTokenizer::new_from_text(Box::from(FILE_CONTENT));
    let tokens = tokenizer.collect::<Vec<JsToken>>();
    let ast = JsAbstractor::new_from_tokens(tokens.into()).build_ast();

    // println!("{ast:#?}");

    let mut runtime = JsRuntime::new(ast);
    let mut console = HashMap::new();
    let console_log = js_ast::JsValueFunction::Binding(Arc::new(Box::new(console_log)));
    console.insert(
        Box::from("log"),
        Arc::new(Mutex::new(JsValue::Function(console_log))),
    );
    let console = JsValue::Object(console);
    runtime.assign_variable(Box::from("console"), Arc::new(Mutex::new(console)));

    while runtime.next_iterate().is_some() {}
}
