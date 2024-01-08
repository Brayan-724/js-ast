use std::sync::{Arc, Mutex};

use js_ast::{JsAbstractor, JsRuntime, JsToken, JsTokenizer, JsValue};

macro_rules! js_value_parse {
    ({ $($key:ident : $value:expr),* }) => {{
        let mut map = ::std::collections::HashMap::new();
        $(
        map.insert(
            Box::from(stringify!($key)),
            Arc::new(Mutex::new($value)),
        );
        )*
        JsValue::Object(map)
    }};
}

fn console_log(args: Vec<Arc<Mutex<JsValue>>>) -> Arc<Mutex<JsValue>> {
    for arg in args {
        let arg = arg.lock().unwrap();
        let arg = arg.to_string();
        print!("{arg}");
    }
    print!("\n");
    Arc::new(Mutex::new(JsValue::Undefined))
}

fn main() {
    let mut args = std::env::args();
    let file = args.nth(1).expect("Provide a js file to execute");
    let file = std::fs::read_to_string(file).expect("Cannot read file");

    let tokenizer = JsTokenizer::new_from_text(Box::from(file));
    let tokens = tokenizer.collect::<Vec<JsToken>>();

    // println!("{tokens:#?}");

    let ast = JsAbstractor::new_from_tokens(tokens.into()).build_ast();

    // println!("{ast:#?}");

    let mut runtime = JsRuntime::new(ast);
    let console_log = js_ast::JsValueFunction::Binding(Arc::new(Box::new(console_log)));
    let console = js_value_parse!({
        log: JsValue::Function(console_log)
    });
    runtime.assign_variable(Box::from("console"), Arc::new(Mutex::new(console)));

    while runtime.next_iterate().is_some() {}
}
