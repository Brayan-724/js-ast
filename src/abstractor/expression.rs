use crate::abstractor::{JsToken, JsTokenLiteral};

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
