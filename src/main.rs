use js_ast::{JsAbstractor, JsNode, JsNodeExpression, JsToken, JsTokenLiteral, JsTokenizer};

fn main() {
    let mut args = std::env::args();
    let file = args.nth(1).expect("Provide a js file to execute");
    let file = std::fs::read_to_string(file).expect("Cannot read file");

    let tokenizer = JsTokenizer::new_from_text(Box::from(file));
    let tokens = tokenizer.collect::<Vec<JsToken>>();

    // println!("{tokens:#?}");

    let ast = JsAbstractor::new_from_tokens(tokens.into()).build_ast();

    let JsNode::Program { child: ast, .. } = ast else {
        unreachable!();
    };

    // println!("{ast:#?}");

    let commands = compile_scope(&ast);
    println!("-----------------------------------");
    println!("{:#?}", commands);
    println!("-----------------------------------");

    let config = amvm::AmvmHeader {
        sum_kind: amvm::AmvmTypeCasting::TypeCastingStrictlessString,
    };
    amvm::Program::new(config, commands)
        .runtime()
        .run()
        .unwrap();
}

fn compile_scope(scope: &[JsNode]) -> Vec<amvm::Command> {
    let mut r = Vec::with_capacity(scope.len());

    for node in scope {
        r.push(compile_cmd(node));
    }

    r
}

fn compile_cmd(cmd: &JsNode) -> amvm::Command {
    match cmd {
        JsNode::Expression { expression, .. } => match expression {
            JsNodeExpression::FunctionCall { call, arguments } => match call.as_ref() {
                JsNodeExpression::Variable {
                    name: JsToken::Variable(name, ..),
                } if name == &Box::from("log") => {
                    return amvm::Command::Puts {
                        value: arguments
                            .iter()
                            .map(|a| compile_expr(a))
                            .fold(None, |acc, i| {
                                if let Some(acc) = acc {
                                    Some(amvm::CommandExpression::Addition(
                                        Box::from(acc),
                                        Box::from(i),
                                    ))
                                } else {
                                    Some(i)
                                }
                            })
                            .unwrap_or(amvm::CommandExpression::Value(amvm::Value::Null)),
                    }
                }

                _ => unimplemented!(),
            },
            JsNodeExpression::Assigment { variable, value } => {
                return amvm::Command::AssignVariable {
                    name: compile_var(variable),
                    value: compile_expr(value.as_ref()),
                }
            }
            expression => todo!("{expression:#?}"),
        },
        JsNode::VariableDeclaration { key, value, .. } => {
            return amvm::Command::DeclareVariable {
                kind: amvm::VariableKind::Let,
                name: compile_var(key),
                value: compile_node_expr(value),
            }
        }
        JsNode::Block {
            label: _, children, ..
        } => {
            return amvm::Command::Scope {
                body: compile_scope(children),
            }
        }
        JsNode::Statement { statement, .. } => match statement {
            js_ast::JsNodeStatement::While {
                condition, inner, ..
            } => {
                let JsNode::Block {
                    label: _,
                    children: inner,
                    ..
                } = inner.as_ref()
                else {
                    unreachable!();
                };
                let inner = compile_scope(
                    &[
                        &[js_ast::JsNode::Statement {
                            statement: js_ast::JsNodeStatement::If {
                                condition: Box::from(JsNodeExpression::BinaryOp {
                                    kind: js_ast::JsNodeExpressionBinary::NotStrictlyEqual,
                                    left: condition.clone(),
                                    right: Box::from(JsNodeExpression::Literal {
                                        value: JsTokenLiteral::Boolean(true),
                                    }),
                                }),
                                inner: Box::from(JsNode::Block {
                                    label: None,
                                    children: Box::from([JsNode::Statement {
                                        statement: js_ast::JsNodeStatement::Break { label: None },
                                        meta: js_ast::JsNodeMeta {},
                                    }]),
                                    meta: js_ast::JsNodeMeta {},
                                }),
                                otherwise: Box::new(None),
                            },
                            meta: js_ast::JsNodeMeta {},
                        }],
                        inner.as_ref(),
                    ]
                    .concat(),
                );
                return amvm::Command::Loop { body: inner };
            }
            js_ast::JsNodeStatement::If {
                condition,
                inner,
                otherwise,
                ..
            } => {
                let JsNode::Block {
                    label: _,
                    children: inner,
                    ..
                } = inner.as_ref()
                else {
                    unreachable!();
                };

                let otherwise = Option::as_ref(otherwise).map(|i| {
                    let JsNode::Block {
                        label: _,
                        children: inner,
                        ..
                    } = i
                    else {
                        unreachable!();
                    };
                    inner
                });

                let condition = compile_expr(&condition);
                let inner = compile_scope(inner);
                let otherwise = otherwise.map(|i| compile_scope(&i));

                amvm::Command::Conditional {
                    condition,
                    body: inner,
                    otherwise,
                }
            }
            js_ast::JsNodeStatement::Break { label: _ } => amvm::Command::Break,
            _ => todo!("{statement:#?}"),
        },
        _ => todo!("{cmd:#?}"),
    }
}

fn compile_expr(expr: &JsNodeExpression) -> amvm::CommandExpression {
    match expr {
        JsNodeExpression::Literal { value } => amvm::CommandExpression::Value(compile_value(value)),
        JsNodeExpression::BinaryOp { kind, left, right } => {
            let left = compile_expr(left);
            let right = compile_expr(right);

            match kind {
                js_ast::JsNodeExpressionBinary::Add => {
                    amvm::CommandExpression::Addition(Box::from(left), Box::from(right))
                }
                js_ast::JsNodeExpressionBinary::Sub => {
                    amvm::CommandExpression::Substraction(Box::from(left), Box::from(right))
                }
                js_ast::JsNodeExpressionBinary::NotStrictlyEqual => {
                    amvm::CommandExpression::BinaryCondition(
                        amvm::BinaryConditionKind::NotEqual,
                        Box::from(left),
                        Box::from(right),
                    )
                }
                js_ast::JsNodeExpressionBinary::LessThan => {
                    amvm::CommandExpression::BinaryCondition(
                        amvm::BinaryConditionKind::LessThan,
                        Box::from(left),
                        Box::from(right),
                    )
                }
                _ => todo!("{kind:?}"),
            }
        }
        JsNodeExpression::Variable { name } => amvm::CommandExpression::Var(compile_var(name)),
        JsNodeExpression::Member { from, member } => amvm::CommandExpression::Property(
            Box::from(compile_expr(from)),
            Box::from(compile_expr(member)),
        ),
        _ => todo!("{expr:#?}"),
    }
}

fn compile_node_expr(node: &JsNode) -> amvm::CommandExpression {
    let JsNode::Expression { expression, .. } = node else {
        unreachable!()
    };

    compile_expr(expression)
}

fn compile_var(name: &JsToken) -> amvm::Value {
    let JsToken::Variable(name, ..) = name else {
        unreachable!();
    };

    amvm::Value::String(String::from(name.as_ref()))
}

fn compile_value(value: &JsTokenLiteral) -> amvm::Value {
    match value {
        JsTokenLiteral::String(s) => amvm::Value::String(s.clone()),
        JsTokenLiteral::Number(n) => amvm::Value::U8(n.clone() as u8),
        JsTokenLiteral::Boolean(n) => amvm::Value::Bool(*n),
        _ => todo!("{value:#?}"),
    }
}
