use crate::parser;
use parser::TokenType;
use anyhow::Result;

struct ParseContext {
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Type {
	pub name: String,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Parameter {
	pub name: String,
	pub param_type: Type,
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum NumberInt {
	Any(i32),
	I8(i8), U8(u8),
	I16(i16), U16(u16),
	I32(i32), U32(u32),
	I64(i64), U64(u64),
	I128(i128), U128(u128),
	ISize(i64), USize(u64),
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum NumberFloat {
	F32(f32),
	F64(f64),
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum Number {
	Int(NumberInt),
	Float(NumberFloat),
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct CodeBlock {
	pub stmts: Vec<Stmt>,
	pub ret_expr: Option<Box<Expr>>,
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum Expr {
	Add{ left: Box<Expr>, right: Box<Expr> },
	Sub{ left: Box<Expr>, right: Box<Expr> },
	Mul{ left: Box<Expr>, right: Box<Expr> },
	Div{ left: Box<Expr>, right: Box<Expr> },
	Mod{ left: Box<Expr>, right: Box<Expr> },
	Token{ id: String },
	Number{ value: Number },
	Block(CodeBlock),
	Function{ params: Vec<Parameter>, ret_type: Type, code: CodeBlock },
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum Stmt {
	Let{ name: String, expr: Expr },
	Ret{ expr: Option<Expr> },
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Root {
	pub stmts: Vec<Stmt>,
}

fn parse_number(_ctx: &mut ParseContext, tok: &parser::Token) -> Result<Number> {
	match tok.ttype {
		TokenType::NumberDec => {
			if let Ok(val) = tok.source.parse::<i32>() {
				return Ok(Number::Int(NumberInt::Any(val)));
			}
			// TODO
			return Err(anyhow::anyhow!("frontend::parse_number:(...): Can't parse NumberDec: '{}'", tok.source));
		},
		_ => {
			return Err(anyhow::anyhow!("frontend::parse_number:(...): Unexpected TokenType: {:?}", tok.ttype));
		},
	}
}

fn parse_type(ctx: &mut ParseContext, tok: &parser::Token) -> Result<Type> {
	match tok.ttype {
		TokenType::Token => {
			return Ok(Type {
				name: tok.source.to_owned(),
			});
		},
		TokenType::TokenType => {
			return parse_type(ctx, &tok.tree[0]);
		},
		_ => {
			return Err(anyhow::anyhow!("frontend::parse_type:(...): Unexpected TokenType: {:?}", tok.ttype));
		},
	}
}

fn parse_expr_block(ctx: &mut ParseContext, tok: &parser::Token) -> Result<CodeBlock> {
	if tok.ttype != TokenType::ExprBlock {
		return Err(anyhow::anyhow!("frontend::parse_expr_block:(...): Unexpected TokenType: {:?}", tok.ttype));
	}

	let group = tok.subtoken(2, TokenType::ExprBlockG0)?;

	let mut stmts = Vec::new();

	for t in &group.tree {
		let stmt = t.subtoken(0, TokenType::Stmt)?;
		stmts.push(parse_stmt(ctx, stmt)?);
	}

	let maybe_expr = tok.subtoken(3, TokenType::ExprBlockG1)?;

	let ret_expr =  if maybe_expr.tree.len() > 0 {
		let expr = tok.subtoken(0, TokenType::ExprBlockG1G0)?;
		Some(Box::new(parse_expr(ctx, expr)?))
	} else {
		None
	};

	return Ok(CodeBlock { stmts, ret_expr, });
}

fn parse_expr_basic(ctx: &mut ParseContext, tok: &parser::Token) -> Result<Expr> {
	match tok.ttype {
		TokenType::Token => {
			return Ok(Expr::Token{ id: tok.source.to_owned() });
		},
		TokenType::Number => {
			return Ok(Expr::Number{ value: parse_number(ctx, &tok.tree[0])? });
		},
		TokenType::ExprFunction => {
			let maybe_params = tok.subtoken(2, TokenType::ExprFunctionG0)?;

			let mut params = Vec::new();

			if maybe_params.tree.len() > 0 {
				let params_decl = maybe_params.subtoken(0, TokenType::ExprFunctionG0G0)?.subtoken(0, TokenType::ParamsDecl)?;

				let param_name = params_decl.subtoken(0, TokenType::Token)?;
				let param_type = params_decl.subtoken(4, TokenType::TokenType)?;

				params.push(Parameter {
					name: param_name.source.to_owned(),
					param_type: parse_type(ctx, param_type)?,
				});

				let _len = params_decl.tree.len();
				let _range = 5.._len;

				for i in 5..params_decl.tree.len() {
					let group = params_decl.subtoken(i, TokenType::ParamsDeclG0)?;
					let param_name = group.subtoken(3, TokenType::Token)?;
					let param_type = group.subtoken(7, TokenType::TokenType)?;

					params.push(Parameter {
						name: param_name.source.to_owned(),
						param_type: parse_type(ctx, param_type)?,
					});
				}
			}

			let maybe_ret_type = tok.subtoken(5, TokenType::ExprFunctionG1)?;

			let ret_type = if maybe_ret_type.tree.len() > 0 {
				let ret_type = maybe_ret_type.subtoken(0, TokenType::ExprFunctionG1G0)?.subtoken(3, TokenType::TokenType)?;
				parse_type(ctx, ret_type)?
			} else {
				return Err(anyhow::anyhow!("parse_expr_basic(...): ExprFunction: Auto return type not implemented"));
			};

			let block_or_expr = tok.subtoken(6, TokenType::ExprFunctionG2)?;
			let code = match block_or_expr.tree[0].ttype {
				TokenType::ExprBlock => parse_expr_block(ctx, &block_or_expr.tree[0])?,
				TokenType::Expr => CodeBlock { stmts: Vec::new(), ret_expr: Some(Box::new(parse_expr(ctx, &block_or_expr.tree[0])?)) },
				_ => { return Err(anyhow::anyhow!("parse_expr_basic(...): ExprFunction: Unexpected code TokenType: {:?}", block_or_expr.tree[0].ttype)); },
			};

			return Ok(Expr::Function {
				params,
				ret_type: ret_type,
				code: code,
			});
		},
		_ => {
			return Err(anyhow::anyhow!("frontend::parse_expr_basic:(...): Unexpected TokenType: {:?}", tok.ttype));
		},
	}
}

fn parse_expr(ctx: &mut ParseContext, tok: &parser::Token) -> Result<Expr> {
	match tok.ttype {
		TokenType::ExprBasic => {
			return parse_expr_basic(ctx, &tok.tree[0]);
		},
		TokenType::ExprMul => {
			let left = tok.subtoken(0, TokenType::ExprBasic)?;

			if tok.tree.len() > 1 {
				let group = tok.subtoken(1, TokenType::ExprMulG0)?;
				let op = &group.subtoken(1, TokenType::ExprMulG0G0)?.tree[0];
				let right = group.subtoken(3, TokenType::ExprBasic)?;

				let left_expr = Box::new(parse_expr(ctx, left)?);
				let right_expr = Box::new(parse_expr(ctx, right)?);

				return match op.ttype {
					TokenType::OpMul => Ok(Expr::Mul{
						left: left_expr,
						right: right_expr,
					}),
					TokenType::OpDiv => Ok(Expr::Div{
						left: left_expr,
						right: right_expr,
					}),
					TokenType::OpMod => Ok(Expr::Mod{
						left: left_expr,
						right: right_expr,
					}),
					_ => Err(anyhow::anyhow!("frontend::parse_expr:(...): ExprMul Unexpected Operator TokenType: {:?}", op.ttype)),
				};
			} else {
				return parse_expr(ctx, left);
			}
		},
		TokenType::ExprAdd => {
			let left = tok.subtoken(0, TokenType::ExprMul)?;

			if tok.tree.len() > 1 {
				let group = tok.subtoken(1, TokenType::ExprAddG0)?;
				let op = &group.subtoken(1, TokenType::ExprAddG0G0)?.tree[0];
				let right = group.subtoken(3, TokenType::ExprMul)?;

				let left_expr = Box::new(parse_expr(ctx, left)?);
				let right_expr = Box::new(parse_expr(ctx, right)?);

				return match op.ttype {
					TokenType::OpAdd => Ok(Expr::Add{
						left: left_expr,
						right: right_expr,
					}),
					TokenType::OpSub => Ok(Expr::Sub{
						left: left_expr,
						right: right_expr,
					}),
					_ => Err(anyhow::anyhow!("frontend::parse_expr:(...): ExprAdd Unexpected Operator TokenType: {:?}", op.ttype)),
				};
			} else {
				return parse_expr(ctx, left);
			}
		},
		TokenType::Expr => {
			return parse_expr(ctx, tok.subtoken(0, TokenType::ExprAdd)?);
		},
		_ => {
			return Err(anyhow::anyhow!("frontend::parse_expr:(...): Unexpected TokenType: {:?}", tok.ttype));
		},
	}
}

fn parse_stmt(ctx: &mut ParseContext, tok: &parser::Token) -> Result<Stmt> {
	match tok.ttype {
		TokenType::StmtLet => {
			let token = tok.subtoken(2, TokenType::Token)?;
			let expr = tok.subtoken(6, TokenType::Expr)?;

			return Ok(Stmt::Let{
				name: token.source.to_owned(),
				expr: parse_expr(ctx, expr)?,
			});
		},
		TokenType::StmtRet => {
			let maybe_group = tok.subtoken(1, TokenType::StmtRetG0)?;

			if maybe_group.tree.len() > 0 {
				let group = maybe_group.subtoken(0, TokenType::StmtRetG0G0)?;
				let expr = group.subtoken(1, TokenType::Expr)?;
	
				return Ok(Stmt::Ret{
					expr: Some(parse_expr(ctx, expr)?),
				});
			} else {
				return Ok(Stmt::Ret{
					expr: None,
				});
			}
		},
		TokenType::Stmt => {
			return parse_stmt(ctx, &tok.tree[0]);
		},
		_ => {
			return Err(anyhow::anyhow!("frontend::parse_stmt:(...): Unexpected TokenType: {:?}", tok.ttype));
		},
	}
}

fn parse_file(ctx: &mut ParseContext, tok: &parser::Token) -> Result<Root> {
	if tok.ttype != TokenType::File {
		return Err(anyhow::anyhow!("frontend::parse:(...): Unexpected TokenType: {:?}", tok.ttype));
	}

	let mut stmts = Vec::new();

	for i in 1..tok.tree.len() {
		let group = tok.subtoken(i, TokenType::FileG0)?;
		let stmt = group.subtoken(0, TokenType::Stmt)?;

		stmts.push(parse_stmt(ctx, stmt)?);
	}

	return Ok(Root{ stmts });
}

pub fn parse(tok: &parser::Token) -> Result<Root> {
	let mut ctx = ParseContext {};
	return parse_file(&mut ctx, tok);
}
