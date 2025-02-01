use crate::frontend;

use std::{cell::RefCell, rc::Rc};

use anyhow::{Context as _, Result};
use inkwell::{basic_block::BasicBlock, builder::Builder, context::Context, module::{Linkage, Module}, types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicTypeEnum, FunctionType}, values::{BasicValueEnum, FunctionValue, PointerValue}};

pub struct IntermediateCode {
	ctx: &'static Context,
	module: &'static Module<'static>,
}

impl IntermediateCode {
	fn new(module_name: &str) -> IntermediateCode {
		unsafe { 
			let ctx = &*Box::into_raw(Box::new(Context::create()));
			let module = &*Box::into_raw(Box::new(ctx.create_module(module_name)));

			IntermediateCode {
				ctx,
				module,
			}
		}
	}

	pub fn get_init_fn_name(&self) -> String {
		self.module.get_first_function().unwrap().get_name().to_str().unwrap().to_owned()
	}

	pub fn to_intermediate(&self) -> Result<String> {
		Ok(self.module.to_string())
	}

	pub fn to_asm(&self) -> Result<String> {
		use inkwell::{OptimizationLevel, targets::{Target, InitializationConfig, TargetTriple, TargetMachineOptions, FileType}};

		Target::initialize_x86(&InitializationConfig::default());

		let triple = TargetTriple::create("x86_64-pc-windows-msvc");
		let target = Target::from_triple(&triple).unwrap();
		let options = TargetMachineOptions::default()
			.set_cpu("x86-64")
			.set_features("+avx2")
			.set_abi("win32")
			.set_level(OptimizationLevel::None);

		let target_machine = target.create_target_machine_from_options(&triple, options).unwrap();
		return Ok(String::from_utf8(target_machine.write_to_memory_buffer(&self.module, FileType::Assembly).map_err(|s| anyhow::anyhow!("{}", s.to_string()))?.as_slice().to_vec())?);
	}

	pub fn to_obj(&self) -> Result<Vec<u8>> {
		use inkwell::{OptimizationLevel, targets::{Target, InitializationConfig, TargetTriple, TargetMachineOptions, FileType}};

		Target::initialize_x86(&InitializationConfig::default());

		let triple = TargetTriple::create("x86_64-pc-windows-msvc");
		let target = Target::from_triple(&triple).unwrap();
		let options = TargetMachineOptions::default()
			.set_cpu("x86-64")
			.set_features("+avx2")
			.set_abi("win32")
			.set_level(OptimizationLevel::None);

		let target_machine = target.create_target_machine_from_options(&triple, options).unwrap();
		return Ok(target_machine.write_to_memory_buffer(&self.module, FileType::Object).map_err(|s| anyhow::anyhow!("{}", s.to_string()))?.as_slice().to_vec());
	}
}

impl Drop for IntermediateCode {
	fn drop(&mut self) {
		unsafe {
			core::mem::drop(Box::from_raw(self.module as *const Module<'static> as *mut Module<'static>));
			core::mem::drop(Box::from_raw(self.ctx as *const Context as *mut Context));
		}
	}
}

#[derive(Clone)]
struct LocalVariableValue<'ctx> {
	ptr: PointerValue<'ctx>,
	value_type: BasicTypeEnum<'ctx>,
}

struct LocalVariable<'ctx> {
	name: String,
	value: LocalVariableValue<'ctx>,
}

struct Block<'ctx> {
	block: BasicBlock<'ctx>,
	vars: Vec<LocalVariable<'ctx>>,
}

struct Function<'ctx> {
	_func: FunctionValue<'ctx>,
	blocks: Vec<Rc<RefCell<Block<'ctx>>>>,
}

struct GenInfo<'ctx> {
	ctx: &'ctx Context,
	module: &'ctx Module<'ctx>,
	builder: Builder<'ctx>,
	functions: Rc<RefCell<Vec<Rc<RefCell<Function<'ctx>>>>>>,
}

impl<'ctx> GenInfo<'ctx> {
	fn find_any_type(&self, type_name: &str) -> Result<AnyTypeEnum<'ctx>> {
		match type_name {
			"void" => Ok(AnyTypeEnum::VoidType(self.ctx.void_type())),
			"i32" => Ok(AnyTypeEnum::IntType(self.ctx.i32_type())),
			_ => Err(anyhow::anyhow!("Uknown type '{}'", type_name)),
		}
	}

	fn find_basic_type(&self, type_name: &str) -> Result<BasicTypeEnum<'ctx>> {
		match type_name {
			"i32" => Ok(BasicTypeEnum::IntType(self.ctx.i32_type())),
			_ => Err(anyhow::anyhow!("Uknown type '{}'", type_name)),
		}
	}

	fn let_var(&self, var: LocalVariable<'ctx>) -> Result<()> {
		let functions = self.functions.borrow();
		let last_function = functions.last().context("let_var(...): Function empty")?.borrow();
		let mut last_block = last_function.blocks.last().context("let_var(...): Blocks empty")?.borrow_mut();
		let vars = &mut last_block.vars;

		if vars.iter().find(|v| v.name == var.name).is_some() {
			return Err(anyhow::anyhow!("Variable already declared: {}", var.name));
		}

		vars.push(var);

		return Ok(());
	}

	fn get_var(&self, name: &str) -> Result<LocalVariableValue<'ctx>> {
		let functions = self.functions.borrow();
		let last_function = functions.last().context("let_var(...): Function empty")?.borrow();
		let mut last_block = last_function.blocks.last().context("let_var(...): Blocks empty")?.borrow_mut();
		let vars = &mut last_block.vars;

		if let Some(var) = vars.iter().find(|v| v.name == *name) {
			return Ok(var.value.clone());
		}

		return Err(anyhow::anyhow!("get_var(...): Can't find variable with name '{name}"));
	}
}

fn try_fn_type<'ctx>(a: AnyTypeEnum<'ctx>, param_types: &[BasicMetadataTypeEnum<'ctx>], is_var_args: bool) -> Result<FunctionType<'ctx>> {
	match a {
		AnyTypeEnum::ArrayType(t) => Ok(t.fn_type(param_types, is_var_args)),
		AnyTypeEnum::FloatType(t) => Ok(t.fn_type(param_types, is_var_args)),
		AnyTypeEnum::FunctionType(_) => Err(anyhow::anyhow!("try_fn_type(...): Can't use FunctionType as return type")),
		AnyTypeEnum::IntType(t) => Ok(t.fn_type(param_types, is_var_args)),
		AnyTypeEnum::PointerType(t) => Ok(t.fn_type(param_types, is_var_args)),
		AnyTypeEnum::StructType(t) => Ok(t.fn_type(param_types, is_var_args)),
		AnyTypeEnum::VectorType(t) => Ok(t.fn_type(param_types, is_var_args)),
		AnyTypeEnum::VoidType(t) => Ok(t.fn_type(param_types, is_var_args)),
	}
}

fn generate_expr<'ctx>(gen: &GenInfo<'ctx>, expr: &frontend::Expr) -> Result<BasicValueEnum<'ctx>> {
	match expr {
		frontend::Expr::Add { left, right } => {
			let left_result = generate_expr(gen, left)?;
			let right_result = generate_expr(gen, right)?;

			match left_result {
				BasicValueEnum::IntValue(left_int) => {
					match right_result {
						BasicValueEnum::IntValue(right_int) => {
							let left_bit_width = left_int.get_type().get_bit_width();
							let right_bit_width = right_int.get_type().get_bit_width();

							if left_bit_width == right_bit_width {
								let val = gen.builder.build_int_add(left_int , right_int, "")?;
								return Ok(BasicValueEnum::IntValue(val));
							} else {
								panic!("backend::generate_expr(...): Different bit size {left_bit_width} != {right_bit_width}");
							}
						},
						_ => {
							return Err(anyhow::anyhow!("backend::generate_expr(...): Unhandled Number type 5 {right_result:?}"));
						},
					}
				},
				_ => {
					return Err(anyhow::anyhow!("backend::generate_expr(...): Unhandled Number type 3 {left_result:?}"));
				},
			}
		},
		frontend::Expr::Token { id } => {
			let var = gen.get_var(id)?;
			match var.value_type {
				BasicTypeEnum::IntType(t) => {
					let val = gen.builder.build_load(t, var.ptr, "")?;
					return Ok(val);
				},
				_ => {
					return Err(anyhow::anyhow!("backend::generate_expr(...): Token: Unhandled type {:?}", var.value_type));
				}
			}
		},
		frontend::Expr::Number { value } => {
			match value {
				frontend::Number::Int(n) => {
					match n {
						frontend::NumberInt::Any(v) => {
							let i32_type = gen.ctx.i32_type();
							let ptr = gen.builder.build_alloca(i32_type, "")?;
							gen.builder.build_store(ptr, i32_type.const_int(*v as u64, *v < 0))?;
							let val = gen.builder.build_load(i32_type, ptr, "")?;
							return Ok(val);
						},
						_ => {
							return Err(anyhow::anyhow!("backend::generate_expr(...): Unhandled Number type 1 {n:?}"));
						},
					}
				},
				_ => {
					return Err(anyhow::anyhow!("backend::generate_expr(...): Unhandled Number type 2 {value:?}"));
				},
			}
		},
		frontend::Expr::Function { params, ret_type, code } => {
			let ret_type = gen.find_any_type(&ret_type.name)?;
			let fn_type = try_fn_type(ret_type, &params.iter().map(|param| gen.find_basic_type(&param.param_type.name).ok().map(|r| r.into())).collect::<Option<Vec<_>>>().with_context(|| format!("backend::generate_expr(...): Function: Can't find param types: {params:?}"))?, false)?;
			let function = gen.module.add_function("", fn_type, Some(Linkage::Internal));
			let entry_block = gen.ctx.append_basic_block(function, "");

			{
				gen.builder.position_at_end(entry_block);
				let _guard_builder_block = scopeguard::guard(gen.functions.borrow().last().unwrap().borrow().blocks.last().unwrap().borrow().block, |prev_block| {
					gen.builder.position_at_end(prev_block);
				});

				gen.functions.borrow_mut().push(Rc::new(RefCell::new(Function {
					_func: function,
					blocks: Vec::new(),
				})));
				let _guard_last_function = scopeguard::guard(gen.functions.clone(), |functions| {
					functions.borrow_mut().pop();
				});

				gen.functions.borrow().last().unwrap().borrow_mut().blocks.push(Rc::new(RefCell::new(Block {
					block: entry_block,
					vars: Vec::new(),
				})));
				let _guard_last_block = scopeguard::guard(gen.functions.borrow().last().unwrap().clone(), |last_function| {
					last_function.borrow_mut().blocks.pop();
				});

				for (param, val) in params.iter().zip(function.get_param_iter()) {
					let val_type = gen.find_basic_type(&param.param_type.name)?;
					let ptr = gen.builder.build_alloca(val_type, "")?;
					gen.builder.build_store(ptr, val)?;

					gen.let_var(LocalVariable {
						name: param.name.clone(),
						value: LocalVariableValue {
							ptr,
							value_type: val_type,
						}
					})?;
				}

				for stmt in &code.stmts {
					generate_stmt(gen, stmt)?;
				}

				if let Some(expr) = &code.ret_expr {
					let expr_result = generate_expr(gen, expr)?;
					gen.builder.build_return(Some(&expr_result))?;
				}
			}

			let ptr_type = gen.ctx.ptr_type(inkwell::AddressSpace::default());
			let ptr = gen.builder.build_alloca(ptr_type, "")?;
			gen.builder.build_store(ptr, function.as_global_value())?;
			let val = gen.builder.build_load(ptr_type, ptr, "")?.into_pointer_value();
			return Ok(BasicValueEnum::PointerValue(val));
		},
		_ => {
			return Err(anyhow::anyhow!("backend::generate_expr(...): Unhandled expression type {expr:?}"));
		},
	}
}

fn generate_stmt(gen: &GenInfo, stmt: &frontend::Stmt) -> Result<()> {
	match stmt {
		frontend::Stmt::Let { name, expr } => {
			let expr_result = generate_expr(gen, expr)?;

			let val_type = expr_result.get_type();
			let ptr = gen.builder.build_alloca(val_type, "")?;
			gen.builder.build_store(ptr, expr_result)?;
			gen.let_var(LocalVariable {
				name: name.clone(),
				value: LocalVariableValue {
					ptr,
					value_type: val_type.into(),
				}
			})?;

			return Ok(());
		},
		frontend::Stmt::Ret { expr: maybe_expr } => {
			if let Some(expr) = maybe_expr {
				let expr_result = generate_expr(gen, expr)?;
				gen.builder.build_return(Some(&expr_result))?;
			} else {
				gen.builder.build_return(None)?;
			}

			return Ok(());
		},
	}
}

fn generate_root(gen: &GenInfo, root: &frontend::Root) -> Result<()> {
	let init_fn_type = gen.ctx.void_type().fn_type(&[], false);
	let init_function = gen.module.add_function(&format!("$init_{}", gen.module.get_name().to_str()?), init_fn_type, None);
	let init_entry_block = gen.ctx.append_basic_block(init_function, "");

	gen.builder.position_at_end(init_entry_block);

	gen.functions.borrow_mut().push(Rc::new(RefCell::new(Function {
		_func: init_function,
		blocks: Vec::new(),
	})));
	let _guard_last_function = scopeguard::guard(gen.functions.clone(), |functions| {
		functions.borrow_mut().pop();
	});

	gen.functions.borrow().last().unwrap().borrow_mut().blocks.push(Rc::new(RefCell::new(Block {
		block: init_entry_block,
		vars: Vec::new(),
	})));
	let _guard_last_block = scopeguard::guard(gen.functions.borrow().last().unwrap().clone(), |last_function| {
		last_function.borrow_mut().blocks.pop();
	});

	for stmt in &root.stmts {
		generate_stmt(gen, stmt)?;
	}

	gen.builder.build_return(None)?;

	return Ok(());
}

pub fn generate(module_name: &str, root: &frontend::Root) -> Result<IntermediateCode> {
	let ic = IntermediateCode::new(module_name);

	{
		let builder = ic.ctx.create_builder();

		let gen = GenInfo {
			ctx: ic.ctx,
			module: ic.module,
			builder: builder,
			functions: Rc::new(RefCell::new(Vec::new())),
		};

		generate_root(&gen, root)?;
	}

	return Ok(ic);
}


pub fn generate_prologue(module_name: &str, inits: &[String]) -> Result<IntermediateCode> {
	let ic = IntermediateCode::new(module_name);

	{
		let builder = ic.ctx.create_builder();
		let start_fn_type = ic.ctx.void_type().fn_type(&[], false);
		let start_function = ic.module.add_function("$start", start_fn_type, None);
		let start_block = ic.ctx.append_basic_block(start_function, "");

		builder.position_at_end(start_block);

		for init in inits {
			let init_func = ic.module.add_function(init, start_fn_type, Some(Linkage::External));
			builder.build_direct_call(init_func, &[], "")?;
		}

		builder.build_return(None)?;
	}

	return Ok(ic);
}
