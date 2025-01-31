use anyhow::{Context, Result};

use super::parser;
use super::frontend;
use super::backend;

#[test]
fn compile_test() -> Result<()> {
	let test_source = std::fs::read_to_string("src/tests/test.vas")?;
	let parse_result = parser::parse_file(&test_source).context("Can't parse file")?;

	let root = frontend::parse(&parse_result.tok)?;
	let code = backend::generate("test", &root)?;

	std::fs::write("target/test.ll", code.to_intermediate()?)?;
	std::fs::write("target/test.asm", code.to_asm()?)?;
	std::fs::write("target/test.obj", code.to_obj()?)?;

	return Ok(());
}
