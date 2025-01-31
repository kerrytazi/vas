mod parser;
mod frontend;
mod backend;

#[cfg(test)]
mod tests;

use anyhow::{Context, Result};
use clap::{Parser, ValueEnum};

#[derive(Parser)]
struct Cli {
	#[arg(long="output", short='o', value_name="file")]
	output: std::path::PathBuf,

	#[arg(long="output-type", value_name="type", default_value="binary")]
	output_type: OutputType,

	#[arg(long="cache-dir", default_value=".")]
	cache_dir: std::path::PathBuf,

	#[arg(value_name="input_files", required=true)]
	input_files: Vec<std::path::PathBuf>,
}

#[derive(ValueEnum, Clone, PartialEq)]
enum OutputType {
	Binary,
	Object,
	Asm,
	LLVM,
}

fn main() -> Result<()> {
	let cli = Cli::parse();

	if cli.output_type != OutputType::Binary {
		return Err(anyhow::anyhow!("Currently output-type not supported"));
	}

	let cache_dir = if cli.cache_dir.is_absolute() {
		cli.cache_dir
	} else {
		std::env::current_dir()?.join(cli.cache_dir)
	};

	std::fs::create_dir_all(&cache_dir)?;

	let output_file = if cli.output.is_absolute() {
		cli.output
	} else {
		std::env::current_dir()?.join(cli.output)
	};

	{
		let mut output_dir = output_file.clone();
		output_dir.pop();
		std::fs::create_dir_all(output_dir)?;
	}

	let mut objs = Vec::new();

	for input_file in &cli.input_files {
		let input_path = input_file;
		let input_file_name = input_path.file_stem().with_context(|| format!("Invalid filename '{:?}'", input_path))?.to_str().with_context(|| format!("Can't convert OsStr to str: '{:?}'", input_path))?.to_owned();
		let source = std::fs::read_to_string(input_path)?;
		let parse_result = parser::parse_file(&source).with_context(|| format!("Can't parse file {}", input_file.to_str().unwrap_or("<Error: Can't read file name>")))?;

		let root = frontend::parse(&parse_result.tok)?;
		let code = backend::generate(&input_file_name, &root)?;

		let obj_path = cache_dir.join(input_file_name + ".obj");
		std::fs::write(&obj_path, code.to_obj()?)?;

		objs.push(obj_path);
	}

	// TODO
	let toolchain_path = std::path::PathBuf::from(r"C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.42.34433\bin\Hostx64\x64\");
	let linker_path = toolchain_path.join("link.exe");

	let res = std::process::Command::new(linker_path.as_os_str()).arg("/NOLOGO")
		.arg("/ENTRY:$start")
		.arg("/SUBSYSTEM:CONSOLE")
		.arg("/MACHINE:x64")
		.arg(format!("/OUT:{}", output_file.as_os_str().to_str().with_context(|| format!("Invalid filename '{:?}'", output_file))?))
		.args(objs)
		.output()?;

	if !res.status.success() {
		println!("link failed");
		println!("link status: {}", res.status);
		println!("link stdout: '{}'", String::from_utf8(res.stdout)?);
		println!("link stderr: '{}'", String::from_utf8(res.stderr)?);
	}

	return Ok(());
}
