mod parser;
mod frontend;
mod backend;
mod cliargs;

#[cfg(test)]
mod tests;

use anyhow::{Context, Result};
use clap::Parser;

fn make_absolute(path: std::path::PathBuf) -> Result<std::path::PathBuf> {
	if path.is_absolute() {
		Ok(path)
	} else {
		Ok(std::env::current_dir()?.join(path))
	}
}

fn main() -> Result<()> {
	let cli = cliargs::Cli::try_parse()?;

	if cli.output_type != cliargs::OutputType::Binary {
		return Err(anyhow::anyhow!("Currently output-type not supported"));
	}

	if cli.toolchain_type != cliargs::ToolchainType::MSVC {
		return Err(anyhow::anyhow!("Currently toolchain-type not supported"));
	}

	let cache_dir = if let Some(path) = cli.cache_dir {
		make_absolute(path)?
	} else {
		std::env::current_dir()?
	};

	std::fs::create_dir_all(&cache_dir)?;

	let output_file = make_absolute(cli.output_path.unwrap_or_else(|| std::path::PathBuf::from("a.exe")))?;

	{
		let mut output_dir = output_file.clone();
		output_dir.pop();
		std::fs::create_dir_all(output_dir)?;
	}

	let mut objs = Vec::new();
	let mut inits = Vec::new();

	let mut entry_name = "$start".to_owned();

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
		inits.push(code.get_init_fn_name());
	}

	// if prologue
	{
		let code = backend::generate_prologue("__prologue", &inits)?;
		let obj_path = cache_dir.join("__prologue.obj");
		std::fs::write(&obj_path, code.to_obj()?)?;

		objs.push(obj_path);

		entry_name = code.get_init_fn_name();
	}

	match cli.toolchain_type {
		cliargs::ToolchainType::MSVC => {
			let toolchain_path = if let Some(path) = cli.toolchain_path {
				make_absolute(path)?
			} else {
				// TODO
				std::path::PathBuf::from(r"C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.42.34433\bin\Hostx64\x64\")
			};

			let linker_path = toolchain_path.join("link.exe");

			let res = std::process::Command::new(linker_path.as_os_str())
				.arg("/NOLOGO")
				.arg(format!("/ENTRY:{}", entry_name))
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

				return Err(anyhow::anyhow!("Linker Error"));
			}
		},
		_ => {
			return Err(anyhow::anyhow!("Toolchain type '{:?}' not supported", cli.toolchain_type));
		}
	};

	return Ok(());
}
