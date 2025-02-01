use clap::{Parser, ValueEnum};

fn gen_help() -> &'static str {
r#"Usage:
  {usage}

Examples:
  vas -o output.exe source1.vas source2.vas
  vas --output-path output.exe --cache-dir path/to/build path/to/source.vas
  vas --output llvm output.ll source.vas

Options:
{options}"#
}

#[derive(Parser)]
#[command(help_template=gen_help())]
//#[command(verbatim_doc_comment, long_about=examples())]
pub struct Cli {
	#[arg(long="output-type", value_name="type", default_value="binary")]
	pub output_type: OutputType,

	#[arg(long="output-path", short='o', value_name="path")]
	pub output_path: Option<std::path::PathBuf>,

	#[arg(long="toolchain-type", value_name="type", default_value="msvc")]
	pub toolchain_type: ToolchainType,

	#[arg(long="toolchain-path", value_name="path")]
	pub toolchain_path: Option<std::path::PathBuf>,

	/// [default: <workdir>]
	#[arg(long="cache-dir", value_name="path")]
	pub cache_dir: Option<std::path::PathBuf>,

	#[arg(value_name="input_files", required=true)]
	pub input_files: Vec<std::path::PathBuf>,
}

#[derive(ValueEnum, Clone, PartialEq, Debug)]
pub enum ToolchainType {
	MSVC,
	GNU,
}

#[derive(ValueEnum, Clone, PartialEq, Debug)]
pub enum OutputType {
	Binary,
	Object,
	Asm,
	LLVM,
}
