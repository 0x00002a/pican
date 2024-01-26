use std::path::Path;

use glob::glob;

fn mk_test(prog_file: &Path, typecheck_fail: bool) -> String {
    format!(
        r#"
#[test]
fn {name}_shouldnt_typecheck_{typecheck_fail}() {{
    let mut ctx = PicanContext::new();
    let input_id = ctx.add_file(
        "{path}",
        std::fs::read_to_string("{path}").expect("failed to read input"),
    );

    let pir_ctx = IrContext::new();
    let pir = pican_frontend::parse_and_lower(input_id, &ctx, &pir_ctx);

    let Some(pir) = pir else {{
        super::print_diagnostics(&ctx.diag.as_codespan(), &ctx.files);
        panic!("failed to produce PIR");
    }};
    let tycheck = ctx.types_for_module(&pir);
    let ty_success = tycheck.check().is_ok();
    let should_fail = if {typecheck_fail} {{ ty_success }} else {{ !ty_success }};
    if should_fail {{
        super::print_diagnostics(&ctx.diag.as_codespan(), &ctx.files);
        panic!("typecheck failed/passed incorrectly");
    }}
}}
"#,
        path = prog_file.display(),
        name = prog_file
            .file_stem()
            .unwrap()
            .to_string_lossy()
            .replace([' '], "_space_")
            .replace('-', "_dash_")
            .replace('.', "_dot_"),
    )
}

fn main() {
    let test_root = std::env::var_os("OUT_DIR").unwrap();
    let test_root = std::path::Path::new(&test_root);
    let output_file = test_root.join("all_programs_compile.rs");
    let programs = glob("inputs/*.pica").expect("failed to read glob");
    let typecheck_fail = glob("tycheck_fail/*.pica").expect("failed to read typecheck fail glob");
    println!("cargo:rerun-if-changed=../inputs");
    println!("cargo:rerun-if-changed=../tycheck_fail");
    let cases = programs
        .map(|p| (p, false))
        .chain(typecheck_fail.map(|p| (p, true)))
        .map(|(p, c)| (p.expect("failed to get path to program file"), c))
        .map(|(p, c)| mk_test(&p, c))
        .collect::<Vec<_>>()
        .join("\n");
    std::fs::write(output_file, cases).expect("failed to write out programs");
}
