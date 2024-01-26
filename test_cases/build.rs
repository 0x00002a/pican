use std::{io::Write, path::Path};

use glob::glob;
use quote::format_ident;

fn mk_picasso_conformance(prog_file: &Path, name: &String, bin_path: &Path) -> String {
    let test_name = format_ident!("picasso_conformance_{name}");
    let bin_path = bin_path.to_string_lossy();
    let prog_file = prog_file.to_string_lossy();
    quote::quote! {
        #[test]
        fn #test_name () {
            let input = include_bytes!(#bin_path);
            let picasso_bin: pican_asm::shbin::Shbin = std::io::Cursor::new(&input).read_le().unwrap();
            let pican_input = run_pican(::std::path::Path::new(#prog_file));
            let pican_bin: pican_asm::shbin::Shbin = std::io::Cursor::new(&pican_input).read_le().unwrap();

            assert_eq!(picasso_bin, pican_bin);
            assert_eq!(&input, &pican_input.as_slice());

            /*println!("bin: {{bin:#?}}");
            println!("-- instructions --\n{{}}", bin.dvlp.compiled_blob.iter().map(|i| format!("{{}}\n", i.to_asm(&bin.dvlp.operand_desc_table.data))).collect::<String>());

            let mut w = Cursor::new(Vec::new());
            bin.write_le(&mut w).unwrap();
            let inner = w.into_inner();
            let roundtrip: Shbin = Cursor::new(&inner).read_le().unwrap();
            assert_eq!(bin, roundtrip);
            assert_eq!(input, inner.as_slice());*/
        }
    }
    .to_string()
}

fn generate_picasso_tests() {
    println!("cargo:rerun-if-changed=../../test_cases/inputs");
    let inputs = glob("./inputs/*.pica").unwrap();
    let outdir_s = std::env::var_os("OUT_DIR").unwrap();
    let outdir = Path::new(&outdir_s);

    let mut roundtrips_file = std::fs::File::create(outdir.join("picasso_match_tests.rs")).unwrap();
    let mut conformance_file =
        std::fs::File::create(outdir.join("picasso_conformance_tests.rs")).unwrap();
    for i in inputs.map(|i| i.unwrap()) {
        let case_name = fmt_testname(&i);
        let outfile = outdir.join(&case_name).with_extension("shbin");
        //let _guard = DelPathOnDrop { p: &outfile };
        let status = std::process::Command::new("picasso")
            .arg(&i)
            .arg("-o")
            .arg(&outfile)
            .spawn()
            .unwrap()
            .wait()
            .unwrap();
        if !status.success() {
            continue;
        }
        let byte_file = &outfile;
        let name = &case_name;

        write!(
                &mut roundtrips_file, "{}",
            {
                let test_name = format_ident!("picasso_roundtrip_{name}");
                let byte_file = byte_file.to_string_lossy();
                quote::quote! {
        #[test]
        fn #test_name () {
            let input = include_bytes!(#byte_file);
            let bin: Shbin = std::io::Cursor::new(input).read_le().unwrap();
            println!("bin: {{bin:#?}}");
            println!("-- instructions --\n{}", bin.dvlp.compiled_blob.iter().map(|i| format!("{}\n", i.to_asm(&bin.dvlp.operand_desc_table.data))).collect::<String>());

            let mut w = Cursor::new(Vec::new());
            bin.write_le(&mut w).unwrap();
            let inner = w.into_inner();
            let roundtrip: Shbin = Cursor::new(&inner).read_le().unwrap();
            assert_eq!(bin, roundtrip);
            assert_eq!(input, inner.as_slice());
        }
                }
            }
            )
            .unwrap();
        write!(
            &mut conformance_file,
            "{}",
            mk_picasso_conformance(&i.canonicalize().unwrap(), name, &outfile)
        )
        .unwrap();
    }
}

fn fmt_testname(prog_file: &Path) -> String {
    prog_file
        .file_stem()
        .unwrap()
        .to_string_lossy()
        .replace([' '], "_space_")
        .replace('-', "_dash_")
        .replace('.', "_dot_")
}

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
        name = fmt_testname(prog_file),
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
    generate_picasso_tests();
}
