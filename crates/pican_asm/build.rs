use std::{
    env::temp_dir,
    fmt::Write,
    path::{Path, PathBuf},
};

use glob::glob;

struct DelPathOnDrop<'p> {
    p: &'p Path,
}
impl Drop for DelPathOnDrop<'_> {
    fn drop(&mut self) {
        if self.p.exists() {
            std::fs::remove_file(self.p).unwrap();
        }
    }
}

fn main() {
    println!("cargo:rerun-if-changed=../../test_cases/inputs");
    let inputs = glob("../../test_cases/inputs/*.pica").unwrap();
    let outdir_s = std::env::var_os("OUT_DIR").unwrap();
    let outdir = Path::new(&outdir_s);
    let binaries: Vec<_> = inputs
        .map(|i| i.unwrap())
        .map(|i| -> anyhow::Result<Option<(String, PathBuf)>> {
            let case_name = i
                .file_stem()
                .unwrap()
                .to_string_lossy()
                .into_owned()
                .replace('-', "__");
            let outfile = outdir.join(&case_name).with_extension("shbin");
            //let _guard = DelPathOnDrop { p: &outfile };
            let status = std::process::Command::new("picasso")
                .arg(i)
                .arg("-o")
                .arg(&outfile)
                .spawn()?
                .wait()?;
            if !status.success() {
                Ok(None)
            } else {
                Ok(Some((case_name, outfile)))
            }
            //let binary = std::fs::read(&outfile)?;
        })
        .collect::<Result<_, _>>()
        .unwrap();
    let cases = binaries
        .into_iter()
        .flatten()
        .fold(String::new(), |mut s, (name, byte_file)| {
            write!(
                &mut s,
                r#"
        #[test]
        fn picasso_roundtrip_{name}() {{
            let input = include_bytes!("{byte_file}");
            let bin: Shbin = std::io::Cursor::new(input).read_le().unwrap();

            let mut w = Cursor::new(Vec::new());
            bin.write_le(&mut w).unwrap();
            let inner = w.into_inner();
            let roundtrip: Shbin = Cursor::new(&inner).read_le().unwrap();
            assert_eq!(bin, roundtrip);
            assert_eq!(input, inner.as_slice());
        }}
        "#,
                byte_file = byte_file.to_string_lossy(),
            )
            .unwrap();
            s
        });
    std::fs::write(outdir.join("picasso_match_tests.rs"), &cases).unwrap();
}
