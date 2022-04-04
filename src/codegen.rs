use std::{fs::File, io::Write};

use xshell::{cmd, Shell};

static C_LIBARY: &'static str = include_str!("remix.c");

pub fn compile_c(program: &str) -> Result<(), Box<dyn std::error::Error>> {
    eprintln!("compile start");
    let mut file = File::create("transpiled.c")?;
    file.write_all(program.as_bytes())?;

    let sh = Shell::new()?;
    cmd!(sh, "'C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/VC/Auxiliary/Build/vcvars64.bat'").run()?;
    cmd!(sh, "clang transpiled.c -o built.exe").run()?;

    eprintln!("compile end");
    Ok(())
}

mod emit {
    fn emit_signature();
    fn emit_statement();

    fn emit_function() {
        emit_signature();
        for statement in statements {
            emit_statement();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_c() {
        compile_c(
            r#"
            #include <stdio.h>
            int main() {
                printf("Hello world\n");
                return 0;
            }
        "#,
        )
        .expect("failed to compile c");
    }
}
