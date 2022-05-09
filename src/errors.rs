use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use pest::Span;

use crate::HIR::FileSpan;

pub fn experimental_function_resolution_var_missing<'s>(
    var_span: FileSpan<'s>,
    def_span: FileSpan<'s>,
) {
    let mut files = SimpleFiles::new();
    let var_file = files.add("main", var_span.text);
    let def_file = files.add("main", def_span.text);

    let diagnostic = Diagnostic::error()
        .with_message("variable not found")
        .with_labels(vec![
            Label::primary(var_file, var_span.span.start()..var_span.span.end())
                .with_message("we thought this was a variable"),
            Label::secondary(def_file, def_span.span.start()..def_span.span.end())
                .with_message("because this function call matches the function"),
            Label::secondary(def_file, def_span.span.start()..def_span.span.end())
                .with_message("but no variable with this name was found in scope"),
        ]);

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
}

pub fn experimental_function_resolution_ambiguous<'s>(
    call_span: FileSpan<'s>,
    def_spans: impl Iterator<Item = FileSpan<'s>>,
) {
    let mut files = SimpleFiles::new();
    let file_id = files.add("program", call_span.text);

    let mut labels = vec![
        Label::primary(file_id, call_span.span.start()..call_span.span.end())
            .with_message("this function call could refer to multiple definitions "),
    ];

    let def_spans = def_spans.collect::<Vec<_>>();

    let names: Vec<String> = (0..def_spans.len())
        .map(|i| i.to_string())
        .collect::<Vec<_>>();

    for (i, (span, name)) in def_spans.iter().zip(names.iter()).enumerate() {
        labels.push(
            Label::secondary(
                files.add(&name, span.text),
                span.span.start()..span.span.end(),
            )
            .with_message("this is a possible choice"),
        )
    }

    let diagnostic = Diagnostic::error()
        .with_message("ambiguous function call")
        .with_labels(labels)
        .with_notes(vec![
            "note: put parameters around variables in the function call so we know which is correct"
                .to_string(),
        ]);

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
}

pub fn experimental_function_resolution_fail<'s>(call_span: FileSpan<'s>) {
    let mut files = SimpleFiles::new();
    let file_id = files.add("program", call_span.text);

    let diagnostic = Diagnostic::error()
        .with_message("no function found")
        .with_labels(vec![Label::primary(
            file_id,
            call_span.span.start()..call_span.span.end(),
        )
        .with_message("we couldn't find any function matching this call")]);

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
}
