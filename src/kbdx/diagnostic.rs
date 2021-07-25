use codespan_reporting::diagnostic::{Diagnostic as CodespanDiagnostic, *};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use std::ops::Range;

use pest::Span;

type Source<'a> = &'a str;

/// Enum listing the the different exit statuses.
///
/// [ExitStatus] implements [Into<usize>], so it can be converted to a process exit code
#[derive(Debug)]
pub enum ExitStatus {
    Success,
    Failure,
}

impl Into<usize> for ExitStatus {
    /// Returns the exit code associated with the status
    fn into(self) -> usize {
        use ExitStatus::*;

        match self {
            Success => 0,
            Failure => 1,
        }
    }
}

/// Contains all diagnostics generated during the program.
///
/// Diagnostics are added through [FileDiagnostics], which allow you to add messages pertaining to
/// a single file at a time.
#[derive(Debug)]
pub struct DiagnosticAggregator<'a> {
    file_db: SimpleFiles<String, Source<'a>>,
    errors: Vec<Diagnostic>,
    warnings: Vec<Diagnostic>,
}

impl<'a> Default for DiagnosticAggregator<'a> {
    fn default() -> DiagnosticAggregator<'a> {
        DiagnosticAggregator {
            file_db: SimpleFiles::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }
}

impl<'a> DiagnosticAggregator<'a> {
    /// Create a [FileDiagnostics] handle.
    ///
    /// Note that because this function takes a mutable reference to `self`, only one
    /// [FileDiagnostics] instance may safely exist at a time
    pub fn new_file<'b>(
        &'b mut self,
        file_name: String,
        file_source: &'a str,
    ) -> FileDiagnostics<'b, 'a> {
        let file_id = self.file_db.add(file_name, file_source);
        FileDiagnostics::new(file_id, self)
    }

    /// Prints all of the diagnostics to stderr and returns the exit code of the program, or
    /// a reason why the printing failed
    pub fn emit_all(self) -> Result<ExitStatus, codespan_reporting::files::Error> {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        let mut writer_lock = writer.lock();

        // helper macro for emitting a single diagnostic
        macro_rules! emit {
            ($diagnostic:expr) => {{
                codespan_reporting::term::emit(
                    &mut writer_lock,
                    &config,
                    &self.file_db,
                    &$diagnostic,
                )?;
            }};
        }

        let exit_code = if self.errors.len() == 0 {
            ExitStatus::Success
        } else {
            ExitStatus::Failure
        };

        for warning in self.warnings {
            let codespan_warning = CodespanDiagnostic::warning()
                .with_message(warning.headline)
                .with_labels(warning.messages)
                .with_notes(warning.notes);

            emit!(codespan_warning)
        }

        for error in self.errors {
            let codespan_error = CodespanDiagnostic::error()
                .with_message(error.headline)
                .with_labels(error.messages)
                .with_notes(error.notes);

            emit!(codespan_error)
        }

        Ok(exit_code)
    }
}

// I was having lifetime issues when I was doing
// &'a mut DiagnosticAggregator<'a>
//
// After reading through the Nomicon, I learned that shared references (&'a T) are COVARIANT over T while
// mutable references are INVARIANT over T
//
// This means that
// &'a mut DiagnosticAggregator<'a>
// will NOT pass for
// &'a mut DiagnosticAggregator<'b>,
// but
// &'a DiagnosticAggregator<'a>
// COULD pass for
// &'a DiagnosticAggregator<'b>
//
/// A single-file view into [DiagnosticAggregator].
#[derive(Debug)]
pub struct FileDiagnostics<'a, 'b> {
    file_id: usize,
    diagnostic_aggregator: &'a mut DiagnosticAggregator<'b>,
    warning_count: usize,
    error_count: usize
}

impl<'a, 'b> FileDiagnostics<'a, 'b> {
    /// Creates a new [FileDiagnostics] from the [SimpleFiles] database's `file_id` and a mutable
    /// reference to the [DiagnosticAggregator]
    fn new(
        file_id: usize,
        diagnostic_aggregator: &'a mut DiagnosticAggregator<'b>,
    ) -> FileDiagnostics<'a, 'b> {
        FileDiagnostics {
            file_id,
            diagnostic_aggregator,
            warning_count: 0,
            error_count: 0
        }
    }

    /// Creates an empty [Diagnostic] with a given headline/main message
    fn with_headline(&self, headline: impl Into<String>) -> Diagnostic {
        Diagnostic {
            file_id: self.file_id,
            headline: headline.into(),
            messages: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Creates an `error` [Diagnostic] in the underlying [DiagnosticAggregator] and returns
    /// a mutable reference to it
    pub fn error<'ret>(&'ret mut self, headline: impl Into<String>) -> &'ret mut Diagnostic {
        let diagnostic = self.with_headline(headline.into());
        self.diagnostic_aggregator.errors.push(diagnostic);
        self.error_count += 1;
        self.diagnostic_aggregator.errors.last_mut().unwrap()
    }

    /// Creates a `warning` [Diagnostic] in the underlying [DiagnosticAggregator] and returns
    /// a mutable reference to it
    pub fn warning<'ret>(&'ret mut self, headline: impl Into<String>) -> &'ret mut Diagnostic {
        let diagnostic = self.with_headline(headline.into());
        self.diagnostic_aggregator.warnings.push(diagnostic);
        self.warning_count += 1;
        self.diagnostic_aggregator.warnings.last_mut().unwrap()
    }

    pub fn error_count(&self) -> usize {
        self.error_count
    }

    pub fn warning_count(&self) -> usize {
        self.warning_count
    }
}

/// A single self-contained diagnostic containing a main headline, messages annotating the input
/// and notes that follow it
#[derive(Debug)]
pub struct Diagnostic {
    file_id: usize,
    headline: String,
    messages: Vec<Label<usize>>,
    notes: Vec<String>,
}

impl Diagnostic {
    /// Adds a [Message] to the [Diagnostic].
    ///
    /// "Messages" are meant for annotating source code
    /// The [Message]'s are stored in a list and are displayed in the order of insertion
    pub fn add_message(&mut self, message: Message) -> &mut Self {
        let Message {
            byte_range,
            text: message,
        } = message;

        let label = if self.messages.is_empty() {
            Label::primary(self.file_id, byte_range)
        } else {
            Label::secondary(self.file_id, byte_range)
        };

        self.messages.push(
            if let Some(message) = message {
                label.with_message(message)
            }
            else {
                label
            }
        );

        self
    }

    /// Adds a note to the [Diagnostic].
    ///
    /// "Notes" are meant for explanations that are not tied to any specific part of the input
    /// Like with [Self::add_message], the notes are stored in a list and are displayed in the order of insertion
    pub fn add_note(&mut self, note: impl Into<String>) -> &mut Self {
        self.notes.push(note.into());
        self
    }
}

/// A single annotation on a byte range of the input
pub struct Message {
    pub byte_range: Range<usize>,
    pub text: Option<String>,
}

impl Message {
    /// Creates a [Message] annotating a Pest Span with `text`.
    ///
    /// This function is a convenience function for [Self::from_byte_range] and does not do any
    /// extra processing
    pub fn from_pest_span(span: &Span<'_>, text: impl Into<String>) -> Message {
        Self::from_byte_range(span.start()..span.end(), text)
    }

    /// Creates a [Message] from a range of bytes.
    ///
    /// To annotate `hello world` within the string `12345hello world54321`, the range would be `5..17`.
    /// Note that the range has nothing to do with lines and is only an offset from the start of
    /// the input (the file being annotated)
    pub fn from_byte_range(byte_range: Range<usize>, message: impl Into<String>) -> Message {
        Message {
            byte_range,
            text: Some(message.into()),
        }
    }

    /// Creates a [Message] annotating a Pest Span
    ///
    /// This function is a convenience function for [Self::from_byte_range_no_text] and does not do any
    /// extra processing
    pub fn from_pest_span_no_text(span: &Span<'_>) -> Message {
        Self::from_byte_range_no_text(span.start()..span.end())
    }

    /// Creates a [Message] from a range of bytes.
    ///
    /// See [Self::from_byte_range] for further documentation
    pub fn from_byte_range_no_text(byte_range: Range<usize>) -> Message {
        Message {
            byte_range,
            text: None,
        }
    }
}
