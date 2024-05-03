use codespan_reporting::diagnostic::{Diagnostic as CodespanDiagnostic, *};
use codespan_reporting::files::{Error, Files, SimpleFile};
use codespan_reporting::term::termcolor::{Buffer, ColorChoice, StandardStream, WriteColor};
use codespan_reporting::term::Config;

use std::ops::Range;

use pest::Span;

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

/// Re-export codespan reporting's error type for errors relating to the Diagnostics aggregation
/// itself.
pub use codespan_reporting::files::Error as MetaError;

/// Contains all diagnostics generated during the program.
///
/// Diagnostics are added through [FileDiagnostics], which allow you to add messages pertaining to
/// a single file at a time.
#[derive(Debug)]
pub struct DiagnosticAggregator<'a> {
    file_db: Vec<FileDiagnostics<'a>>,
}

impl<'a> Default for DiagnosticAggregator<'a> {
    fn default() -> DiagnosticAggregator<'a> {
        DiagnosticAggregator {
            file_db: Vec::new(),
        }
    }
}

type FileId = usize;
type FileName<'a> = &'a str;
type FileSource<'a> = &'a str;

impl<'a> Files<'a> for &'a DiagnosticAggregator<'a> {
    type FileId = FileId;
    type Name = FileName<'a>;
    type Source = FileSource<'a>;

    fn name(&self, file_id: usize) -> Result<Self::Name, MetaError> {
        self.get_file_handle(file_id).map(|h| h.file_name())
    }

    fn source(&self, file_id: usize) -> Result<Self::Source, MetaError> {
        self.get_file_handle(file_id).map(|h| h.file_contents())
    }

    fn line_index(&self, file_id: usize, byte_index: usize) -> Result<usize, MetaError> {
        self.get_file_handle(file_id)
            .and_then(|h| h.file.line_index((), byte_index))
    }

    fn line_range(&self, file_id: usize, line_index: usize) -> Result<Range<usize>, Error> {
        self.get_file_handle(file_id)
            .and_then(|h| h.file.line_range((), line_index))
    }
}

impl<'a> DiagnosticAggregator<'a> {
    /// Create a new [FileDiagnostics] and return its [FileId]. You can then use
    /// [Self::get_file_handle] or [Self::get_file_handle_mut] to get access to the
    /// [FileDiagnostics].
    pub fn new_file(&mut self, file_name: &'a str, file_contents: &'a str) -> FileId {
        let file_id = self.file_db.len();
        let file_diagnostics = FileDiagnostics::new(file_id, file_name, file_contents);
        self.file_db.push(file_diagnostics);
        file_id
    }

    /// Returns an immutable reference to the [FileDiagnostics].
    /// Throws [MetaError::FileMissing] if the given [FileId] is invalid.
    pub fn get_file_handle<'b>(
        &'b self,
        file_id: FileId,
    ) -> Result<&'b FileDiagnostics<'b>, MetaError> {
        self.file_db.get(file_id).ok_or(MetaError::FileMissing)
    }

    /// Returns a mutable reference to the [FileDiagnostics].
    /// Throws [MetaError::FileMissing] if the given [FileId] is invalid.
    pub fn get_file_handle_mut<'b>(
        &'b mut self,
        file_id: FileId,
    ) -> Result<&'b mut FileDiagnostics<'a>, MetaError> {
        self.file_db.get_mut(file_id).ok_or(MetaError::FileMissing)
    }

    /// Emits all file diagnostics to the provided [WriteColor] implementation.
    fn emit_all(
        &self,
        writer: &mut impl WriteColor,
        config: Config,
    ) -> Result<ExitStatus, codespan_reporting::files::Error> {
        // helper macro for emitting a single diagnostic
        macro_rules! emit {
            ($diagnostic:expr) => {{
                codespan_reporting::term::emit(writer, &config, &self, &$diagnostic)?;
            }};
        }

        let mut counted_errors = 0;

        for file_diagnostics in &self.file_db {
            for warning in &file_diagnostics.warnings {
                let codespan_warning = CodespanDiagnostic::warning()
                    .with_message(warning.headline.to_owned())
                    .with_labels(warning.messages.to_owned())
                    .with_notes(warning.notes.to_owned());

                emit!(codespan_warning)
            }

            for error in &file_diagnostics.errors {
                let codespan_error = CodespanDiagnostic::error()
                    .with_message(error.headline.to_owned())
                    .with_labels(error.messages.to_owned())
                    .with_notes(error.notes.to_owned());

                emit!(codespan_error);
                counted_errors += 1;
            }
        }

        let exit_code = if counted_errors == 0 {
            ExitStatus::Success
        } else {
            ExitStatus::Failure
        };

        Ok(exit_code)
    }

    /// Allocates a new buffer and emits all of the diagnostics to said buffer, returning it along
    /// with the exit code of the program, or a reason why the printing failed
    pub fn emit_all_to_buffer(
        &self,
        use_ansi: bool,
    ) -> (
        Vec<u8>,
        Result<ExitStatus, codespan_reporting::files::Error>,
    ) {
        let mut buf = if use_ansi {
            Buffer::ansi()
        } else {
            Buffer::no_color()
        };

        let config = codespan_reporting::term::Config::default();
        let result = self.emit_all(&mut buf, config);
        let bytes = buf.into_inner();
        (bytes, result)
    }

    /// Prints all of the diagnostics to stderr and returns the exit code of the program, or
    /// a reason why the printing failed
    pub fn emit_all_to_stderr(&self) -> Result<ExitStatus, codespan_reporting::files::Error> {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        let mut writer_lock = writer.lock();

        self.emit_all(&mut writer_lock, config)
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
pub struct FileDiagnostics<'a> {
    file_id: FileId,
    file: SimpleFile<FileName<'a>, FileSource<'a>>,
    warnings: Vec<Diagnostic>,
    errors: Vec<Diagnostic>,
}

impl<'a> FileDiagnostics<'a> {
    /// Creates a new [FileDiagnostics] from the [SimpleFiles] database's `file_id` and a mutable
    /// reference to the [DiagnosticAggregator]
    fn new(
        file_id: FileId,
        file_name: FileName<'a>,
        file_contents: FileSource<'a>,
    ) -> FileDiagnostics<'a> {
        FileDiagnostics {
            file_id,
            file: SimpleFile::new(file_name, file_contents),
            warnings: Vec::new(),
            errors: Vec::new(),
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
        self.errors.push(diagnostic);
        self.errors.last_mut().unwrap()
    }

    /// Creates a `warning` [Diagnostic] in the underlying [DiagnosticAggregator] and returns
    /// a mutable reference to it
    pub fn warning<'ret>(&'ret mut self, headline: impl Into<String>) -> &'ret mut Diagnostic {
        let diagnostic = self.with_headline(headline.into());
        self.warnings.push(diagnostic);
        self.warnings.last_mut().unwrap()
    }

    pub fn error_count(&self) -> usize {
        self.errors.len()
    }

    pub fn warning_count(&self) -> usize {
        self.warnings.len()
    }

    /// Returns the name of the file associated with the [FileDiagnostics] instance.
    pub fn file_name(&self) -> &str {
        self.file.name()
    }

    /// Returns the contents of the file associated with the [FileDiagnostics] instance.
    pub fn file_contents(&self) -> &str {
        self.file.source()
    }
}

/// A single self-contained diagnostic containing a main headline, messages annotating the input
/// and notes that follow it
#[derive(Debug)]
pub struct Diagnostic {
    file_id: FileId,
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

        self.messages.push(if let Some(message) = message {
            label.with_message(message)
        } else {
            label
        });

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
