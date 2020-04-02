use clap::ArgMatches;
use regex::{self, Regex};
use std::error;
use std::fmt;
use std::fs::File;
use std::io::{self, stdin, stdout, BufRead, BufReader, Write};
use std::num;
use std::str::FromStr;

#[derive(Debug)]
/// Variants for what can go wrong when running `nl(1)`
pub enum NlError<'a> {
    /// `Regex::new(Regex)` failed. This is likely do to an invalid Regex
    BadRegex(regex::Error),
    /// The Regex option was specified, but no regex was provided
    EmptyRegex,
    /// A non-canonical (left justified w/o zeros, right justified w/o zeros, right justified w/
    /// zeros) numbering format  was given
    IllegalFormat(&'a str),
    /// A non-canonical (all, non-empty, none, regex) numbering type was given
    IllegalNumberingType(&'a str),
    /// An option that required an integer was specified, but a non-integral type was given as the
    /// parameter
    InvalidNumber,
    /// An I/O error occured while running the program
    IoError(io::Error),
}

impl<'a> error::Error for NlError<'a> {}

impl<'a> fmt::Display for NlError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NlError::BadRegex(e) => {
                write!(f, "body expr: ill formed regex -- {}", e)
            }
            NlError::EmptyRegex => {
                write!(f, "body expr: empty (sub)expression --")
            }
            NlError::IllegalNumberingType(t) => {
                write!(f, "illegal body line numbering type -- {}", t)
            }
            NlError::IllegalFormat(e) => write!(f, "illegal format -- {}", e),
            NlError::InvalidNumber => write!(f, "invalid num argument"),
            NlError::IoError(e) => write!(f, "{}", e.to_string()),
        }
    }
}

impl<'a> From<num::ParseIntError> for NlError<'a> {
    fn from(_err: num::ParseIntError) -> NlError<'a> {
        NlError::InvalidNumber
    }
}

impl<'a> From<io::Error> for NlError<'a> {
    fn from(err: io::Error) -> NlError<'a> {
        NlError::IoError(err)
    }
}

#[derive(Debug)]
enum NumberingType {
    /// Number everyline for the given file
    All,
    /// Number everyline that contains > 1 '\n' character
    NonEmpty,
    /// Don't number any lines
    None,
    /// Number lines that `Regex::match(Regex)` holds
    Regex(Regex),
}

impl NumberingType {
    fn from_opt(s: Option<&str>) -> Result<NumberingType, NlError> {
        match s {
            None | Some("t") => Ok(NumberingType::NonEmpty),
            Some("a") => Ok(NumberingType::All),
            Some("n") => Ok(NumberingType::None),
            Some(s) => {
                // if we're here we were either given an unsupported
                // numbering type or 'p' with a regex
                if !s.starts_with("p") {
                    return Err(NlError::IllegalNumberingType(s));
                }

                if !(s.len() > 1) {
                    return Err(NlError::EmptyRegex);
                }

                let (_p, re) = s.split_at(1);
                match Regex::new(re) {
                    Ok(re) => Ok(NumberingType::Regex(re)),
                    Err(e) => Err(NlError::BadRegex(e)),
                }
            }
        }
    }
}

#[derive(Debug, PartialEq)]
/// Variants for how the numbers on a numbered line are formatted
enum LineNumberFormat {
    /// Left aligned without leading zeros
    Ln,
    /// Right aligned without leading zeros (Default)
    Rn,
    /// Right aligned with leading zeros
    Rz,
}

impl LineNumberFormat {
    fn from_opt<'a>(opt: Option<&'a str>) -> Result<Self, NlError<'a>> {
        match opt {
            None | Some("rn") => Ok(LineNumberFormat::Rn),
            Some("ln") => Ok(LineNumberFormat::Ln),
            Some("rz") => Ok(LineNumberFormat::Rz),
            Some(s) => Err(NlError::IllegalFormat(s)),
        }
    }

    fn as_string(&self, num: u32, width: usize) -> String {
        match self {
            LineNumberFormat::Ln => format!("{:<width$}", num, width = width),
            LineNumberFormat::Rn => format!("{:>width$}", num, width = width),
            LineNumberFormat::Rz => format!("{:0>width$}", num, width = width),
        }
    }
}

enum FileType<'a> {
    File(&'a str),
    Stdin,
}

/// An opaque structure to store command line options specified by the user.
pub struct Cli<'a> {
    blanks: u32,
    body: NumberingType,
    delim: &'a str,
    footer: NumberingType,
    format: LineNumberFormat,
    header: NumberingType,
    increment: u32,
    startnum: u32,
    restart: bool,
    width: usize,
    file: FileType<'a>,
}

fn parse_str_or<F: FromStr>(
    s: Option<&str>,
    default: F,
) -> Result<F, <F as FromStr>::Err> {
    s.map_or(Ok(default), |s| s.parse::<F>())
}

impl<'a> Cli<'a> {
    /// Parsing command line options is tricky. The options are *optional*. Additionally,
    /// a user can provide invalid input (e.g. empty input, non numeric input for a numeric option,
    /// etc.). In anycase, we need to deterministically evaluate what the user provides and store it
    /// when it's valid, propogate an error when it's invalid, or default to a well-defined value when
    /// it's absent.
    pub fn new(args: &'a ArgMatches) -> Result<Self, NlError<'a>> {
        let body = NumberingType::from_opt(args.value_of("body-type"))?;

        let header = NumberingType::from_opt(args.value_of("header-type"))?;

        let footer = NumberingType::from_opt(args.value_of("footer-type"))?;

        let format = LineNumberFormat::from_opt(args.value_of("format"))?;

        let blanks = parse_str_or(args.value_of("blanks"), 1)?;

        let startnum = parse_str_or(args.value_of("initial-value"), 1)?;

        let increment = parse_str_or(args.value_of("increment"), 1)?;

        let width = parse_str_or(args.value_of("width"), 6)?;

        let file = match args.value_of("file") {
            Some(f) => FileType::File(f),
            None => FileType::Stdin,
        };

        let restart = args.is_present("restart");

        Ok(Cli {
            blanks: 1,
            body,
            delim: "\\:",
            footer,
            format,
            header,
            startnum,
            increment,
            restart,
            width,
            file,
        })
    }

    /// Output a file to `stdout` annotated with numbering in the style specified by
    /// the user through command line flags.
    pub fn filter(self) -> Result<(), NlError<'a>> {
        let stdin = stdin();
        match self.file {
            FileType::File(f) => {
                let file = File::open(f)?;
                self.try_filter(&mut BufReader::new(file))
            }
            FileType::Stdin => self.try_filter(&mut stdin.lock()),
        }
    }

    fn try_filter<T: BufRead>(self, input: T) -> Result<(), NlError<'a>> {
        let mut num = self.startnum;
        for line in input.lines() {
            let line = line?;

            let donumber = match &self.body {
                NumberingType::All => true,
                NumberingType::NonEmpty => !(line.is_empty()),
                NumberingType::Regex(re) => re.is_match(&line),
                _ => false,
            };

            if donumber {
                write!(
                    stdout(),
                    "{}",
                    self.format.as_string(num, self.width)
                )?;

                num += self.increment;
            }

            writeln!(stdout(), "{}", line)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl PartialEq for NumberingType {
        fn eq(&self, other: &Self) -> bool {
            match (self, other) {
                (NumberingType::Regex(left), NumberingType::Regex(right)) => {
                    left.as_str() == right.as_str()
                }
                (NumberingType::All, NumberingType::All) => true,
                (NumberingType::None, NumberingType::None) => true,
                (NumberingType::NonEmpty, NumberingType::NonEmpty) => true,
                _ => false,
            }
        }
    }

    impl<'a> PartialEq for NlError<'a> {
        fn eq(&self, other: &Self) -> bool {
            match (self, other) {
                (NlError::BadRegex(left), NlError::BadRegex(right)) => {
                    left == right
                }
                (NlError::EmptyRegex, NlError::EmptyRegex) => true,
                (
                    NlError::IllegalFormat(left),
                    NlError::IllegalFormat(right),
                ) => left == right,
                (
                    NlError::IllegalNumberingType(left),
                    NlError::IllegalNumberingType(right),
                ) => left == right,
                (NlError::InvalidNumber, NlError::InvalidNumber) => true,
                (NlError::IoError(left), NlError::IoError(right)) => {
                    left.kind() == right.kind()
                }
                _ => false,
            }
        }
    }

    #[test]
    fn it_can_build_numbering_type_all() {
        let t = NumberingType::from_opt("a");
        assert!(t.is_ok());
        assert_eq!(NumberingType::All, t.unwrap());
    }

    #[test]
    fn it_can_build_numbering_type_non_empty() {
        let t = NumberingType::from_opt("t");
        assert!(t.is_ok());
        assert_eq!(NumberingType::NonEmpty, t.unwrap());
    }

    #[test]
    fn it_can_build_numbering_type_none() {
        let t = NumberingType::from_opt("n");
        assert!(t.is_ok());
        assert_eq!(NumberingType::None, t.unwrap());
    }

    #[test]
    fn it_can_build_numbering_type_regex() {
        let t = NumberingType::from_opt("p^foobar");
        assert!(t.is_ok());
        assert_eq!(
            NumberingType::Regex(Regex::new("^foobar").unwrap()),
            t.unwrap()
        );
    }

    #[test]
    fn it_recognizes_unsupported_numbering_types() {
        let t = NumberingType::from_opt("zzz");
        assert!(t.is_err());
        assert_eq!(NlError::IllegalNumberingType("zzz"), t.unwrap_err());
    }

    #[test]
    fn it_recognizes_empty_regex() {
        let t = NumberingType::from_opt("p");
        assert!(t.is_err());
        assert_eq!(NlError::EmptyRegex, t.unwrap_err());
    }

    #[test]
    fn it_can_left_align() {
        let left_aligned = LineNumberFormat::Ln;
        assert_eq!("1     ", left_aligned.as_string(1, 6));
    }

    #[test]
    fn it_can_right_align() {
        let right_aligned = LineNumberFormat::Rn;
        assert_eq!("     1", right_aligned.as_string(1, 6));
    }

    #[test]
    fn it_can_right_align_with_zeros() {
        let right_aligned = LineNumberFormat::Rz;
        assert_eq!("000001", right_aligned.as_string(1, 6));
    }

    #[test]
    fn it_can_build_left_justified_variant() {
        assert_eq!(
            LineNumberFormat::from_opt("ln").unwrap(),
            LineNumberFormat::Ln
        );
    }

    #[test]
    fn it_can_build_right_justified_variant() {
        assert_eq!(
            LineNumberFormat::from_opt("rn").unwrap(),
            LineNumberFormat::Rn
        );
    }

    #[test]
    fn it_can_build_right_justified_leading_zeros_variant() {
        assert_eq!(
            LineNumberFormat::from_opt("rz").unwrap(),
            LineNumberFormat::Rz
        );
    }

    #[test]
    fn its_an_error_to_give_bad_numbering_format() {
        assert!(LineNumberFormat::from_opt("zz").is_err(),);
    }
}
