#![warn(clippy::pedantic, clippy::nursery)]

use clap::ArgMatches;
use regex::{self, Regex};
use std::collections::HashMap;
use std::error;
use std::fmt;
use std::fs::File;
use std::io::{self, stdin, stdout, BufRead, BufReader, Write};
use std::num;
use std::ptr;
use std::result;
use std::str::FromStr;

type Result<'a, T> = result::Result<T, NlError<'a>>;

#[derive(Debug)]
/// Variants for what can go wrong when running `nl(1)`
pub enum NlError<'a> {
    /// `Regex::new(Regex)` failed. This is likely do to an invalid Regex
    BadRegex(regex::Error),
    /// The Regex option was specified, but no regex was provided
    EmptyRegex,
    /// A non-canonical (left justified w/o zeros, right justified w/o zeros,
    /// right justified w/ zeros) numbering format  was given
    IllegalFormat(&'a str),
    /// A non-canonical (all, non-empty, none, regex) numbering type was given
    IllegalNumberingType(&'a str),
    /// An option that required an integer was specified, but a non-integral
    /// type was given as the parameter
    InvalidNumber,
    /// An I/O error occured while running the program
    IoError(io::Error),
    /// An invalid delimeter was specified
    InvalidDelim(&'a str),
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
            NlError::InvalidDelim(d) => {
                write!(f, "invalid delim argument -- {}", (*d).to_string())
            }
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

#[derive(Debug, Clone)]
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
    fn from_opt(s: &str) -> Result<Self> {
        match s {
            "t" => Ok(Self::NonEmpty),
            "a" => Ok(Self::All),
            "n" => Ok(Self::None),
            s => {
                // if we're here we were either given an unsupported
                // numbering type or 'p' with a regex
                if !s.starts_with('p') {
                    return Err(NlError::IllegalNumberingType(s));
                }

                if s.len() <= 1 {
                    return Err(NlError::EmptyRegex);
                }

                let (_p, re) = s.split_at(1);
                match Regex::new(re) {
                    Ok(re) => Ok(Self::Regex(re)),
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
    fn from_opt(opt: Option<&str>) -> Result<Self> {
        match opt {
            None | Some("rn") => Ok(Self::Rn),
            Some("ln") => Ok(Self::Ln),
            Some("rz") => Ok(Self::Rz),
            Some(s) => Err(NlError::IllegalFormat(s)),
        }
    }

    // This does not perform well. Everytime it's called a new string is
    // allocated
    fn as_string(&self, num: Option<u32>, width: usize) -> String {
        // TODO can num be &str ?
        let num = num.map_or_else(|| " ".to_string(), |n| n.to_string());
        match self {
            Self::Ln => format!("{:<width$}", num, width = width),
            Self::Rn => format!("{:>width$}", num, width = width),
            Self::Rz => format!("{:0>width$}", num, width = width),
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
    footer: NumberingType,
    format: LineNumberFormat,
    header: NumberingType,
    increment: u32,
    startnum: u32,
    norestart: bool,
    width: usize,
    file: FileType<'a>,
    states: HashMap<usize, char>,
}

fn parse_str_or<F: FromStr>(
    s: Option<&str>,
    default: F,
) -> result::Result<F, <F as FromStr>::Err> {
    s.map_or(Ok(default), str::parse)
}

impl<'a> Cli<'a> {
    /// Parsing command line options is tricky. The options are *optional*.
    /// Additionally, a user can provide invalid input (e.g. empty input,
    /// non numeric input for a numeric option, etc.). In anycase, we need
    /// to deterministically evaluate what the user provides and store it
    /// when it's valid, propogate an error when it's invalid, or default to a
    /// well-defined value when it's absent.
    ///
    /// # Errors
    /// `Cli::new` will return on error if:
    /// (1) a numeric option is specified and a non-numeric value is given
    /// (2) a non-canonical numbering type is given
    /// (3) a non-canonical numbering format is given
    pub fn new(args: &'a ArgMatches) -> Result<'a, Self> {
        let body = args
            .value_of("body-type")
            .map_or(Ok(NumberingType::NonEmpty), NumberingType::from_opt)?;

        let header = args
            .value_of("header-type")
            .map_or(Ok(NumberingType::None), NumberingType::from_opt)?;

        let footer = args
            .value_of("footer-type")
            .map_or(Ok(NumberingType::None), NumberingType::from_opt)?;

        let format = LineNumberFormat::from_opt(args.value_of("format"))?;

        let blanks = parse_str_or(args.value_of("blanks"), 1)?;

        let startnum = parse_str_or(args.value_of("initial-value"), 1)?;

        let increment = parse_str_or(args.value_of("increment"), 1)?;

        let width = parse_str_or(args.value_of("width"), 6)?;

        let file = match args.value_of("file") {
            Some(f) => FileType::File(f),
            None => FileType::Stdin,
        };

        let norestart = args.is_present("restart");

        let delim = args.value_of("delim").map_or(
            Ok(vec!['\\', ':']),
            |s| -> Result<Vec<char>> {
                // TODO SUS says this can be 0 < s.len() <= 2
                if s.len() != 2 {
                    return Err(NlError::InvalidDelim(s));
                }

                Ok(s.chars().collect::<Vec<char>>())
            },
        )?;

        assert!(delim.len() == 2);
        let states: HashMap<usize, char> = {
            let mut m = HashMap::new();
            m.insert(0, delim[0]);
            m.insert(1, delim[1]);
            m
        };

        Ok(Cli {
            blanks,
            body,
            footer,
            format,
            header,
            startnum,
            increment,
            norestart,
            width,
            file,
            states,
        })
    }

    /// Output a file to `stdout` annotated with numbering in the style
    /// specified by the user through command line flags.
    /// # Errors
    /// Filter can fail on numerous `io::Error`s (e.g. unable to open a file,
    /// unable to read lines from a file, etc.),
    ///
    /// All io errors are converted to `NlError`s that wrap the outstanding
    /// `io::Error`
    pub fn filter(self) -> Result<'a, ()> {
        let stdin = stdin();
        match self.file {
            FileType::File(f) => {
                let file = File::open(f)?;
                self.try_filter(&mut BufReader::new(file))
            }
            FileType::Stdin => self.try_filter(&mut stdin.lock()),
        }
    }

    /// Gets the appropriate section transition based on the given line
    ///
    /// If `line` contains `delim`, determine if it's `delim` for footer, body,
    /// or header by executing a FSM.
    ///
    /// # Returns
    /// `None` if a section transition is unneeded
    /// `Some(type)` if a tranisition is needed, where type is the new current
    /// numbering type
    fn section(&self, line: &str) -> Option<&NumberingType> {
        if line.len() > 6 || line.len() < 2 {
            return None;
        }

        // TODO move this into an instance variable
        let types: HashMap<usize, &NumberingType> = {
            let mut m: HashMap<usize, &NumberingType> =
                HashMap::with_capacity(8);
            m.insert(2, &self.footer);
            m.insert(4, &self.body);
            m.insert(6, &self.header);
            m
        };

        for (state, c) in line.chars().enumerate() {
            if (*self.states.get(&(state % 2))?) != c {
                return None;
            }
        }

        Some(*types.get(&line.len())?)
    }

    fn try_filter<T: BufRead>(self, input: T) -> Result<'a, ()> {
        let mut adj = 1;
        let mut current_numbering = &self.body;
        let mut num = self.startnum;
        for line in input.lines() {
            let line = line?;

            if let Some(s) = self.section(&line) {
                if ptr::eq(s, &self.header) && !self.norestart {
                    num = self.startnum;
                }

                current_numbering = s;
                continue;
            };

            // TODO clean this up
            let n = match &current_numbering {
                NumberingType::All => {
                    if line.is_empty() && adj < self.blanks {
                        adj += 1;
                        None
                    } else {
                        adj = 1;
                        Some(num)
                    }
                }
                NumberingType::NonEmpty => {
                    if line.is_empty() {
                        None
                    } else {
                        Some(num)
                    }
                }
                NumberingType::Regex(re) => {
                    if re.is_match(&line) {
                        Some(num)
                    } else {
                        None
                    }
                }
                _ => None,
            };

            // TODO this is slow. preallocate and reuse a buffer to avoid
            // an allocation on every line
            writeln!(
                stdout(),
                "{}\t{}",
                self.format.as_string(n, self.width),
                line
            )?;

            if n.is_some() {
                num += self.increment;
            }
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
        assert_eq!("1     ", left_aligned.as_string(Some(1), 6));
    }

    #[test]
    fn it_can_right_align() {
        let right_aligned = LineNumberFormat::Rn;
        assert_eq!("     1", right_aligned.as_string(Some(1), 6));
    }

    #[test]
    fn it_can_right_align_with_zeros() {
        let right_aligned = LineNumberFormat::Rz;
        assert_eq!("000001", right_aligned.as_string(Some(1), 6));
    }

    #[test]
    fn it_can_build_left_justified_variant() {
        assert_eq!(
            LineNumberFormat::from_opt(Some("ln")).unwrap(),
            LineNumberFormat::Ln
        );
    }

    #[test]
    fn it_can_build_right_justified_variant() {
        assert_eq!(
            LineNumberFormat::from_opt(Some("rn")).unwrap(),
            LineNumberFormat::Rn
        );
    }

    #[test]
    fn it_can_build_right_justified_leading_zeros_variant() {
        assert_eq!(
            LineNumberFormat::from_opt(Some("rz")).unwrap(),
            LineNumberFormat::Rz
        );
    }

    #[test]
    fn its_an_error_to_give_bad_numbering_format() {
        assert!(LineNumberFormat::from_opt(Some("zz")).is_err(),);
    }

    #[test]
    fn it_can_determine_its_section_non_delim() {
        let args = ArgMatches::new();
        let cli = Cli::new(&args).unwrap();
        assert_eq!(None, cli.section("foobar"));
    }

    #[test]
    fn it_can_determine_its_section_header() {
        let args = ArgMatches::new();
        let header = NumberingType::None;
        let cli = Cli::new(&args).unwrap();
        assert_eq!(Some(&header), cli.section("\\:\\:\\:"));
    }

    #[test]
    fn it_can_determine_its_section_footer() {
        let args = ArgMatches::new();
        let footer = NumberingType::None;
        let cli = Cli::new(&args).unwrap();
        assert_eq!(Some(&footer), cli.section("\\:"));
    }

    #[test]
    fn it_can_determine_its_section_body() {
        let args = ArgMatches::new();
        let body = NumberingType::NonEmpty;
        let cli = Cli::new(&args).unwrap();
        assert_eq!(Some(&body), cli.section("\\:\\:"));
    }
}
