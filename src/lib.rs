use clap::ArgMatches;
use regex::{self, Regex};
use std::error;
use std::fmt;
use std::io::{self, stdout, BufRead, Write};
use std::num;

#[derive(PartialEq, Debug)]
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
        }
    }
}

impl<'a> From<num::ParseIntError> for NlError<'a> {
    fn from(_err: num::ParseIntError) -> NlError<'a> {
        NlError::InvalidNumber
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
    fn from_opt(s: &str) -> Result<NumberingType, NlError> {
        match s {
            "a" => Ok(NumberingType::All),
            "t" => Ok(NumberingType::NonEmpty),
            "n" => Ok(NumberingType::None),
            _ => {
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
enum LineNumberFormat {
    Ln,
    Rn,
    Rz,
}

impl LineNumberFormat {
    fn from_opt(opt: &str) -> Result<Self, NlError> {
        match opt {
            "ln" => Ok(LineNumberFormat::Ln),
            "rn" => Ok(LineNumberFormat::Rn),
            "rz" => Ok(LineNumberFormat::Rz),
            _ => Err(NlError::IllegalFormat(opt)),
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
}

impl<'a> Cli<'a> {
    /// Parsing command line options is tricky. The options are *optional*. Additionally,
    /// a user can provide invalid input (e.g. empty input, non numeric input for a numeric option,
    /// etc.). In anycase, we need to deterministically evaluate what the user provides and store it
    /// when it's valid, propogate an error when it's invalid, or default to a well-defined value when
    /// it's absent.
    pub fn new(args: &'a ArgMatches) -> Result<Self, NlError<'a>> {
        let body = match args.value_of("body") {
            Some(b) => NumberingType::from_opt(b)?,
            None => NumberingType::NonEmpty,
        };

        let header = match args.value_of("header-type") {
            Some(h) => NumberingType::from_opt(h)?,
            None => NumberingType::NonEmpty,
        };

        let footer = match args.value_of("footer-type") {
            Some(f) => NumberingType::from_opt(f)?,
            None => NumberingType::NonEmpty,
        };

        let format = match args.value_of("format") {
            Some(f) => LineNumberFormat::from_opt(f)?,
            None => LineNumberFormat::Rn,
        };

        let increment = match args.value_of("increment") {
            Some(i) => i.parse::<u32>()?,
            None => 1,
        };

        let startnum = match args.value_of("initial-value") {
            Some(i) => i.parse::<u32>()?,
            None => 1,
        };

        Ok(Cli {
            blanks: 1,
            body,
            delim: "\\:",
            footer,
            format,
            header,
            startnum,
            increment,
            restart: true,
            width: 6,
        })
    }

    /// Output a file to `stdout` annotated with numbering in the style specified by
    /// the user through command line flags.
    pub fn filter<T: BufRead>(self, input: T) -> Result<(), io::Error> {
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
