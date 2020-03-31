// This implementation references (1) apple's and (2) illumos's. As such, both
// copyrights are provided below for brevity:
//
// (1)
// Copyright (c) 1999 The NetBSD Foundation, Inc.
// All rights reserved.
//
// This code is derived from software contributed to The NetBSD Foundation
// by Klaus Klein.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
// 3. All advertising materials mentioning features or use of this software
//    must display the following acknowledgement:
//        This product includes software developed by the NetBSD
//        Foundation, Inc. and its contributors.
// 4. Neither the name of The NetBSD Foundation nor the names of its
//    contributors may be used to endorse or promote products derived
//    from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE NETBSD FOUNDATION, INC. AND CONTRIBUTORS
// ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
// TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
// PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR CONTRIBUTORS
// BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// (2)
// CDDL HEADER START
//
// The contents of this file are subject to the terms of the
// Common Development and Distribution License, Version 1.0 only
// (the "License").  You may not use this file except in compliance
// with the License.
//
// You can obtain a copy of the license at usr/src/OPENSOLARIS.LICENSE
// or http://www.opensolaris.org/os/licensing.
// See the License for the specific language governing permissions
// and limitations under the License.
//
// When distributing Covered Code, include this CDDL HEADER in each
// file and include the License file at usr/src/OPENSOLARIS.LICENSE.
// If applicable, add the following below this CDDL HEADER, with the
// fields enclosed by brackets "[]" replaced with your own identifying
// information: Portions Copyright [yyyy] [name of copyright owner]
//
// CDDL HEADER END
//
//
// Copyright 1995 Sun Microsystems, Inc.  All rights reserved.
// Use is subject to license terms.
//
//
//	Copyright (c) 1984, 1986, 1987, 1988, 1989 AT&T
//	  All Rights Reserved
use clap::{App, Arg};
use lazy_static::lazy_static;
use program::Program;
use regex::{Error, Regex};
use std::fmt;
use std::fs::File;
use std::io::{self, stderr, stdin, stdout, BufRead, BufReader, Write};
use std::process::exit;

#[derive(Debug)]
enum NumberingType {
    All,
    NonEmpty,
    None,
    Regex(Regex),
}

#[derive(PartialEq, Debug)]
enum NlError<'a> {
    BadRegex(Error),
    EmptyRegex,
    IllegalFormat(&'a str),
    IllegalNumberingType(&'a str),
}

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
        }
    }
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

lazy_static! {
    static ref NL: Program = Program::new("nl");
}

struct Cli<'a> {
    program: &'static Program,
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

/// execute `$f` on `$var` and storing the result in `$cli`.
/// do nothing if `$var` is `None`. `perror` if `$f(Some($var)).is_err()`
macro_rules! store_arg {
    ( $cli:ident, $var:ident, $f:expr ) => {
        match $var {
            Some(v) => match $f(v) {
                Ok(v) => {
                    $cli.$var = v;
                    $cli
                }
                Err(e) => $cli.program.perror(e),
            },
            None => $cli,
        }
    };
}

impl<'a> Cli<'a> {
    fn new(program: &'static Program) -> Self {
        Cli {
            program: program,
            blanks: 1,
            body: NumberingType::NonEmpty,
            delim: "\\:",
            footer: NumberingType::NonEmpty,
            format: LineNumberFormat::Rn,
            header: NumberingType::NonEmpty,
            increment: 1,
            startnum: 1,
            restart: true,
            width: 6,
        }
    }

    // stub
    fn blanks(self, blanks: Option<&str>) -> Self {
        self
    }

    fn body(mut self, body: Option<&str>) -> Self {
        store_arg!(self, body, NumberingType::from_opt)
    }

    // stub
    fn delim(self, delim: Option<&str>) -> Self {
        self
    }

    fn footer(mut self, footer: Option<&str>) -> Self {
        store_arg!(self, footer, NumberingType::from_opt)
    }

    fn format(mut self, format: Option<&str>) -> Self {
        store_arg!(self, format, LineNumberFormat::from_opt)
    }

    fn header(mut self, header: Option<&str>) -> Self {
        store_arg!(self, header, NumberingType::from_opt)
    }

    // stub
    fn increment(self, increment: Option<&str>) -> Self {
        self
    }

    // stub
    fn startnum(self, startnum: Option<&str>) -> Self {
        self
    }

    // stub
    fn restart(self, startnum: Option<&str>) -> Self {
        self
    }

    // stub
    fn width(self, width: Option<&str>) -> Self {
        self
    }

    fn filter<T: BufRead>(self, mut input: T) -> Result<(), io::Error> {
        let mut buf = String::with_capacity(1024);
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
                num += 1;
            }

            writeln!(stdout(), "{}", line)?;
        }

        Ok(())
    }
}

fn main() {
    let args = App::new(NL.name)
        .version("0.0.1")
        .arg(Arg::with_name("blanks").short("l").takes_value(true))
        .arg(Arg::with_name("body-type").short("b").takes_value(true))
        .arg(Arg::with_name("delim").short("d").takes_value(true))
        .arg(Arg::with_name("file").index(1).takes_value(true))
        .arg(Arg::with_name("footer-type").short("f").takes_value(true))
        .arg(Arg::with_name("format").short("n").takes_value(true))
        .arg(Arg::with_name("header-type").short("h").takes_value(true))
        .arg(Arg::with_name("increment").short("i").takes_value(true))
        .arg(Arg::with_name("initial-value").short("v").takes_value(true))
        .arg(Arg::with_name("restart-at-page").short("p"))
        .arg(Arg::with_name("width").short("w").takes_value(true))
        .get_matches_safe();

    let matches = match args {
        Ok(m) => m,
        Err(e) => {
            let _ = write!(stderr(), "{}", e);
            exit(0);
        }
    };

    let cli = Cli::new(&NL)
        .blanks(matches.value_of("blanks"))
        .body(matches.value_of("body-type"))
        .delim(matches.value_of("delim"))
        .footer(matches.value_of("footer-type"))
        .format(matches.value_of("format"))
        .header(matches.value_of("header-type"))
        .increment(matches.value_of("increment"))
        .startnum(matches.value_of("initial-value"))
        .restart(matches.value_of("restart-at-page"))
        .width(matches.value_of("width"));

    let stdin = stdin();
    let result = match matches.value_of("file") {
        Some(f) => match File::open(f) {
            Ok(f) => cli.filter(&mut BufReader::new(f)),
            Err(e) => NL.perror(format!("{}: {}", f, e)),
        },
        None => cli.filter(&mut stdin.lock()),
    };

    if let Err(e) = result {
        NL.perror(e);
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
