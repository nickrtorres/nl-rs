use clap::{value_t, App, Arg};
use lazy_static::lazy_static;
use program::Program;
use regex::{Error, Regex};
use std::fmt;
use std::fs::File;
use std::io::{stdin, BufRead, BufReader};
/// This implementation references (1) apple's and (2) illumos's. As such, both
/// copyrights are provided below for brevity:
///
/// (1)
/// Copyright (c) 1999 The NetBSD Foundation, Inc.
/// All rights reserved.
///
/// This code is derived from software contributed to The NetBSD Foundation
/// by Klaus Klein.
///
/// Redistribution and use in source and binary forms, with or without
/// modification, are permitted provided that the following conditions
/// are met:
/// 1. Redistributions of source code must retain the above copyright
///    notice, this list of conditions and the following disclaimer.
/// 2. Redistributions in binary form must reproduce the above copyright
///    notice, this list of conditions and the following disclaimer in the
///    documentation and/or other materials provided with the distribution.
/// 3. All advertising materials mentioning features or use of this software
///    must display the following acknowledgement:
///        This product includes software developed by the NetBSD
///        Foundation, Inc. and its contributors.
/// 4. Neither the name of The NetBSD Foundation nor the names of its
///    contributors may be used to endorse or promote products derived
///    from this software without specific prior written permission.
///
/// THIS SOFTWARE IS PROVIDED BY THE NETBSD FOUNDATION, INC. AND CONTRIBUTORS
/// ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
/// TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
/// PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR CONTRIBUTORS
/// BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
/// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
/// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
/// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
/// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
/// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
/// POSSIBILITY OF SUCH DAMAGE.
///
/// (2)
/// CDDL HEADER START
///
/// The contents of this file are subject to the terms of the
/// Common Development and Distribution License, Version 1.0 only
/// (the "License").  You may not use this file except in compliance
/// with the License.
///
/// You can obtain a copy of the license at usr/src/OPENSOLARIS.LICENSE
/// or http://www.opensolaris.org/os/licensing.
/// See the License for the specific language governing permissions
/// and limitations under the License.
///
/// When distributing Covered Code, include this CDDL HEADER in each
/// file and include the License file at usr/src/OPENSOLARIS.LICENSE.
/// If applicable, add the following below this CDDL HEADER, with the
/// fields enclosed by brackets "[]" replaced with your own identifying
/// information: Portions Copyright [yyyy] [name of copyright owner]
///
/// CDDL HEADER END
///
///
/// Copyright 1995 Sun Microsystems, Inc.  All rights reserved.
/// Use is subject to license terms.
///
///
///	Copyright (c) 1984, 1986, 1987, 1988, 1989 AT&T
///	  All Rights Reserved

#[derive(Debug)]
enum NumberingType {
    All,
    NonEmpty,
    None,
    Regex(Regex),
}

#[derive(PartialEq, Debug)]
enum BadNumberingType<'a> {
    UnsupportedType(&'a str),
    EmptyRegex,
    BadRegex(Error),
}

impl<'a> fmt::Display for BadNumberingType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BadNumberingType::UnsupportedType(t) => {
                write!(f, "illegal body line numbering type -- {}", t)
            }
            BadNumberingType::EmptyRegex => {
                write!(f, "body expr: empty (sub)expression --")
            }
            BadNumberingType::BadRegex(e) => {
                write!(f, "body expr: ill formed regex -- {}", e)
            }
        }
    }
}

impl NumberingType {
    fn from_opt(s: &str) -> Result<NumberingType, BadNumberingType> {
        match s {
            "a" => Ok(NumberingType::All),
            "t" => Ok(NumberingType::NonEmpty),
            "n" => Ok(NumberingType::None),
            _ => {
                // if we're here we were either given an unsupported
                // numbering type or 'p' with a regex
                if !s.starts_with("p") {
                    return Err(BadNumberingType::UnsupportedType(s));
                }

                if !(s.len() > 1) {
                    return Err(BadNumberingType::EmptyRegex);
                }

                let (_p, re) = s.split_at(1);
                match Regex::new(re) {
                    Ok(re) => Ok(NumberingType::Regex(re)),
                    Err(e) => Err(BadNumberingType::BadRegex(e)),
                }
            }
        }
    }
}

fn filter<T: BufRead>(
    input: &mut T,
    startnum: u32,
    numbering: &NumberingType,
) {
    let mut buf = String::with_capacity(1024);
    let mut num = startnum;
    while let Ok(s) = input.read_line(&mut buf) {
        if s == 0 {
            return;
        }

        let donumber = match numbering {
            NumberingType::All => true,
            NumberingType::NonEmpty => !(s == 1 && buf.starts_with("\n")),
            NumberingType::Regex(re) => re.is_match(&buf),
            _ => false,
        };

        if donumber {
            print!("{}", num);
        }

        print!("\t{}", buf);

        if donumber {
            num += 1;
        }

        buf.clear();
    }
}

lazy_static! {
    static ref NL: Program = Program::new("nl");
}

fn main() {
    let args = App::new(NL.name)
        .version("0.0.1")
        .arg(Arg::with_name("body-type").short("b").takes_value(true))
        .arg(Arg::with_name("delim").short("d").takes_value(true))
        .arg(Arg::with_name("footer-type").short("f").takes_value(true))
        .arg(Arg::with_name("header-type").short("h").takes_value(true))
        .arg(Arg::with_name("increment").short("i").takes_value(true))
        .arg(Arg::with_name("blanks").short("l").takes_value(true))
        .arg(Arg::with_name("format").short("n").takes_value(true))
        .arg(Arg::with_name("restart-at-page").short("p"))
        .arg(Arg::with_name("initial-value").short("v").takes_value(true))
        .arg(Arg::with_name("width").short("w").takes_value(true))
        .arg(Arg::with_name("file").index(1).takes_value(true))
        .get_matches();

    // XSI: "The default *type* for logical page body shall be **t** (text lines numbered)"
    let body_type = match NumberingType::from_opt(
        args.value_of("body-type").unwrap_or("t"),
    ) {
        Ok(t) => t,
        Err(e) => NL.perror(&e),
    };

    let startnum = value_t!(args.value_of("initial-value"), u32).unwrap_or(1);

    let stdin = stdin();
    match args.value_of("file") {
        Some(f) => match File::open(f) {
            Ok(f) => filter(&mut BufReader::new(f), startnum, &body_type),
            Err(e) => NL.perror(&e),
        },
        None => filter(&mut stdin.lock(), startnum, &body_type),
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
        assert_eq!(BadNumberingType::UnsupportedType("zzz"), t.unwrap_err());
    }

    #[test]
    fn it_recognizes_empty_regex() {
        let t = NumberingType::from_opt("p");
        assert!(t.is_err());
        assert_eq!(BadNumberingType::EmptyRegex, t.unwrap_err());
    }
}
