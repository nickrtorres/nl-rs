use clap::{App, Arg};

fn main() {
    let args = App::new("nl")
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
}
