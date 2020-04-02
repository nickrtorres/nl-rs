mod integration {
    use std::process::Command;

    #[test]
    fn it_reports_error_on_fnf() {
        let expected = Command::new("/usr/bin/nl")
            .arg("testinput.txt")
            .output()
            .expect("Could not execute /usr/bin/nl");
        let expected_stderr = String::from_utf8(expected.stderr).unwrap();

        let actual = Command::new("./target/debug/nl-rs")
            .arg("testinput.txt")
            .output()
            .expect("Could not execute ./target/debug/nl-rs");
        let actual_stderr = String::from_utf8(actual.stderr).unwrap();

        assert!(expected_stderr.contains("No such file or directory"));
        assert!(actual_stderr.contains("No such file or directory"));
    }

    #[test]
    fn it_numbers_lines_by_default() {
        let expected = Command::new("/usr/bin/nl")
            .arg("/etc/passwd")
            .output()
            .expect("Could not execute /usr/bin/nl");

        let actual = Command::new("./target/debug/nl")
            .arg("/etc/passwd")
            .output()
            .expect("Could not execute ./target/debug/nl-rs");

        assert_eq!(
            String::from_utf8(actual.stdout).unwrap(),
            String::from_utf8(expected.stdout).unwrap()
        );
    }

    #[test]
    fn it_can_arbitrarily_increment() {
        let expected = Command::new("/usr/bin/nl")
            .arg("-i")
            .arg("5")
            .arg("/etc/passwd")
            .output()
            .expect("Could not execute /usr/bin/nl");

        let actual = Command::new("./target/debug/nl")
            .arg("-i")
            .arg("5")
            .arg("/etc/passwd")
            .output()
            .expect("Could not execute ./target/debug/nl-rs");

        assert_eq!(
            String::from_utf8(actual.stdout).unwrap(),
            String::from_utf8(expected.stdout).unwrap()
        );
    }

    #[test]
    fn it_can_specify_a_starting_number() {
        let expected = Command::new("/usr/bin/nl")
            .arg("-v")
            .arg("5")
            .arg("/etc/passwd")
            .output()
            .expect("Could not execute /usr/bin/nl");

        let actual = Command::new("./target/debug/nl")
            .arg("-v")
            .arg("5")
            .arg("/etc/passwd")
            .output()
            .expect("Could not execute ./target/debug/nl-rs");

        assert_eq!(
            String::from_utf8(actual.stdout).unwrap(),
            String::from_utf8(expected.stdout).unwrap()
        );
    }

    #[test]
    fn it_can_left_align_text() {
        let expected = Command::new("/usr/bin/nl")
            .arg("-n")
            .arg("ln")
            .arg("/etc/passwd")
            .output()
            .expect("Could not execute /usr/bin/nl");

        let actual = Command::new("./target/debug/nl")
            .arg("-n")
            .arg("ln")
            .arg("/etc/passwd")
            .output()
            .expect("Could not execute ./target/debug/nl-rs");

        assert_eq!(
            String::from_utf8(actual.stdout).unwrap(),
            String::from_utf8(expected.stdout).unwrap()
        );
    }

    #[test]
    fn it_can_right_align_text() {
        let expected = Command::new("/usr/bin/nl")
            .arg("-n")
            .arg("rn")
            .arg("/etc/passwd")
            .output()
            .expect("Could not execute /usr/bin/nl");

        let actual = Command::new("./target/debug/nl")
            .arg("-n")
            .arg("rn")
            .arg("/etc/passwd")
            .output()
            .expect("Could not execute ./target/debug/nl-rs");

        assert_eq!(
            String::from_utf8(actual.stdout).unwrap(),
            String::from_utf8(expected.stdout).unwrap()
        );
    }

    #[test]
    fn it_can_right_align_text_with_zeros() {
        let expected = Command::new("/usr/bin/nl")
            .arg("-n")
            .arg("rz")
            .arg("/etc/passwd")
            .output()
            .expect("Could not execute /usr/bin/nl");

        let actual = Command::new("./target/debug/nl")
            .arg("-n")
            .arg("rz")
            .arg("/etc/passwd")
            .output()
            .expect("Could not execute ./target/debug/nl-rs");

        assert_eq!(
            String::from_utf8(actual.stdout).unwrap(),
            String::from_utf8(expected.stdout).unwrap()
        );
    }

    #[test]
    fn it_can_number_lines_matching_regex() {
        let expected = Command::new("/usr/bin/nl")
            .arg("-b")
            .arg("p^[^_]")
            .arg("/etc/passwd")
            .output()
            .expect("Could not execute /usr/bin/nl");

        let actual = Command::new("./target/debug/nl")
            .arg("-b")
            .arg("p^[^_]")
            .arg("/etc/passwd")
            .output()
            .expect("Could not execute ./target/debug/nl-rs");

        assert_eq!(
            String::from_utf8(actual.stdout).unwrap(),
            String::from_utf8(expected.stdout).unwrap()
        );
    }

    #[test]
    fn it_aware_of_adjacent_blank_line_rules() {
        let expected = Command::new("/usr/bin/nl")
            .arg("-b")
            .arg("a")
            .arg("-l")
            .arg("2")
            .arg("./tests/blanks.txt")
            .output()
            .expect("Could not execute /usr/bin/nl");

        let actual = Command::new("./target/debug/nl")
            .arg("-b")
            .arg("a")
            .arg("-l")
            .arg("2")
            .arg("./tests/blanks.txt")
            .output()
            .expect("Could not execute ./target/debug/nl-rs");

        assert_eq!(
            String::from_utf8(actual.stdout).unwrap(),
            String::from_utf8(expected.stdout).unwrap()
        );
    }

    #[test]
    fn it_can_number_pages_discretely() {
        let expected = Command::new("/usr/bin/nl")
            .arg("-f")
            .arg("a")
            .arg("./tests/pages.txt")
            .output()
            .expect("Could not execute /usr/bin/nl");

        let actual = Command::new("./target/debug/nl")
            .arg("-f")
            .arg("a")
            .arg("./tests/pages.txt")
            .output()
            .expect("Could not execute ./target/debug/nl-rs");

        assert_eq!(
            String::from_utf8(actual.stdout).unwrap(),
            String::from_utf8(expected.stdout).unwrap()
        );
    }
}
