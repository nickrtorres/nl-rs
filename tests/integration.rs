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
}
