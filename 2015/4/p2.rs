fn main() {
    let stdin = std::io::stdin();
    let mut line = String::new();
    stdin.read_line(&mut line).unwrap();
    let line = line.trim();

    for i in 0.. {
        let digest = md5::compute(format!("{line}{i}").as_bytes());
        if &format!("{digest:x}")[0..6] == "000000" {
            println!("{i}");
            return;
        }
    }
}
