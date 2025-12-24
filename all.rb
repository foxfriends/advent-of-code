#!/usr/bin/env ruby

require "json"

def which(cmd)
  exts = ENV["PATHEXT"] ? ENV["PATHEXT"].split(";") : [""]
  ENV["PATH"].split(File::PATH_SEPARATOR).each do |path|
    exts.each do |ext|
      exe = File.join(path, "#{cmd}#{ext}")
      return exe if File.executable?(exe) && !File.directory?(exe)
    end
  end
  nil
end

def measure
  before = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
  yield
  after = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
  after - before
end

def report(label)
  answer = nil
  time = measure do
    answer = yield
  end
  return :program_error unless $?.success?
  answer = answer.chomp
  puts "\t#{label}: #{answer} in #{(time / 10000).to_f / 100}ms"
  return {
           duration_ns: time,
           answer: answer,
         }
end

module Rust
  def self.available?
    which "rustc"
  end

  def self.implemented?(part)
    File.exist? "p#{part}.rs"
  end

  def self.run(part)
    `rustc p#{part}.rs 2> /dev/null`
    return :rustc_error unless $?.success?
    report "p#{part}.rs" do
      `timeout 1m ./p#{part} < input`
    end
  end
end

module Haskell
  def self.available?
    which "ghc"
  end

  def self.implemented?(part)
    File.exist? "p#{part}.hs" or File.exist? "solution.cabal"
  end

  def self.run(part)
    if File.exist? "solution.cabal"
      `cabal build p#{part}`
      return :cabal_error unless $?.success?
      report "solution.cabal (p#{part})" do
        `timeout 1m cabal run p#{part} < input`
      end
    else
      `ghc p#{part}.hs 2> /dev/null`
      return :ghc_error unless $?.success?
      report "p#{part}.hs" do
        `timeout 1m ./p#{part} < input`
      end
    end
  end
end

module Trilogy
  def self.clang
    return "clang" if which "clang"
    return "clang-19" if which "clang-19"
    nil
  end

  def self.available?
    which "trilogy" and self.clang != nil
  end

  def self.implemented?(part)
    File.exist? "p#{part}.tri"
  end

  def self.run(part)
    `trilogy compile p#{part}.tri 2> /dev/null | #{self.clang} -O3 -o p#{part} -x ir -`
    return :trilogy_error unless $?.success?
    report "p#{part}.tri" do
      `timeout 1m ./p#{part} < input`
    end
  end
end

module C
  def self.clang
    return "clang" if which "clang"
    return "clang-19" if which "clang-19"
    return "gcc" if which "gcc"
    nil
  end

  def self.available?
    self.clang != nil
  end

  def self.implemented?(part)
    File.exist? "p#{part}.c"
  end

  def self.run(part)
    `#{self.clang} -O3 -o p#{part} p#{part}.c`
    return :clang_error unless $?.success?
    report "p#{part}.c" do
      `timeout 1m ./p#{part} < input`
    end
  end
end

module Cpp
  def self.available?
    which "g++"
  end

  def self.implemented?(part)
    File.exist? "p#{part}.cpp"
  end

  def self.run(part)
    `g++ -std=c++2c -O3 -o p#{part} p#{part}.cpp`
    return :gcc_error unless $?.success?
    report "p#{part}.cpp" do
      `timeout 1m ./p#{part} < input`
    end
  end
end

module Swift
  def self.available?
    which "swiftc"
  end

  def self.implemented?(part)
    File.exist? "p#{part}.swift"
  end

  def self.run(part)
    `swiftc p#{part} p#{part}.swift`
    return :swiftc_error unless $?.success?
    report "p#{part}.swift" do
      `timeout 1m ./p#{part} < input`
    end
  end
end

module Python
  def self.python
    return "python3" if which "python3"
    return "python" if which "python"
    return nil
  end

  def self.available?
    self.python
  end

  def self.implemented?(part)
    File.exist? "p#{part}.py"
  end

  def self.run(part)
    report "p#{part}.py" do
      `timeout 1m #{self.python} p#{part}.py < input`
    end
  end
end

module Ruby
  def self.available?
    which "ruby"
  end

  def self.implemented?(part)
    File.exist? "p#{part}.rb"
  end

  def self.run(part)
    report "p#{part}.rb" do
      `timeout 1m ruby p#{part}.rb < input`
    end
  end
end

module TypeScript
  def self.available?
    which "deno"
  end

  def self.implemented?(part)
    File.exist? "p#{part}.ts"
  end

  def self.run(part)
    report "p#{part}.ts" do
      `timeout 1m deno p#{part}.ts < input`
    end
  end
end

module Erlang
  def self.available?
    which "erl"
  end

  def self.implemented?(part)
    File.exist? "p#{part}.erl"
  end

  def self.run(part)
    `erl -compile p#{part}.erl`
    return :erl_error unless $?.success?
    report "p#{part}.erl" do
      `timeout 1m erl -noshell -s p#{part} main -s init stop < input`
    end
  end
end

module Elixir
  def self.available?
    which "elixir"
  end

  def self.implemented?(part)
    File.exist? "p#{part}.ex"
  end

  def self.run(part)
    report "p#{part}.ex" do
      `timeout 1m elixir p#{part}.ex < input`
    end
  end
end

module Gleam
  def self.available?
    which "gleam"
  end

  def self.implemented?(part)
    File.exist? "p#{part}/gleam.toml"
  end

  def self.run(part)
    pwd = Dir.pwd
    Dir.chdir "p#{part}"
    `gleam build --no-print-progress`
    return :gleam_error unless $?.success?
    report "p#{part}.gleam" do
      `timeout 1m gleam run --no-print-progress < ../input`
    end
    Dir.chdir pwd
  end
end

module Prolog
  def self.available?
    which "swipl"
  end

  def self.implemented?(part)
    return false unless File.exist? "p#{part}.pl"
    first_line = File.open "p#{part}.pl", &:gets
    not first_line.include?("perl")
  end

  def self.run(part)
    report "p#{part}.pl" do
      `timeout 1m swipl -s p#{part}.pl -g main,halt < input`
    end
  end
end

module Perl
  def self.available?
    which "perl"
  end

  def self.implemented?(part)
    return false unless File.exist? "p#{part}.pl"
    first_line = File.open "p#{part}.pl", &:gets
    first_line.include?("perl")
  end

  def self.run(part)
    report "p#{part}.pl" do
      `timeout 1m perl p#{part}.pl < input`
    end
  end
end

module Php
  def self.available?
    which "php"
  end

  def self.implemented?(part)
    File.exist? "p#{part}.php"
  end

  def self.run(part)
    report "p#{part}.php" do
      `timeout 1m php p#{part}.php < input`
    end
  end
end

module Bash
  def self.available?
    which "bash"
  end

  def self.implemented?(part)
    File.exist? "p#{part}.bash"
  end

  def self.run(part)
    report "p#{part}.bash" do
      `timeout 1m bash p#{part}.bash < input`
    end
  end
end

module Fish
  def self.available?
    which "fish"
  end

  def self.implemented?(part)
    File.exist? "p#{part}.fish"
  end

  def self.run(part)
    report "p#{part}.fish" do
      `timeout 1m fish p#{part}.fish < input`
    end
  end
end

languages = [Haskell, Rust, Trilogy, C, Cpp, Swift, Python, Ruby, TypeScript, Erlang, Elixir, Gleam, Prolog, Php, Perl, Bash, Fish]
  .filter { |lang| lang.available? }

root = Dir.pwd
report = {}

for year in `ls .`.split.filter { |x| /^\d+$/ =~ x }
  report[year] = {}
  for day in 1..25
    next unless Dir.exist? "#{year}/#{day}"
    report[year][day] = {}
    puts "year #{year} day #{day}"
    `just get #{day} #{year} 2> /dev/null`

    Dir.chdir "#{year}/#{day}"
    for part in ["1", "2"]
      report[year][day]["p#{part}"] = {}
      for lang in languages
        next unless lang.implemented? part
        report[year][day]["p#{part}"][lang] = lang.run part
      end
    end
    Dir.chdir root
  end
end

File.write "report.json", JSON.pretty_generate(report)
