# tyj

Needed things
---
*   [Nix](https://nixos.org/nix/)

Quick start
---
```
$ nix-shell
[nix-shell:path/to/tyj]$ ./scripts/check.sh                 # type-check, `clippy`, `rustfmt`
[nix-shell:path/to/tyj]$ ./scripts/build.sh test            # build `bin/test`
[nix-shell:path/to/tyj]$ ./scripts/build.sh                 # build `bin/main`
[nix-shell:path/to/tyj]$ ./scripts/run.sh                   # build, run `$ bin/test`
[nix-shell:path/to/tyj]$ ./scripts/run.sh FILENAME.js       # build, run `$ bin/main FILENAME.js`
[nix-shell:path/to/tyj]$ ./scripts/profile.sh FILENAME.js   # `perf`, `cachegrind`
```
