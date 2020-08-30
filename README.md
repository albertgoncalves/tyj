# tyj

Static type-checking for the subset of `javascript` syntax that I tend to use.

The idea is to quickly type-check regular `.js` files without needing to bundle or transpile. Should be **fast** and **not require some auxiliary syntax** to facilitate static typing.

Needed things
---
*   [Nix](https://nixos.org/download.html)

Quick start
---
```
$ nix-shell
[nix-shell:path/to/tyj]$ ./scripts/check.sh                 # type-check, `clippy`, `rustfmt`
[nix-shell:path/to/tyj]$ ./scripts/build.sh test            # build `bin/test`
[nix-shell:path/to/tyj]$ ./scripts/build.sh main            # build `bin/main`
[nix-shell:path/to/tyj]$ ./scripts/run.sh test              # build, run `bin/test`
[nix-shell:path/to/tyj]$ ./scripts/run.sh FILENAME.js       # build, run `bin/main FILENAME.js`
[nix-shell:path/to/tyj]$ ./scripts/profile.sh FILENAME.js   # `perf`, `cachegrind`
```
