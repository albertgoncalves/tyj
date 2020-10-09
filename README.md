# tyj

Static type-checking for the subset of `javascript` syntax that I tend to use.

---
The idea is to type-check regular `.js` files without needing to bundle or transpile.
~Should **not require some auxiliary syntax** to facilitate static typing~.
Auxiliary syntax is _hidden_ in comments; valid `.js` source code can be immediately interpreted after type-checking.
Type-checking should be **fast**.

Needed things
---
*   [Nix](https://nixos.org/download.html)

Quick start
---
```
$ nix-shell
[nix-shell:path/to/tyj]$ ./scripts/check.sh                 # type-check, `clippy`, `rustfmt`
[nix-shell:path/to/tyj]$ ./scripts/build.sh test            # build `bin/test`
[nix-shell:path/to/tyj]$ ./scripts/build.sh debug           # build `bin/debug`
[nix-shell:path/to/tyj]$ ./scripts/test.sh                  # build, run `bin/test`
[nix-shell:path/to/tyj]$ ./scripts/debug.sh FILENAME.js     # build, run `bin/debug FILENAME.js`
[nix-shell:path/to/tyj]$ ./scripts/profile.sh FILENAME.js   # `perf`, `cachegrind`
```
