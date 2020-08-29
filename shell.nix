with import <nixpkgs> {};
mkShell {
    buildInputs = [
        clippy
        linuxPackages.perf
        rustc
        rustfmt
        shellcheck
        valgrind
    ];
    shellHook = ''
        . .shellhook
    '';
}
