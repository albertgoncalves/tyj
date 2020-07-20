with import <nixpkgs> {};
mkShell {
    buildInputs = [
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
