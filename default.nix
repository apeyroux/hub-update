with import <nixpkgs> {}; {
     hsEnv = stdenv.mkDerivation {
     	      name = "hs";
	      buildInputs = [ stack ];
	      shellHook = ''export PS1="\u@\W λ "'';
     };
}
