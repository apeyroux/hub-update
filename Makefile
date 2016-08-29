all:
	stack build --copy-bins

nix:
	stack --nix build --copy-bins
