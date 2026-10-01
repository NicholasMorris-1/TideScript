# This is here to allow nix-shell to call the right thing, but we define the
# actual shell in default.nix so that everything uses the same package
# set/dependencies
(import ./default_2.nix {}).shell
