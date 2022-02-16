{
  description = "Personal Emacs with packages pre-installed";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/21.11";

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux.emacs = import ./emacs.nix { pkgs = nixpkgs.legacyPackages.x86_64-linux; };

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.emacs;

  };
}
