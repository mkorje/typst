{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    systems.url = "github:nix-systems/default";
  };

  outputs = inputs@{ self, ... }:
  let
    forAllSystems = f: inputs.nixpkgs.lib.genAttrs (import inputs.systems) (system: (forSystem system f));
    forSystem = system: f: f rec {
      inherit system;
      pkgs = import inputs.nixpkgs {
        inherit system; overlays = [ (import inputs.rust-overlay) ];
      };
      lib = pkgs.lib;
    };
  
  in {
    # formatter = forAllSystems (pkgs: pkgs.nixpkgs-fmt);

    # packages = forAllSystems (pkgs: rec {
    #   default = typst;
    #   typst-dev = default;
    # });

    # overlayAttrs = builtins.removeAttrs self'.packages [ "default" ];

    # apps = forAllSystems (pkgs: {
    #   default = {
    #     type = "app";
    #     program = pkgs.lib.getExe typst;
    #   };
    # });

    devShells = forAllSystems ({ pkgs, lib, ... }: {
      default = pkgs.mkShell {
        packages = with pkgs; [
          (rust-bin.selectLatestNightlyWith (toolchain: toolchain.default))
          (writeShellScriptBin "testit" ''
            cargo test --workspace --test tests -- "$@"
          '')
        ];

        buildInputs = (lib.optionals pkgs.stdenv.isDarwin [
          pkgs.darwin.apple_sdk.frameworks.CoreServices
          pkgs.libiconv
        ]) ++ [
          pkgs.openssl
        ];

        nativeBuildInputs = [
          pkgs.pkg-config
          pkgs.openssl.dev
        ];
      };
    });
  };

# # Generate the typst package for the given nixpkgs instance.
# packageFor = pkgs:
#   let
#     inherit (lib)
#       importTOML
#       optionals
#       sourceByRegex
#       ;
#     Cargo-toml = importTOML ./Cargo.toml;

#     pname = "typst";
#     version = Cargo-toml.workspace.package.version;

#     # Crane-based Nix flake configuration.
#     # Based on https://github.com/ipetkov/crane/blob/master/examples/trunk-workspace/flake.nix
#     craneLib = crane.mkLib pkgs;

#     # Typst files to include in the derivation.
#     # Here we include Rust files, docs and tests.
#     src = sourceByRegex ./. [
#       "(docs|crates|tests)(/.*)?"
#       ''Cargo\.(toml|lock)''
#       ''build\.rs''
#     ];

#     # Typst derivation's args, used within crane's derivation generation
#     # functions.
#     commonCraneArgs = {
#       inherit src pname version;

#       buildInputs = (optionals pkgs.stdenv.isDarwin [
#         pkgs.darwin.apple_sdk.frameworks.CoreServices
#         pkgs.libiconv
#       ]) ++ [
#         pkgs.openssl
#       ];

#       nativeBuildInputs = [
#         pkgs.installShellFiles
#         pkgs.pkg-config
#         pkgs.openssl.dev
#       ];
#     };

#     # Derivation with just the dependencies, so we don't have to keep
#     # re-building them.
#     cargoArtifacts = craneLib.buildDepsOnly commonCraneArgs;
#   in
#   craneLib.buildPackage (commonCraneArgs // {
#     inherit cargoArtifacts;

#     postInstall = ''
#       installManPage crates/typst-cli/artifacts/*.1
#       installShellCompletion \
#         crates/typst-cli/artifacts/typst.{bash,fish} \
#         --zsh crates/typst-cli/artifacts/_typst
#     '';

#     GEN_ARTIFACTS = "artifacts";
#     TYPST_VERSION =
#       let
#         rev = self.shortRev or "dirty";
#         version = (builtins.fromTOML (builtins.readFile ./Cargo.toml)).workspace.package.version;
#       in
#       "${version} (${rev})";

#     meta.mainProgram = "typst";
#   });

# typst = packageFor pkgs;

}
