{
  description = "A flake for todolist";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";
  };
  outputs = inputs @ { self, nixpkgs, flake-parts, systems }:
  flake-parts.lib.mkFlake { inherit inputs; } {
    systems = import systems;
    perSystem = { pkgs, ... }: let
      ############ Settings ############
      ## Project name
      pname = "todolist";
      ## Project version
      version = "0.0.0";
      ## Source directory
      src = ./.;
      ## Exported systems
      systems = [pname "${pname}/test"];
      ## Dependencies
      lispLibs = lisp: with lisp.pkgs; [
        ningle
        clack
        clack-handler-hunchentoot
        hunchentoot
        spinneret
        parachute
        cl-ppcre
      ];
      ## Non-Lisp dependencies
      nativeLibs = with pkgs; [
        openssl
      ];
      ## Supported Lisp implementations
      lispImpls = [
        "sbcl"
      #  "abcl"
        "ecl"
      ];
      ##################################
      LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath nativeLibs;
      recipe = {
        sbcl = rec {
          mainLib = pkgs.sbcl.buildASDFSystem {
            inherit pname version src systems nativeLibs;
            lispLibs = lispLibs pkgs.sbcl;
          };
          mainExe = let app = pkgs.sbcl.withPackages (ps: lispLibs pkgs.sbcl); in
            pkgs.stdenv.mkDerivation {
              inherit pname version src;
              meta.mainProgram = pname;
              nativeBuildInputs = [app];
              dontStrip = true;
              buildPhase = ''
                export HOME=$TMPDIR
                export CL_SOURCE_REGISTRY=$src
                export CL_BUILD_PATHNAME=`realpath -s --relative-to=$src $TMPDIR/${pname}_raw`
                export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
                ${app}/bin/sbcl --noinform --non-interactive --eval "(require :asdf)" --eval "(asdf:make :${pname})"
              '';
              installPhase = ''
                install -D $CL_BUILD_PATHNAME $out/bin/${pname}_raw
                cat > $out/bin/${pname} <<-EOF
                  #!/bin/sh
                  export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
                  exec $out/bin/${pname}_raw "\$@"
                EOF
                chmod +x $out/bin/${pname}
              '';
            };
          testExe = let app = pkgs.sbcl.withPackages (ps: [mainLib]); in
            pkgs.writeShellScriptBin "${pname}-test" ''
              export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
              ${app}/bin/sbcl --noinform --non-interactive --eval "(require :asdf)" --eval "(asdf:test-system :${pname})"
            '';
        };
        abcl = rec {
          mainLib = pkgs.abcl.buildASDFSystem {
            inherit pname version src systems nativeLibs;
            lispLibs = lispLibs pkgs.abcl;
          };
          mainExe = let app = pkgs.abcl.withPackages (ps: [mainLib]); in
            pkgs.writeShellScriptBin pname ''
              export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
              ${app}/bin/abcl --noinform --eval "(require :asdf)" --eval "(asdf:load-system :${pname})" --eval "(${pname}:main)" --eval "(quit)" -- "$@"
            '';
          testExe = let app = pkgs.abcl.withPackages (ps: [mainLib]); in
            pkgs.writeShellScriptBin "${pname}-test" ''
              export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
              ${app}/bin/abcl --noinform --eval "(require :asdf)" --eval "(asdf:test-system :${pname})" --eval "(quit)"
            '';
        };
        ecl = rec {
          mainLib = pkgs.ecl.buildASDFSystem {
            inherit pname version src systems nativeLibs;
            lispLibs = lispLibs pkgs.ecl;
          };
          mainExe = let app = pkgs.ecl.withPackages (ps: [mainLib]); in
            pkgs.writeShellScriptBin pname ''
              export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
              ${app}/bin/ecl --eval "(require :asdf)" --eval "(asdf:load-system :${pname})" --eval "(${pname}:main)" --eval "(quit)" -- "$@"
            '';
          testExe = let app = pkgs.ecl.withPackages (ps: [mainLib]); in
            pkgs.writeShellScriptBin "${pname}-test" ''
              export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
              ${app}/bin/ecl --eval "(require :asdf)" --eval "(asdf:test-system :${pname})" --eval "(quit)"
            '';
        };
      };
      apps =  impl: [
        { name = "main-" + impl; value = { type = "app"; program = recipe.${impl}.mainExe; }; }
        { name = "test-" + impl; value = { type = "app"; program = recipe.${impl}.testExe; }; }
      ];
      packages = impl: { name = "lib-" + impl; value = recipe.${impl}.mainLib; };
      devPackages = impl: pkgs.${impl}.withPackages (ps: [recipe.${impl}.mainLib]);
    in {
      devShells.default = pkgs.mkShell {
        inherit LD_LIBRARY_PATH;
        shellHook = ''export CL_SOURCE_REGISTRY=$PWD'';
        packages = builtins.map devPackages lispImpls;
      };
      packages = builtins.listToAttrs (builtins.map packages lispImpls);
      apps = builtins.listToAttrs (builtins.concatMap apps lispImpls);
    };
  };
}
