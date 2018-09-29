let
  nixpkgs = import <nixpkgs> {};
in with nixpkgs;
let
  haskellPackages = haskell.packages.ghc843.override ({
    overrides = self: super: {
      clash-prelude = haskell.lib.dontCheck super.clash-prelude;
    };
  });
  haskellEnv = haskellPackages.ghc.withHoogle (haskellPackages: with haskellPackages; [
    clash-prelude clash-ghc ghc-typelits-natnormalise ghc-typelits-knownnat ghc-typelits-extra
  ]);

  buildVerilog = { sourceFile, topEntity }:
    stdenv.mkDerivation {
      name = "verilog-${topEntity}";
      src = ./src;
      buildInputs = [
        haskellEnv
        arachne-pnr yosys icestorm
      ];
      buildPhase = ''
        clash ${sourceFile} --verilog
      '';
      installPhase = ''
        mkdir -p $out
        cp -R $(find verilog -iname '*.v') $out
      '';
    };

  buildBLIF = { src, sourceRoot ? null, topEntity}:
    stdenv.mkDerivation {
      name = "blif-${topEntity}";
      inherit src sourceRoot;
      buildInputs = [
        yosys icestorm
      ];
      buildPhase = ''
        mkdir -p $out
        yosys -p "synth_ice40 -top ${topEntity} -blif $out/blif" ${src}/*.v
      '';
      installPhase = '' echo done '';
    };

  buildPnR = { blifFile }:
    stdenv.mkDerivation {
      name = "pnr";
      buildInputs = [arachne-pnr];
      buildCommand = ''
        mkdir -p $out
        arachne-pnr -d 1k -P cb81 -o $out/pnr.asc ${blifFile}
      '';
    };

  icepack = { ascFile }:
    stdenv.mkDerivation {
      name = "icepack";
      buildInputs = [icestorm];
      buildCommand = ''
        mkdir -p $out
        icepack ${ascFile} $out/packed.bin
      '';
    };

  timingReport = { ascFile, deviceType }: 
    stdenv.mkDerivation {
      name = "timing-report";
      buildInputs = [icestorm];
      buildCommand = ''
        mkdir -p $out
        icetime -t -d ${deviceType} ${ascFile} > $out/timing.rpt
      '';
    };

  clashIcestormBuild = { sourceFile, topEntity, deviceType ? "lp1k" }:
    let verilog = buildVerilog { inherit sourceFile topEntity; };
    in { inherit verilog; } //
       icestormBuild { inherit verilog topEntity deviceType; };
       
  icestormBuild = { verilog, topEntity, deviceType ? "lp1k" }: rec {
    blif = buildBLIF {
      src = verilog;
      inherit topEntity;
    };
    pnr = buildPnR { blifFile = "${blif}/blif"; };
    packed = icepack { ascFile = "${pnr}/pnr.asc"; };
    report = timingReport { ascFile = "${pnr}/pnr.asc"; inherit deviceType; };
    icebox = stdenv.mkDerivation {
      name = "icebox";
      buildInputs = [icestorm];
      buildCommand = ''
        mkdir $out
        icebox_vlog ${pnr}/pnr.asc > $out/icebox
      '';
    };
  };

in
  rec {
    test = clashIcestormBuild {
      sourceFile = "Test.hs";
      topEntity = "test";
    };

    runEnv = buildEnv {
      name = "icestorm-env";
      paths = [arachne-pnr yosys icestorm];
    };

    shellEnv = 
      stdenv.mkDerivation {
        name = "shell-env";
        buildInputs = [
          haskellEnv
          arachne-pnr yosys icestorm
        ];
      };
  }
