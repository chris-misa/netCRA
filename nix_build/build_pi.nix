# Original author: Walt O'Connor

with (import <nixpkgs> {});
pkgs.stdenv.mkDerivation rec {
    pname = "p4_libpi";
    version = "0.1.0";

    src = pkgs.fetchFromGitHub {
        owner = "p4lang";
        repo = "PI";
        rev = "66da143288d4f02f17cd6a63d0f4b2a87b011499";
        sha256 = "03xrcn10dmwm1wrxsihk1b4s5sn7j8rp6x1vmqjv5m698ss9m4ph";
        fetchSubmodules = true;
    };

    buildInputs = with pkgs; [
    gnumake
    autoconf
    automake
    cmake
    git
    gcc
    libtool
    pkg-config
    protobuf3_10
    grpc
    openssl
    boost
    boost_process
    ];

    configurePhase = ''
        ./autogen.sh
        ./configure --with-proto --without-internal-rpc --without-cli --without-bmv2 --with-boost-libdir=${boost}/lib --bindir=$out/bin --libdir=$out/lib --datadir=$out/data --includedir=$out/include
    '';

    buildPhase = ''
        make -j 16
    '';

    installPhase = ''
        mkdir -p $out/{lib,bin,include,data}
        make install
    '';
}
