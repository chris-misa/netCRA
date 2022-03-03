# Original author: Walt O'Connor
# Update by Chris to use release version and fewer threads/memory

let
  pkgs = import <nixpkgs> {};
  python-with-my-packages = pkgs.python3.withPackages (p: with p; [
    mininet-python
    thrift
    scapy
    nanomsg-python
  ]);
in
pkgs.stdenv.mkDerivation rec {
    pname = "bmv2";
    version = "1.15.0";

    src = builtins.fetchTarball {
        url = "https://github.com/p4lang/behavioral-model/archive/refs/tags/1.15.0.tar.gz";
        sha256 = "1cpqc80bnsgjkgv9afdwnjgkpr3zr9bq74cm1krbg38hhd2slwsx";
    };

    buildInputs = with pkgs; [
      gnumake
      autoconf
      automake
      gmp
      cmake
      boost
      libtool
      flex
      bison
      pkg-config
      gcc
      openssl
      thrift
      nanomsg
      libevent
      protobuf3_10
      python-with-my-packages
      libpcap
      grpc
    ];

    configurePhase = ''
        ./autogen.sh
        ./configure --prefix=$out
    '';

    buildPhase = ''
        make -j 8
    '';

    installPhase = ''
        mkdir -p $out
        make install
    '';
}
