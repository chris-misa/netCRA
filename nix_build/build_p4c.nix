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
    pname = "p4c";
    version = "1.2.2.1";

    src = builtins.fetchTarball {
        url = "https://github.com/p4lang/p4c/archive/refs/tags/v1.2.2.1.tar.gz";
        sha256 = "1jp1psbwf1jp75zi6m8mlzwwbywsf6vhis2zn30pwdkfwjxsyqg2";
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
        git
        cmake
        protobuf3_10
        doxygen
        graphviz
        gcc
        boehmgc
        python-with-my-packages
    ];

    configurePhase = ''
        mkdir build
        cd build
        cmake .. -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=$out -DENABLE_BMV2=ON -DENABLE_GC=ON -DENABLE_MULTITHREAD=ON 
    '';

    buildPhase = ''
        make -j 8
    '';

    installPhase = ''
        mkdir -p $out
        make install
    '';
}
