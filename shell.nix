# Original author: Walt O'Connor
# Modified towards a minimal p4 dev environment by Chris
#
# Should provide the three key components:
# 1. p4c -- for compiling p4 programs into json configs
# 2. simple_switch -- a "switch" for interpreting json configs
# 3. bm_CLI -- a cli for controlling the "switch"
#
# Seems like a static config given as a list of CLI commands can by applied simply by piping it into bm_CLI.
# bm_CLI reads the state of the running "switch" on each launch so you can apply a config, then go in and inspect / update it.
#

with (import <nixpkgs> {});
let
  python-with-my-packages = python3.withPackages (p: with p; [
    mininet-python
    thrift
    scapy
    nanomsg-python
  ]);
  PI = (import ./nix_build/build_pi.nix);
  p4c = (import ./nix_build/build_p4c.nix);
  bmv2 = (import ./nix_build/build_bmv2.nix);
in
mkShell {
  buildInputs = [
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
    PI
    libpcap
    grpc
    p4c
    bmv2
  ];

  shellHook = ''
  '';
}
