let pkgs = import ./nix {}; in

with rec
{
  runTest = test: pkgs.runCommand "test"
    { buildInputs = [ pkgs.netcat pkgs.curl pkgs.haskellPackages.wai-app-static ] ;}
    test;

  runPort = port:
    ''
    trap 'echo $SRV_TEST_PIDS | xargs kill -9' EXIT
    SRV_TEST_PIDS="''${SRV_TEST_PIDS:-""}"
    warp -d ${apiPort port} -p ${builtins.toString port} &
    SRV_TEST_PIDS="$SRV_TEST_PIDS $!"
    while ! nc -z 127.0.0.1 ${builtins.toString port}; do
      echo waiting for port ${builtins.toString port}
      sleep 1
    done
    '';

  apiPort = port: pkgs.writeTextFile
    { text = builtins.toString port ;
      name = "someAPI";
    };
};

let tests =
{
  # Accessing localhost on port 80 reroutes to port 1234
  test_portMap =
    ''
      ${runPort 1234}

      SRV_MAP="*:80:*:1234" \
        LD_PRELOAD="${pkgs.surveyor}/lib/surveyor.so" \
         curl localhost | grep -q '1234'

      touch $out
    '';

  # Accessing 127.0.0.2 reroutes to 127.0.0.1
  test_addrMap =
    ''
      ${runPort 1234}

      SRV_MAP="127.0.0.2:*:127.0.0.1:*" \
        LD_PRELOAD="${pkgs.surveyor}/lib/surveyor.so" \
         curl 127.0.0.2:1234 | grep -q '1234'

      touch $out
    '';

  # Accessing anything on port 1234 reroutes to 1235 and
  # accessing anything on port 1235 reroutes to 1234
  test_swapPorts =
    ''
      ${runPort 1234}
      ${runPort 1235}

      export SRV_MAP="*:1234:*:1235 *:1235:*:1234"

      LD_PRELOAD="${pkgs.surveyor}/lib/surveyor.so" \
          curl localhost:1234 | grep -q '1235'
      LD_PRELOAD="${pkgs.surveyor}/lib/surveyor.so" \
          curl localhost:1235 | grep -q '1234'

      touch $out
    '';
}; in

{ inherit (pkgs) surveyor;
  inherit test_portMap;
} // (builtins.mapAttrs (k: v: runTest v) tests)
