let pkgs = import ./nix {}; in

with rec
{
  runTest = test: pkgs.runCommand "test"
    { buildInputs = [ pkgs.netcat pkgs.curl pkgs.haskellPackages.wai-app-static ] ;}
    (test + "\n" + "touch $out");

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


  setHosts = hosts:
    with rec
    { mkHost = host: "${pkgs.lib.elemAt host 0} ${pkgs.lib.elemAt host 1}";
      lines = map mkHost hosts;
    }; pkgs.writeText "hosts" (pkgs.lib.concatStringsSep "\n" lines);
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
    '';

  # Accessing 192.168.42.42 reroutes to 127.0.0.1
  test_addrMap =
    ''
      ${runPort 1234}

      SRV_MAP="192.168.42.42:*:127.0.0.1:*" \
        LD_PRELOAD="${pkgs.surveyor}/lib/surveyor.so" \
         curl 192.168.42.42:1234 | grep -q '1234'
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
    '';

  # Accessing www.example.com:80 reroutes to localhost:1234 and
  # accessing does.not.exist reroutes to localhost:1235
  test_redirectHosts =
    ''
      ${runPort 1234}
      ${runPort 1235}

      export SRV_MAP="0.0.0.42:80:127.0.0.1:1234 0.0.0.43:80:127.0.0.1:1235"
      export NIX_REDIRECTS=/etc/hosts=${setHosts
        [["0.0.0.42" "www.example.com"] ["0.0.0.43" "does.not.exist"]]}


      LD_PRELOAD="${pkgs.surveyor}/lib/surveyor.so ${pkgs.libredirect}/lib/libredirect.so" \
          curl www.example.com | grep -q '1234'
      LD_PRELOAD="${pkgs.surveyor}/lib/surveyor.so ${pkgs.libredirect}/lib/libredirect.so" \
          curl does.not.exist | grep -q '1235'
    '';
}; in

{ inherit (pkgs) surveyor;
  inherit test_portMap;
} // (builtins.mapAttrs (k: v: runTest v) tests)
