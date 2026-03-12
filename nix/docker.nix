{
  pkgs,
  hoardExe,
  deployment,
}:
let
  configFiles = {
    dev = ../config/dev.yaml;
    staging = ../config/staging.yaml;
    prod = ../config/prod.yaml;
    ci = ../config/ci.yaml;
  };

  networkConfigs = {
    dev = ../config/preprod;
    staging = ../config/preprod;
    prod = ../config/preprod; # TODO: switch to ../config/mainnet when ready
    ci = ../config/preprod;
  };

  configFile = configFiles.${deployment} or (throw "Unknown deployment: ${deployment}");
  networkConfig = networkConfigs.${deployment};
  networkName = builtins.baseNameOf (toString networkConfig);

  configDir = pkgs.runCommand "hoard-config-${deployment}" { } ''
    mkdir -p $out/app/config
    cp ${configFile} $out/app/config/${deployment}.yaml
    cp -r ${networkConfig} $out/app/config/${networkName}
  '';
in
pkgs.dockerTools.buildLayeredImage {
  name = "cardano-hoarding-node";
  tag = deployment;

  contents = [
    hoardExe
    configDir
    pkgs.cacert
    pkgs.tzdata
    pkgs.dockerTools.fakeNss
  ];

  config = {
    Cmd = [
      "${hoardExe}/bin/hoard-exe"
      "--env"
      deployment
    ];
    WorkingDir = "/app";
    Env = [
      "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
      "NIX_SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
    ];
  };
}
