{ pkgs ? import ./packages.nix { system = "x86_64-linux"; } }:

let
  bin = (pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.geocode-city-api);
  migrations = ../migrations;
in

# This is the nix api to build images
pkgs.dockerTools.buildImage {
  # our image name
  name = "geocode-city-api";
  # our image tag
  tag = "latest";
  
  # this is a list of the things we want to include
  # in the image. it's incredibly bare by default.
  contents = [
    bin # our app
  ];
  fromImage = pkgs.dockerTools.pullImage {
    imageName = "alpine";
    imageDigest = "sha256:e1871801d30885a610511c867de0d6baca7ed4e6a2573d506bbec7fd3b03873f";
    sha256 = "0ymhp3hrhpf7425n3awz6b67510x9xcgpldi4xm610aqfk1rygy9";
  };
  # from: https://blog.codeaddict.org/2020/06/01/build-a-nix-docker-image-in-gitlab-ci/
  # copy the config folder in its entirety, also link the nix-provided tzdata
  # to the standard location as expected (perhaps erroneously) by timezone-detect:
  # https://github.com/lfborjas/timezone-detect/blob/6c3d7954431b63c07c1b0018a364897e42080e54/src/Data/Time/LocalTime/TimeZone/Detect.hs#L123
  extraCommands = ''
    cp -rf ${migrations} migrations
  '';

  # This exposes the Dockerfile commands you might be familiar with
  config = {
    Cmd = [ "${bin}/bin/geocode-city-api-exe" ];
    Env = [ 
      "DEPLOY_ENV=Production"
    ];
  };
}
