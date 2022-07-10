{system ? builtins.currentSystem}:
# NOTE(luis)
# All packages listed here will be built from source, as they're not
# 'blessed' in our pinned nix version. The sources themselves _are_
# obtained from cache.nixos.org, but need to be rebuilt regardless.
# Exercise restraint!
let
  dontCheck   = (import ./packages.nix{inherit system;}).haskell.lib.dontCheck;
  doJailbreak = (import ./packages.nix{inherit system;}).haskell.lib.doJailbreak;
in (super: {
  #servant = super.servant_0_18;
  #servant-server = super.servant-server_0_18;
  #fused-effects = super.fused-effects_1_1_0_0;
  # don't check: uses a version of `tasty` not available in our nix pin:
  fused-effects  = dontCheck super.fused-effects;
  # Had to jailbreak due to dependency on old version of bytestring
  postgresql-simple-migration = doJailbreak super.postgresql-simple-migration;
  hs-opentelemetry-sdk = super.callPackage ./extra-pkgs/hs-opentelemetry-sdk.nix {};
  hs-opentelemetry-api = super.callPackage ./extra-pkgs/hs-opentelemetry-api.nix {};
  hs-opentelemetry-exporter-otlp = super.callPackage ./extra-pkgs/hs-opentelemetry-exporter-otlp.nix {};
  hs-opentelemetry-propagator-w3c = super.callPackage ./extra-pkgs/hs-opentelemetry-propagator-w3c.nix {};
  hs-opentelemetry-otlp = super.callPackage ./extra-pkgs/hs-opentelemetry-otlp.nix {};
})
