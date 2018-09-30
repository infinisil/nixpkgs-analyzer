{ path ? [] }:
let
  pkgs = import ~/src/nixpkgs {
    config = {
      allowUnfree = true;
      allowUnsupportedSystem = true;
      allowBroken = true;
    };
  };
  inherit (pkgs) lib;
  target = lib.attrByPath path null pkgs;

  /*
  We may assume the value evaluates without errors
  If the value is a derivation we may assume it instantiates

  If no such attribute path: Error out
  

  */
in 
  if lib.isAttrs target then
    if lib.isDerivation target then {
      tag = "derivation";
      derivation = target;
    } else {
      tag = "attrs";
      names = builtins.attrNames target;
    }
  else {
    tag = "other";
    nixType = builtins.typeOf target;
  }
