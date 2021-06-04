# packages/uhk-agent -- UHK configuration agent overlay
#
# Ref: https://discourse.nixos.org/t/ultimate-hacking-keyboard-firmware-update/3001/6

{ config, lib, pkgs, stdenv, makeDesktopItem, ... }:

let
  pname = "uhk-agent";
  name = "${pname}-${version}";

  # version >1.3.0 causes uhk-agent to hang on launch
  # "Loading keyboard configuration. Hang tight!") -- literally tight.
  version = "1.5.11"; # Change at your own risk.

  src = builtins.fetchurl {
    url = "https://github.com/UltimateHackingKeyboard/agent/releases/download/v${version}/UHK.Agent-${version}-linux-x86_64.AppImage";
    sha256 = {
      "1.2.12" = "1gr3q37ldixcqbwpxchhldlfjf7wcygxvnv6ff9nl7l8gxm732l6";
      "1.3.0" =  "09k09yn0iyivc9hf283cxrcrcyswgg2jslc85k4dwvp1pc6bpp07";
      "1.3.1" =  "0inps9q6f6cmlnl3knmfm2mmgqb5frl4ghxplbzvas7kmrd2wg4k";
      "1.3.2" =  "1y2n2kkkkqsqxw7rsya7qxh8m5nh0n93axcssi54srp3h7040w3h";
      "1.4.0" =  "1y6gy3zlj0pkvydby7ibm7hx83lmc3vs2m0bfww5dq1114j99dy5";
      "1.4.5" =  "1nimb8ab7p478p8xpa5lkdddwr1g59cp9jly167fc47gqq8zs7kl";
      "1.5.0" =  "1kwp133ipxd5al9jf0v40grpnpyiqvz95yydv9rylagxllcvr2s4";
      "1.5.11" =  "f6669c8b3a95d73415e58a13d4364e8475ee70e49c1dc7632cbffece6cb980f5";
    }."${version}";
  };

  desktopItem = makeDesktopItem {
    name = pname;
    desktopName = "UHK Agent";
    genericName = "Keyboard configuration";
    comment = "Agent is the configuration application of the Ultimate Hacking Keyboard";
    icon = "uhk-keyboard";
    terminal = "false";
    exec = pname;
    categories = "Utility;";
  };

  xdgDirs = builtins.concatStringsSep ":" [
    "${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}"
    "${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}"
    "$XDG_DATA_DIRS"
  ];

  appimageContents = pkgs.appimageTools.extractType2 {
    inherit name src;
  };

in pkgs.appimageTools.wrapType2 rec {
  inherit name src;
  
  # Uncomment in case debugging is necessary {{
  # runScript = pkgs.writeScript "run" ''
  #   #!${pkgs.stdenv.shell}

  #   export APPDIR=${pkgs.appimageTools.extractType2 { inherit name src; }}
  #   export APPIMAGE_SILENT_INSTALL=1

  #   # >>> inspect the script running environment here <<<
  #   echo "INSPECT: ''${GIO_EXTRA_MODULES:-no extra modules!}"
  #   echo "INSPECT: ''${GSETTINGS_SCHEMA_DIR:-no schemas!}"
  #   echo "INSPECT: ''${XDG_DATA_DIRS:-no data dirs!}"

  #   cd $APPDIR
  #   exec ./AppRun "$@"
  # '';
  # }} Uncomment in case debugging is necessary

  multiPkgs = null;

  # Borrows Electron packages from Atom
  # Ref: https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/editors/atom/env.nix
  extraPkgs = pkgs: with pkgs; atomEnv.packages ++ [
    pciutils
    libusb1

    # Additional electron dependencies (pinning version)
    at-spi2-atk
    at-spi2-core
    xorg.libxshmfence
    libdrm
    libxkbcommon
    mesa
  ];
  
  profile = ''
    export XDG_DATA_DIRS="${xdgDirs}"
    export APPIMAGE=''${APPIMAGE-""} # Kill a seemingly useless error message
  '';

  meta = with lib; {
    description = ''
      Agent is the configuration application of the Ultimate Hacking Keyboard
    '';
    
    longDescription = ''
      The Ultimate Hacking Keyboard is a split mechanical keyboard which utilizes
      Cherry MX-style switches. It's also a fully programmable keyboard which
      can be vastly customized through this agent for your needs.
    ''; # adapted from https://ultimatehackingkeyboard.com/

    homepage = https://ultimatehackingkeyboard.com/start/agent;
    license = licenses.unfreeRedistributable;
    maintainers = with maintainers; [ macunha1 ];
    platforms = [ "i386-linux" "x86_64-linux" ];
  };
}
