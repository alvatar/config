{ config, pkgs, lib, ... }:

{
  ## Packages
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowBroken = true;
  environment = {
    variables = {
      EDITOR = pkgs.lib.mkOverride 0 "vim";
      XCURSOR_SIZE = "64";
      GDK_SCALE = "2";
      GDK_DPI_SCALE = "0.5";
      QT_AUTO_SCREEN_SCALE_FACTOR = "1";
      #_JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=lcd";
      _JAVA_AWT_WM_NONREPARENTING = "1";
    };
    systemPackages = with pkgs; let
      nvidia-chromium = pkgs.writeScriptBin "nvidia-chromium" ''
        nvidia-offload chromium 
      '';
    in [
      # Utils
      wget tmux vim emacs git git-lfs zsh gnumake htop tree p7zip zip unzip file killall
      silver-searcher nload iftop iotop nmap appimage-run openssl wipe groff steam-run
      lsof ecryptfs ecryptfs-helper encfs direnv tcpdump pstree ffmpeg-full unrar hfsprogs
      # Hardware utils
      lm_sensors acpitool pciutils glxinfo powertop tlp s-tui cpufrequtils pulseaudio-modules-bt
      # Browsers
      firefox chromium
      # GUI base
      picom rxvt_unicode urxvt_perls dmenu unclutter dunst autocutsel libnotify vanilla-dmz
      capitaine-cursors stalonetray xorg.xmodmap xorg.xev xorg.libxshmfence hicolor-icon-theme maim 
      pavucontrol gtk2 cbatticon imagemagick xdotool xclip xorg.xwininfo xorg.xkill
      # GUI programs
      mplayer vlc zathura imv mupdf gimp 
      xfce.ristretto xfce.tumbler xfce.xfce4-screenshooter xfce.thunar-bare 
      transmission-gtk networkmanagerapplet calibre nicotine-plus soulseekqt imgcat
      anki texlive.combined.scheme-full nicotine-plus signal-desktop xournal okular
      element-desktop alarm-clock-applet wireshark slack uhk-agent
      tdesktop discord
      rclone rclone-browser
      # Unfree
      spotify
      # Language
      gcc openjdk11 python37Full leiningen nodejs
      go_1_16
      rustup 
      # Global Python packages
      python37Packages.pip python37Packages.pylint python37Packages.black
      # Databases
      postgresql_12 apacheKafka_2_4 
      # Development tools
      jq
      # Development libraries
      protobuf binutils.bintools llvm clang llvmPackages.libclang libudev
      # DevOps
      docker-compose kubectl minikube kustomize k9s
      nvidia-docker
      # Custom bins
      nvidia-chromium
    ] ++ [ config.boot.kernelPackages.cpupower ];
  };

  ## Overlays
  nixpkgs.overlays = [
  (self: super:
  {
    uhk-agent = super.callPackage ./packages/uhk-agent.nix { };
    pcloud = super.callPackage ./packages/pcloud.nix { };
  }
  )];

  ## Boot

  boot = {
    loader = {
      systemd-boot.enable = lib.mkDefault true;
      efi.canTouchEfiVariables = lib.mkDefault true;
    };
    supportedFilesystems = ["ecryptfs" "zfs"];
    extraModulePackages = with config.boot.kernelPackages; [ zfs ];
    #kernelParams = lib.mkDefault [ "acpi_rev_override" ]; 
  };

  ## Hardware

  imports =
    [
       <nixos-hardware/dell/xps/15-9500/nvidia>
      ./hardware-configuration.nix
    ];

  hardware.opengl = {
    enable = true;
    setLdLibraryPath = true;
    driSupport32Bit = true;
  };

  hardware.enableAllFirmware = true;
  hardware.bluetooth = {
    enable = true;
    settings = {
      General = {
        Enable = "Source,Sink,Media,Socket";
      };
    };
  };
  services.blueman.enable = true;

  # Power management
  powerManagement = {
    enable = true;
    #cpuFreqGovernor = lib.mkDefault "ondemand";
    cpuFreqGovernor = "ondemand";
    powertop.enable = false;
  };

  services.tlp = {
    enable = true;
    settings = {
      "CPU_SCALING_GOVERNOR_ON_AC"= "performance";
      "CPU_SCALING_GOVERNOR_ON_BAT"= "performance";
      #"ENERGY_PERF_POLICY_ON_BAT"= "power";
      "CPU_ENERGY_PERF_POLICY_ON_AC"="performance";
      "CPU_ENERGY_PERF_POLICY_ON_BAT"="balance_power";
      "SOUND_POWER_SAVE_ON_BAT"= 0;
      "SOUND_POWER_SAVE_CONTROLLER"= "Y";
      "START_CHARGE_THRESH_BAT0"= 40;
      "STOP_CHARGE_THRESH_BAT0"= 80;
      # Runtime Power Management for PCI(e) bus devices: on=disable, auto=enable.
      "RUNTIME_PM_ON_AC"= "on";
      "RUNTIME_PM_ON_BAT"= "auto";
    };
  };

  # Virtualization
  virtualisation.docker = {
    enable = true;
    enableNvidia = true;
  };
  virtualisation.virtualbox.host.enable = true;

  # Networking
  networking = {
    hostName = "arctic";
    hostId = "71c88239";
    networkmanager.enable = true;
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
    interfaces.wlp0s20f3.useDHCP = true;
    # Open ports in the firewall.
    firewall = {
      allowedTCPPorts = [ 17500 62571 62572 ];
      allowedUDPPorts = [ 17500 62571 62572 ];
    };
    # Or disable the firewall altogether.
    # networking.firewall.enable = false;
  };

  # Sound
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    package = pkgs.pulseaudioFull;
    extraConfig = "
load-module module-udev-detect tsched=0
load-module module-switch-on-connect
load-module module-bluetooth-discover a2dp_config=\"ldac_eqmid=hq sbc_min_bp=53 sbc_min_bp=53\"
    ";
  };

  # Udev rules for Ultimate Hacking Keyboard
  services.udev.extraRules = ''
      # uhk
      SUBSYSTEM=="input", GROUP="input", MODE="0666"
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="612[0-7]", MODE:="0666", GROUP="plugdev"
      KERNEL=="hidraw*", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="612[0-7]", MODE="0666", GROUP="plugdev"
'';

  ## Environment

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users = {
    defaultUserShell = pkgs.zsh;
    users = {
      alvatar = {
        isNormalUser = true;
        extraGroups = [ "wheel" "video" "networkmanager" "docker" "adbusers"];
        initialPassword = "1234";
      };
    };
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
    #font = "Lat2-Terminus16";
    #keyMap = "us";
    useXkbConfig = true;
  };

  time.timeZone = "Europe/Tallinn";

  location = {
    provider = "manual";
    latitude = 40.4;
    longitude = -3.4;
  }; 
  services.redshift = {
    enable = true;
    brightness = {
      day = "1";
      night = "1";
    };
    temperature = {
      day = 6500;
      night = 3700;
    };
  };

  services.xserver = {
    enable = true;
    layout = "us";
    dpi = 180;
    xkbOptions = "eurosign:e grp:alt_space_toggle, ctrl:swapcaps";
    # Enable touchpad support.
    libinput = {
      enable = true;
      touchpad = {
        accelSpeed = "16.0";
        naturalScrolling = true;
        clickMethod = "clickfinger";
        tapping = false;
        disableWhileTyping = true;
      };
      mouse = {
        naturalScrolling = true;
      };
   };
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
        haskellPackages.xmonad
      ];
    };
    displayManager = let
      myCustomLayout = pkgs.writeText "xkb-layout"
      ''
        keycode 10 = 1 exclam exclamdown
        keycode 14 = 5 percent EuroSign
        keycode 61 = slash question questiondown
        keycode 38 = a A aacute Aacute ae AE ae
        keycode 26 = e E eacute Eacute EuroSign cent EuroSign
        keycode 30 = u U uacute Uacute downarrow uparrow downarrow
        keycode 31 = i I iacute Iacute rightarrow idotless rightarrow
        keycode 32 = o O oacute Oacute oslash Oslash oslash
        keycode 57 = n N ntilde Ntilde n N n
        keycode 54 = c C cent
        ! Maps the Mode key to the right Alt key
        keycode 108 = Mode_switch
        ! Turns Control_R into AltGr (not working, but should)
        remove control = Control_R
        clear mod5
        add mod5 = Control_R
        keycode 105 = ISO_Level3_Shift
      '';
    in
    {
      defaultSession = "none+xmonad";
      sddm.enableHidpi = true;
      sessionCommands = with pkgs; lib.mkAfter
      ''
      ${pkgs.xorg.xmodmap}/bin/xmodmap ${myCustomLayout}
      ${xlibs.xsetroot}/bin/xsetroot -xcf ${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ/cursors/left_ptr 128 &disown
      if test -e $HOME/.Xresources; then
        ${pkgs.xorg.xrdb}/bin/xrdb -merge $HOME/.Xresources &disown
      fi
      stalonetray -i 48 -bg '#ffffff' --kludges=force_icons_size,fix_window_pos  &disown
      blueman-applet &disown
      cbatticon &disown
      nm-applet &disown
      '';
    }; 
  };

  fonts.fonts = with pkgs; [
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    liberation_ttf
    fira-code
    fira-code-symbols
    mplus-outline-fonts
    dina-font
    proggyfonts
    hack-font
    iosevka
    symbola
  ];

  programs.ssh.startAgent = true;
  programs.slock.enable = true;
  programs = {
    zsh.ohMyZsh = {
      enable = true;
      plugins = [ "git" "python" "man" ];
      theme = "agnoster";
    };
    light.enable = true;
  };  

  services.avahi = {
    nssmdns = true;
    enable = true;
    ipv4 = true;
    ipv6 = true;
    publish = {
      enable = true;
      addresses = true;
      workstation = true;
    };
  };

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_12;
    enableTCPIP = true;
    authentication = pkgs.lib.mkOverride 10 ''
      local all all trust
      host all all ::1/128 trust
      host all all 0.0.0.0/0 trust
    '';
  };

  services.apache-kafka = {
    enable = false;
    extraProperties = ''
offsets.topic.replication.factor = 1
delete.topic.enable = true
    '';
  };

  services.zookeeper = {
    enable = false;
  };

  ## Systemd

  systemd.user.services.dropbox = {
    enable = false;
    description = "Dropbox";
    wantedBy = [ "graphical-session.target" ];
    environment = {
      QT_PLUGIN_PATH = "/run/current-system/sw/" + pkgs.qt5.qtbase.qtPluginPrefix;
      QML2_IMPORT_PATH = "/run/current-system/sw/" + pkgs.qt5.qtbase.qtQmlPrefix;
    };
    serviceConfig = {
      ExecStart = "${pkgs.dropbox.out}/bin/dropbox";
      ExecReload = "${pkgs.coreutils.out}/bin/kill -HUP $MAINPID";
      KillMode = "control-group"; # upstream recommends process
      Restart = "on-failure";
      PrivateTmp = true;
      ProtectSystem = "full";
      Nice = 10;
    };
  };

  systemd.user.services."urxvtd" = {
    enable = true;
    description = "rxvt unicode daemon";
    wantedBy = [ "default.target" ];
    path = [ pkgs.rxvt_unicode ];
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.rxvt_unicode}/bin/urxvtd -q -o";
  };
 
  systemd.user.services."dunst" = {
    enable = true;
    description = "";
    wantedBy = [ "default.target" ];
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.dunst}/bin/dunst";
  };

  systemd.user.services."unclutter" = {
    enable = true;
    description = "hide cursor after X seconds idle";
    wantedBy = [ "default.target" ];
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.unclutter}/bin/unclutter";
  };
 
  systemd.user.services."picom" = {
    enable = true;
    description = "";
    wantedBy = [ "default.target" ];
    path = [ pkgs.picom ];
    serviceConfig.Type = "forking";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.picom}/bin/picom -b";
    ## serviceConfig.ExecStart = "${pkgs.picom}/bin/picom -b --config /home/alvatar/.picom.conf";
  };
  
   #systemd.user.services."autocutsel" = {
    #enable = true;
    #description = "AutoCutSel";
    #wantedBy = [ "default.target" ];
    #serviceConfig.Type = "forking";
    #serviceConfig.Restart = "always";
    #serviceConfig.RestartSec = 2;
    #serviceConfig.ExecStartPre = "${pkgs.autocutsel}/bin/autocutsel -fork";
    #serviceConfig.ExecStart = "${pkgs.autocutsel}/bin/autocutsel -selection PRIMARY -fork";
  #};

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}

