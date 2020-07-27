# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports =
    [
      (import "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos")
      ./hardware-configuration.nix
    ];

  ## Hardware config

  services.xserver.videoDrivers = lib.mkDefault ["nvidia"];
  hardware.nvidia.modesetting.enable = lib.mkDefault true;
  hardware.nvidia.optimus_prime.enable = lib.mkDefault true;
  hardware.nvidia.optimus_prime.nvidiaBusId = lib.mkDefault "PCI:1:0:0";
  hardware.nvidia.optimus_prime.intelBusId = lib.mkDefault "PCI:0:2:0";

  hardware.cpu.intel.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;
  
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  hardware.opengl.extraPackages = with pkgs; [
    vaapiIntel
    vaapiVdpau
    libvdpau-va-gl
    intel-media-driver
  ];

  # Power management
  services.tlp.enable = lib.mkDefault true;
  #powerManagement = {
    #enable = true;
    #cpuFreqGovernor = "ondemand";
  #};

  ## Boot
  boot = {
    loader = {
      systemd-boot.enable = lib.mkDefault true;
      efi.canTouchEfiVariables = lib.mkDefault true;
    };
    kernelParams = lib.mkDefault [ "acpi_rev_override" ]; 
  };

  ## Networking
  
  networking.hostName = "arctic"; # Define your hostname.
  #networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  # Open ports in the firewall.
  networking.firewall = {
    allowedTCPPorts = [ 17500 ];
    allowedUDPPorts = [ 17500 ];
  };
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  ## Environment

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
    #font = "Lat2-Terminus16";
    #keyMap = "us";
    useXkbConfig = true;
  };

  time.timeZone = "Europe/Madrid";

  nixpkgs.config.allowUnfree = true;

  environment = {
    variables = {
      EDITOR = pkgs.lib.mkOverride 0 "vim";
      XCURSOR_SIZE = "64";
      GDK_SCALE = "2";
      GDK_DPI_SCALE = "0.5";
      QT_AUTO_SCREEN_SCALE_FACTOR = "1";
      _JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=lcd";
    };
    systemPackages = with pkgs; [
      wget tmux vim emacs git zsh gnumake htop tree p7zip zip unzip file killall silver-searcher
      lm_sensors acpitool pciutils glxinfo powertop
      picom rxvt_unicode urxvt_perls dmenu unclutter dunst autocutsel libnotify vanilla-dmz capitaine-cursors
      stalonetray xorg.xmodmap xorg.xev xclip autokey hicolor-icon-theme pavucontrol
      firefox chromium
      nload iftop nmap
      mplayer vlc libreoffice zathura imv mupdf gimp darktable scribus xfce.ristretto
      spotify dropbox-cli zoom-us
      docker-compose protobuf
      go openjdk12 python3
      postgresql_12 apacheKafka_2_4
    ];
  };

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

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_12;
    enableTCPIP = true;
    authentication = pkgs.lib.mkOverride 10 ''
      local all all trust
      host all all ::1/128 trust
    '';
    #initialScript = pkgs.writeText "backend-initScript" ''
      #CREATE ROLE nixcloud WITH LOGIN PASSWORD 'nixcloud' CREATEDB;
      #CREATE DATABASE nixcloud;
      #GRANT ALL PRIVILEGES ON DATABASE nixcloud TO nixcloud;
    #'';
  };

  services.apache-kafka = {
    enable = true;
    extraProperties = ''
offsets.topic.replication.factor = 1
delete.topic.enable = true
    '';
  };

  services.zookeeper = {
    enable = true;
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  #   pinentryFlavor = "gnome3";
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;


  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    package = pkgs.pulseaudioFull;
  };

  services.xserver = {
    enable = true;
    layout = "us";
    dpi = 180;
    xkbOptions = "eurosign:e grp:alt_space_toggle, ctrl:swapcaps";
    # Enable touchpad support.
    libinput = {
      enable = true;
      accelSpeed = "4.0";
      naturalScrolling = true;
      clickMethod = "clickfinger";
      tapping = false;
      disableWhileTyping = true;
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
      autokey-gtk &disown
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
  ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users = {
    defaultUserShell = pkgs.zsh;
    users = {
      alvatar = {
        isNormalUser = true;
        extraGroups = [ "wheel" "video" "networkmanager" "docker" ];
        initialPassword = "1234";
      };
    };
  };

  home-manager.users.alvatar = {
    programs.git = {
      enable = true;
      userName  = "Alvaro Castro-Castilla";
      userEmail = "a@fourthbit.com";
      extraConfig = {
        url."ssh://git@host".insteadOf = "http://github.com";
      };
    };
    programs.go.enable = true;
  };

  ## Systemd

  # services.thermald = {
    # enable = lib.mkDefault true;
    # debug = true;
  # };

  systemd.user.services.dropbox = {
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
  
   systemd.user.services."autocutsel" = {
    enable = true;
    description = "AutoCutSel";
    wantedBy = [ "default.target" ];
    serviceConfig.Type = "forking";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStartPre = "${pkgs.autocutsel}/bin/autocutsel -fork";
    serviceConfig.ExecStart = "${pkgs.autocutsel}/bin/autocutsel -selection PRIMARY -fork";
  };

  ## Virtualization

  virtualisation.docker.enable = true;
  virtualisation.virtualbox.host.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

}

