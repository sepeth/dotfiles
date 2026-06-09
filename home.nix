{ pkgs, lib, ... }:
let
  dotfilesRepo = builtins.getEnv "DOTFILES_REPO";
  user = builtins.getEnv "USER";
  homeDir = builtins.getEnv "HOME";
  homebrewPaths = [
    "/opt/homebrew/bin"
    "/opt/homebrew/sbin"
  ];
in
{
  home.username = user;
  home.homeDirectory =
    if homeDir != "" then homeDir
    else if pkgs.stdenv.isDarwin then "/Users/${user}"
    else "/home/${user}";
  home.stateVersion = "24.05";

  programs.home-manager.enable = true;
  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      set -g fish_greeting

      if set -q SSH_TTY
        echo (prompt_hostname) "via SSH"
      end
    '';
    functions._tide_item_nix_shell = ''
      set -q IN_NIX_SHELL && _tide_print_item nix_shell $tide_nix_shell_icon
    '';
    plugins = [
      {
        name = "tide";
        src = pkgs.fishPlugins.tide.src;
      }
    ];
  };
  programs.zoxide = {
    enable = true;
    enableFishIntegration = true;
  };

  programs.direnv = {
    enable = true;
    config.global.load_dotenv = true;
    nix-direnv.enable = true;
    enableFishIntegration = true;
  };

  home.sessionPath = homebrewPaths;
  home.sessionVariables = {
    EDITOR = "nvim";
    VISUAL = "nvim";
    SUDO_EDITOR = "nvim";
  };

  home.packages = with pkgs; [
    git
    neovim
    openssh
    tmux
    vim
  ];

  xdg.configFile."fish/functions".source = ./fish/functions;
  xdg.configFile."fish/functions".recursive = true;
  xdg.configFile."fish/conf.d/homebrew-path.fish".source = ./fish/conf.d/homebrew-path.fish;

  home.file.".gitconfig".source = ./gitconfig;
  home.file.".tmux.conf".source = ./tmux.conf;
  home.file.".vimrc".source = ./vimrc;

  home.file.".vim/ftplugin".source = ./vim/ftplugin;
  home.file.".vim/ftplugin".force = true;
  home.file.".vim/snippets".source = ./vim/snippets;
  home.file.".vim/snippets".force = true;
  home.file.".vim/syntax".source = ./vim/syntax;
  home.file.".vim/syntax".force = true;

  home.activation.dotfilesSetup = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
    if [ -n "${dotfilesRepo}" ] && [ -d "${dotfilesRepo}/.git" ]; then
      ${pkgs.git}/bin/git -C "${dotfilesRepo}" submodule update --init

      mkdir -p "$HOME/.vim/bundle"
      if [ -d "${dotfilesRepo}/vim/bundle/Vundle.vim" ] && [ ! -e "$HOME/.vim/bundle/Vundle.vim" ]; then
        ln -s "${dotfilesRepo}/vim/bundle/Vundle.vim" "$HOME/.vim/bundle/Vundle.vim"
      fi
    fi

    if [ -x "${pkgs.vim}/bin/vim" ] && [ -d "$HOME/.vim/bundle/Vundle.vim" ]; then
      if [ ! -d "$HOME/.vim/bundle/molokai" ]; then
        export PATH="${pkgs.git}/bin:${pkgs.vim}/bin:$PATH"
        mkdir -p "$HOME/.cache"
        ${pkgs.vim}/bin/vim -E -s -u "$HOME/.vimrc" +PluginInstall +qall >"$HOME/.cache/vundle-install.log" 2>&1 || true
      fi
    fi

    if [ -x "${pkgs.git}/bin/git" ]; then
      export PATH="${pkgs.git}/bin:${pkgs.openssh}/bin:$PATH"
      nvim_dir="$ASTRONVIM_DIR"
      nvim_repo="$ASTRONVIM_REPO"
      if [ -n "$nvim_dir" ] && [ -n "$nvim_repo" ]; then
        if [ -d "$nvim_dir/.git" ]; then
          ${pkgs.git}/bin/git -C "$nvim_dir" pull --ff-only || true
        elif [ -e "$nvim_dir" ]; then
          echo "Existing $nvim_dir found; skipping AstroNvim config"
        else
          mkdir -p "$HOME/.config"
          ${pkgs.git}/bin/git clone "$nvim_repo" "$nvim_dir"
        fi
      else
        echo "ASTRONVIM_DIR/ASTRONVIM_REPO not set; skipping AstroNvim config"
      fi
    fi
  '';

  home.activation.tideBootstrap = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    fish_variables="$HOME/.config/fish/fish_variables"

    if ! grep -q '^SETUVAR tide_left_prompt_items:' "$fish_variables" 2>/dev/null; then
      mkdir -p "$HOME/.config/fish"
      ${pkgs.fish}/bin/fish -ic "
        tide configure --auto --style=Lean --prompt_colors='True color' --show_time='24-hour format' --lean_prompt_height='Two lines' --prompt_connection=Disconnected --prompt_spacing=Compact --icons='Few icons' --transient=No
      "
    fi

    ${pkgs.fish}/bin/fish -ic "
      set -U tide_right_prompt_items (for item in \$tide_right_prompt_items
        if not contains \$item rustc kubectl
          echo \$item
        end
      end)
    "
  '';

  # Setup Firefox profile
  # Firefox itself is installed through Homebrew, including Developer Edition.
  # That's why I can't use programs.firefox.profiles
  home.file."Library/Application Support/Firefox/Profiles/dotfiles/user.js".text = ''
    user_pref("browser.tabs.inTitlebar", 0);
    user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
  '';
  home.file."Library/Application Support/Firefox/Profiles/dotfiles/chrome/userChrome.css".text = ''
    #main-window:has(
      #sidebar-box[sidebarcommand="treestyletab_piro_sakura_ne_jp-sidebar-action"]:not([hidden])
    ) #TabsToolbar {
      visibility: collapse !important;
    }

    /* Make the icons in the toolbar grayscale */
    .webextension-browser-action > .toolbarbutton-badge-stack > .toolbarbutton-icon {
      filter: grayscale(1) contrast(1.15) !important;
    }

    #sidebar-header {
      font-size: 1em !important;
      min-height: 28px !important;
      padding-block: 4px !important;
    }

    #sidebar-close {
      width: 24px !important;
      height: 24px !important;
      min-width: 24px !important;
      min-height: 24px !important;
      padding: 0 !important;
    }

    #sidebar-close > .toolbarbutton-icon {
      width: 12px !important;
      height: 12px !important;
      padding: 0 !important;
    }
  '';

  home.activation.firefoxProfileBootstrap = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
    firefox_dir="$HOME/Library/Application Support/Firefox"
    mkdir -p "$firefox_dir/Profiles/dotfiles"

    if [ -L "$firefox_dir/profiles.ini" ]; then
      rm "$firefox_dir/profiles.ini"
    fi

    cat >"$firefox_dir/profiles.ini" <<'EOF'
    [Profile0]
    Name=dotfiles
    IsRelative=1
    Path=Profiles/dotfiles
    Default=1

    [General]
    StartWithLastProfile=1
    Version=2

    [InstallAB92965EBB8B747A]
    Default=Profiles/dotfiles
    Locked=1

    [Install1F42C145FFDD4120]
    Default=Profiles/dotfiles
    Locked=1
    EOF

    cat >"$firefox_dir/installs.ini" <<'EOF'
    [AB92965EBB8B747A]
    Default=Profiles/dotfiles
    Locked=1

    [1F42C145FFDD4120]
    Default=Profiles/dotfiles
    Locked=1
    EOF
  '';
}
