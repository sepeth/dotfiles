{ pkgs, lib, ... }:
let
  dotfilesRepo = builtins.getEnv "DOTFILES_REPO";
  homebrewPaths = [
    "/opt/homebrew/bin"
    "/opt/homebrew/sbin"
  ];
  fishSource = lib.cleanSourceWith {
    src = ./fish;
    filter = path: type:
      let
        baseName = builtins.baseNameOf path;
      in
        baseName != "fish_variables"
        && !lib.hasPrefix "fish_variables." baseName
        && !lib.hasPrefix "fishd.tmp." baseName;
  };
in
{
  home.username = "sepeth";
  home.homeDirectory =
    if pkgs.stdenv.isDarwin then "/Users/sepeth" else "/home/sepeth";
  home.stateVersion = "24.05";

  programs.home-manager.enable = true;

  home.sessionPath = homebrewPaths;

  home.packages = with pkgs; [
    fish
    git
    openssh
    tmux
    vim
  ];

  xdg.configFile."fish".source = fishSource;
  xdg.configFile."fish".recursive = true;
  xdg.configFile."fish/conf.d/homebrew-path.fish".text =
    let
      fishAddPath = lib.concatMapStringsSep "\n" (path: "  fish_add_path " + path) homebrewPaths;
    in ''
      if test -d /opt/homebrew/bin
      ${fishAddPath}
      end
    '';
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
}
