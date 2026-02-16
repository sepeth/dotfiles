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
  '';
}
