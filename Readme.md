# Dotfiles

My personal dotfiles but inspired from many others here and from all over the
interweb. Some of them are old and perhaps not used, but most are. If you are
not me, you wouldn't install this, but in any case, it starts with the `setup`
script.

The `setup` script defaults to the Nix Home Manager flow and auto-detects the
target. Override it with `--target` or use the legacy symlink/Vundle flow:

```sh
./setup --target sepeth-darwin
./setup --legacy
```

Targets:

- `sepeth-darwin`
- `sepeth-linux`
- `sepeth-linux-arm`

Note: fish writes `fish_variables` at runtime, so the Home Manager config
excludes that file from the managed `~/.config/fish` directory to avoid
permission errors.

## Known issues

* fish's `funced` and `funcsave` don't work as expected anymore. After
switching to Nix home-manager, fish/functions directory is a symlink to the Nix
store, which is _read-only_. Just edit the function files and run `setup`.
