function nd
    set -l search_dir $PWD

    while true
        if test -e "$search_dir/flake.nix"
            nix develop "path:$search_dir" -c fish $argv
            return $status
        end

        set -l parent_dir (path dirname "$search_dir")
        test "$parent_dir" = "$search_dir"; and break
        set search_dir "$parent_dir"
    end

    nix develop path:. -c fish $argv
end
