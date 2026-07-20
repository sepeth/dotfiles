function direnv-allow-all --description "Find env files and run direnv allow for each one"
    set -l search_dir

    if test (count $argv) -eq 0
        set search_dir $PWD
    else if test (count $argv) -eq 1
        set search_dir $argv[1]
    else
        echo "Usage: direnv-allow-all [directory]"
        return 2
    end

    if not command -q direnv
        echo "direnv-allow-all: direnv is not installed or not in PATH" >&2
        return 1
    end

    if not test -d $search_dir
        echo "direnv-allow-all: directory does not exist: $search_dir" >&2
        return 1
    end

    set -l search_abs (path resolve $search_dir)
    set -l allowed 0

    for env_file in (find $search_abs \( -name '.envrc' -o -name '.env' \) -type f -not -path '*/node_modules/*' -not -path '*/.git/*' -not -path '*/target/*' | sort)
        direnv allow $env_file
        or return $status

        echo "direnv-allow-all: allowed $env_file"
        set allowed (math $allowed + 1)
    end

    echo "direnv-allow-all: allowed $allowed"
end
