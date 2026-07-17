function envcopy --description "Copy .env files from source to destination, preserving relative paths"
    set -l source_dir
    set -l destination_dir

    if test (count $argv) -eq 1
        set source_dir $PWD
        set destination_dir $argv[1]
    else if test (count $argv) -eq 2
        set source_dir $argv[1]
        set destination_dir $argv[2]
    else
        echo "Usage: envcopy <destination>"
        echo "       envcopy <source> <destination>"
        return 2
    end

    if not test -d $source_dir
        echo "envcopy: source does not exist or is not a directory: $source_dir" >&2
        return 1
    end

    if not test -d $destination_dir
        echo "envcopy: destination does not exist or is not a directory: $destination_dir" >&2
        return 1
    end

    set -l source_abs (path resolve $source_dir)
    set -l destination_abs (path resolve $destination_dir)
    set -l copied 0
    set -l skipped 0

    for source_file in (find $source_abs -name '.env*' -type f -not -path '*/node_modules/*' -not -path '*/.git/*' -not -path '*/target/*' | sort)
        set -l relative_file (string replace -- "$source_abs/" '' $source_file)
        set -l destination_file "$destination_abs/$relative_file"

        if test -e $destination_file
            echo "envcopy: skipping existing $destination_file" >&2
            set skipped (math $skipped + 1)
            continue
        end

        mkdir -p (path dirname $destination_file)
        or return $status

        cp $source_file $destination_file
        or return $status

        echo "envcopy: copied $relative_file"
        set copied (math $copied + 1)
    end

    echo "envcopy: copied $copied, skipped $skipped"
end
