function gp
    set -l protected_branches main master
    set -l positional_args
    set -l pushes_tags 0

    for arg in $argv
        if test "$arg" = "--tags"
            set pushes_tags 1
            continue
        end

        if string match -q -- '-*' $arg
            continue
        end

        set -a positional_args $arg
    end

    if test (count $positional_args) -eq 0
        if test $pushes_tags -eq 1
            git push $argv
            return $status
        end

        set -l branch (git branch --show-current 2>/dev/null)
        if contains -- $branch $protected_branches
            echo "gp: refusing to push protected branch '$branch'" >&2
            return 1
        end
    else if test (count $positional_args) -eq 1
        set -l branch (git branch --show-current 2>/dev/null)
        if contains -- $branch $protected_branches
            echo "gp: refusing to push protected branch '$branch'" >&2
            return 1
        end
    else
        for refspec in $positional_args[2..-1]
            set -l destination $refspec

            if string match -q -- '*:*' $refspec
                set destination (string split -m1 : -- $refspec)[2]
            end

            set destination (string replace -r '^refs/heads/' '' -- $destination)
            if contains -- $destination $protected_branches
                echo "gp: refusing to push protected branch '$destination'" >&2
                return 1
            end
        end
    end

    git push $argv
end
