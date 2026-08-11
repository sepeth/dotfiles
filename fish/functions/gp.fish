function gp
    set -l branch (git branch --show-current 2>/dev/null)

    if contains -- $branch main master
        echo "gp: refusing to push protected branch '$branch'" >&2
        return 1
    end

    git push $argv
end
