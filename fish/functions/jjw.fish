function jjw --description "Create a jj workspace, copy .env files, and allow direnv"
    if test (count $argv) -lt 1
        echo "Usage: jjw <workspace-path> [jj workspace add args...]"
        return 2
    end

    set -l workspace_path $argv[1]
    set -l workspace_args $argv[2..-1]
    set -l source_dir $PWD

    jj workspace add $workspace_args $workspace_path
    or return $status

    envcopy $source_dir $workspace_path
    or return $status

    direnv-allow-all $workspace_path
end
