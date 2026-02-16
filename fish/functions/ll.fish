function ll --wraps=ls --description 'List contents of directory using long format'
    ls -taoehsr $argv

    # -t: Sorts the output by time, with the most recently modified files and directories appearing first.
    # -a: Lists all files and directories, including those whose names begin with a dot (.), which are normally hidden.
    # -o: Produces a long format listing (like -l) but with the group column omitted.
    # -e: Displays the Access Control List (ACL) for files and directories that have one.
    # -u: Uses the file's last access time for sorting (-t) and display (-l), instead of the default modification time.
    # -h: Prints file sizes in a human-readable format (e.g., "1K", "234M", "2G").
    # -s: Displays the number of filesystem blocks used by each file.

end
