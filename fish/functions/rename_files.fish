function rename_files
    # Loop through all files in the current directory
    for file in *.epub
        # Extract the filename without extension
        set filename (basename $file .epub)

        # Convert filename to lowercase and replace underscores with spaces
        set new_filename (string replace "_" " " $filename)

        # Capitalize the first letter of each word
        set new_filename (string split $new_filename -n) | map (string capitalize) | string join " "

        # Rename the file
        rename (mv -- $file $new_filename.epub)

        # Print confirmation message
        echo "Renamed '$file' to '$new_filename.epub'"
    end
end
