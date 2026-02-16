function syt
    md ~/Songs
    yt-dlp --extract-audio --audio-format aac $argv
    cd -
end
