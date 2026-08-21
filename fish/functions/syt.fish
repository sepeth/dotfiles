function syt
    md ~/Music/Sing
    yt-dlp --extract-audio --audio-format aac $argv
    cd -
end
