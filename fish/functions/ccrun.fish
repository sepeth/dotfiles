function ccrun
    set -l outfile (path change-extension '' $argv[1])
    set -l homeinc (path resolve ~/Code/include)
    set -l catch2flags (pkg-config catch2 --cflags --libs)
    CPPFLAGS="-std=c++20 -I$homeinc $catch2flags -lCatch2Main" make $outfile && ./$outfile $argv[2..-1]
end
