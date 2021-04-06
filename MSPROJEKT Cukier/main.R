

lubuskie <-read.table("dane1.txt", quote="\"", comment.char="")
wielkopolskie <-read.table("dane2.txt", quote="\"", comment.char="")

source("Functions.R")
zad1_sz(lubuskie$V1)
zad1_roz(lubuskie$V1)
zad1_sz(wielkopolskie$V1)
zad1_roz(wielkopolskie$V1)

zad2(lubuskie$V1, 0.95)
zad2(wielkopolskie$V1, 0.95)

zad3(lubuskie$V1, 0.05)

zad4(wielkopolskie$V1, 2, 0.05)
test_fs(lubuskie$V1,wielkopolskie$V1,0.05)

zad5(lubuskie$V1,wielkopolskie$V1,0.05)
