

lubuskie <-read.table("dane1.txt", quote="\"", comment.char="")
wielkopolskie <-read.table("dane2.txt", quote="\"", comment.char="")

source("Functions.R")
zad1_sz(lubuskie$V1)
zad1_roz(lubuskie$V1)

