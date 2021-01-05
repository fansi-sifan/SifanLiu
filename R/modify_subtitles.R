# portrait de la jeune fille en feu

srt <- readLines("../../../Downloads/portrait.of.a.lady.on.fire.(2019).fre.1cd.(8245242)/Portrait.de.la.Jeune.Fille.en.Feu.2019.FRENCH.1080p.BluRay.HDLight.x264.AC3-EXTREME.srt")

# take out time
srt_num <- srt[grep("[0-9]", srt)]
srt_txt <- setdiff(srt, srt_num)

grep("voulu",srt_txt)
grep("nager", srt_txt)

cat(srt_txt[179:267])

# change timeline 末代皇后 ----
grep("drôle",srt_txt, value = T)
Sys.setlocale("LC_ALL", "en_US.UTF-8")

srt <- readLines("Downloads/The Last Emperor 1987 1080p BluRay DTS x264-CtrlHD/The.Last.Emperor.1987.Chs.srt")
srt_time <- unlist(strsplit(srt, " --> ", useBytes = TRUE)) # split time start/end

srt_id <- grep("\\d{2}:\\d{2}:\\d{2},\\d{3}", srt, useBytes = TRUE) # ids of time data in t
y <- sort(c(srt_id, srt_id + 1)) # ids of time data in tt
ttt <- gsub(",", ".", srt_time[y]) # replace decimal comma

a <- strptime(ttt, format = "%H:%M:%OS", tz = "GMT")
b <- as.numeric(a) # convert to number
c <- 14 # add 14 sec

d <- as.POSIXct(as.numeric(b + c + 1e-6), origin = "1970-01-01", tz = "GMT") # convert back
e <- format(d, "%H:%M:%OS") # re-format
f <- paste0(e, ",001") # replace decimal point

id_t1 <- seq(1, length(y), 2)
id_t2 <- seq(0, length(y), 2)

g <- paste0(f[id_t1], " --> ", f[id_t2]) # bring into original form

srt_new <- srt
srt_new[srt_id] <- g # insert new sequences into original data
head(srt_new)

### save to new file:
writeLines(srt_new, "Downloads/The Last Emperor 1987 1080p BluRay DTS x264-CtrlHD/The.Last.Emperor.1987.Chs.srt", useBytes = T)
