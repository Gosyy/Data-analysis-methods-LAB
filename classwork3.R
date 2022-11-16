msg <- readLines('putin2018.txt')

#посчитаем кол-во символов 
length(msg)
l<-lapply(msg, nchar)
Reduce(sum, l)

#можно не заморачиваться
txt <- paste(msg, collapse = " ")
nchar(txt)

r <- grep(' мы',msg, ignore.case = TRUE)
r
msg[r]


words <- unlist(strsplit(txt, ' '))
wc <- table(words)
wc <- sort(wc, decreasing = TRUE)
head(wc,20)
