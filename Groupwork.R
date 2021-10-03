setwd("/Users/Danni/Downloads/Fall/Statistical programming/StP-practical_1-Gourp_8")
Bible <- scan(file = "Bible.txt", what = "character", skip = 156)
n_Bible <- length(Bible)
Bible <- Bible[-((n_Bible - 2909) : n_Bible)] ## strip license

split_punct <- function(text){
  index_punct <- NULL
  punct <- c(",$", "\\.$", ";$", "!$", ":$", "\\?$")
  #n_index = NULL
  for (i in 1 : length(punct)) {
    index_punct[[i]]<- grep(punct[i], text)
    #n_index[i] = length(index_punct[[i]])
  }
  #cat("The number of words with different punctuations should be:", n_index, collapse = "\n")
  index_punct <- sort(unlist(index_punct, use.names = FALSE))
  index_punct_new <- index_punct + 1 : length(index_punct)
  text_new <- rep("", length(index_punct) + length(text))
  text_new[index_punct_new] <- substr(text[index_punct], nchar(text)[index_punct], nchar(text)[index_punct])
  text_new[index_punct_new - 1] <- substr(text[index_punct], 1, nchar(text)[index_punct] - 1)
  text_new[-c(index_punct_new, index_punct_new - 1)] <- text[-index_punct]
  return(text_new)
}

Bible_new <- split_punct(Bible)

b <- NULL
Blower <- tolower(Bible_new)
Bible_unique <- unique(Blower)
indicies <- match(Blower, Bible_unique)
appearance <- tabulate(indicies)
threshold <- head(sort(appearance, decreasing = TRUE),1000)[1000]

loc_1000 <- which(appearance > threshold)
b <- Bible_unique[loc_1000]

b_match <- match(Blower, b)
rr <- which(is.na(b_match)==FALSE)
cmws <- Blower[rr]
cmws_f <- Blower[rr+1]
which(cmws_f == cmws)
match(cmws_f,cmws)
aaaa<-cbind(cmws, cmws_f)
pp <- which(is.na(match(cmws_f,cmws))==FALSE)
cmws_bind <- aaaa[pp,1:2]

A <- matrix(0,997,997)
for (i in 1:997){
  for(j in 1:997){
    temp <- c(b[i], b[j])
    which
        A[i,j] <-A[i,j]+1
      }
    }
  }
}
a <- 0
for (k in nrow(cmws_bind)){
  if (temp == cmws_bind[k,]){
    a  <- a+1}
}
