
get_url<- function(text){
  m <- gregexpr("http://[[:alnum:]]+.[[:alnum:]]*/*[[:alnum:]]*", text, perl=TRUE)
  return(regmatches(text, m))
}

# get the page content if there is link
for (i in 1:length(tweets$text)){
  if (length(get_url(tweets$text[i])[[1]])>0) {
    for url in get_url(tweets$text[i])[[1]] tweets$text[i] = paste(tweets$text[i], readLines(url))
  }
}
readLines(x[[1]][1])