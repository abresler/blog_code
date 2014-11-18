#REQUIRES RVEST - pipeR - dplyr - reshape2
getGoogleResults <- function (search.term, show = 10){
	Trim <- function (x) gsub("^\\s+|\\s+$", "", x)
	packages = c('rvest','dplyr','pipeR','reshape2')
	lapply(packages,library,character.only = T)
	search.term -> st
	if ("&" %>>% grepl(x = st) == T) {
		st <- gsub("\\&", "%26", search.term)
	}
	if ("\\/" %>>% grepl(x = st) == T) {
		st <- gsub("\\/", "%2F", search.term)
	}
	st <- st %>>% (gsub("\\ ", "+", .))
	url.name = paste0("https://www.google.com/search?q=", st)
	show %>>% (ceiling(./10)) %>>%
	(seq(from = 1, to = ., by = 1)) -> loops
	paste0(url.name,"&start=",loops * 10) -> l_urls
	search_url = c(url.name, l_urls)
	data.frame(loop = 1:length(search_url),search_url) %>>%
		tbl_df -> df_urls
	data.frame() %>>% tbl_df -> all_df
	for(s in 1:length(search_url)){
	search_url[s] %>>% getURL %>>% htmlParse %>>%
		html_nodes(".r a") %>>%
		html_attrs %>>% unlist -> urls
	names(urls) = NULL
	urls %>>% (substr(x = ., start = 8, urls %>>% nchar)) -> urls
	urls[!urls %in% ""] -> urls
	urls %>>% (colsplit(string = ., "\\&", c("link", "key"))) %>>%
		(.[, 1]) %>>% as.character -> urls
	search_url[s] %>>% getURL %>>% htmlParse %>>% html_nodes(".r a") %>>%
		html_text %>>% unlist -> name
	data.frame(url = urls, url_name = name, google_url = search_url[s],
									 search_term = search.term) %>>% tbl_df -> df
	all_df %>>% rbind(df) -> all_df
	rm(df)
}
all_df[all_df$url %>>% grepl(pattern = "http|https"), ] -> all_df
all_df[1:show, ] -> all_df
Sys.time() -> all_df$search_time
colsplit(string = all_df$url_name,pattern = '\\||\\ - ',c('title','site')) %>>%
	(cbind(.,all_df)) -> all_df
apply(all_df[,1:2],2,Trim) -> all_df[,1:2]
all_df[,c(3,1:2,4:7)] -> all_df
return(all_df)
}


getGoogleResults(search.term = "Brooklyn Nets", show = 74) -> nets_df
