library(rvest)

'http://espn.go.com/sportsnation/teamrankings#table' %>>%
	html %>>%
	html_table() %>>% data.frame %>>% tbl_df -> data

names(data) %<>% tolower()
c('rank','team') -> names(data)[1:2]
data$rank  %>>% (gsub('\\.','',.)) %>>% as.numeric -> data$rank
data[!is.na(data$rank),] -> data
2014 -> data$year
2003:2013 -> years
'http://espn.go.com/sportsnation/teamrankings/_/year/' %>>% paste0(years) -> urls
data.frame(year = years, url = urls) -> df_urls
urls[2] -> u
data.frame() %>>% tbl_df -> all_df
for(u in urls){
	u %>>% html %>>%
		html_table() %>>% data.frame %>>% tbl_df -> df
	names(df) %<>% tolower()
	c('rank','team') -> names(df)[1:2]
	df$rank  %>>% (gsub('\\.','',.)) %>>% as.numeric -> df$rank
	df[!is.na(df$rank),] -> df
	df_urls[df_urls$url == u,'year'] -> df$year
	all_df %>>% rbind_list(df) -> all_df
}
all_df %>>% rbind_list(data) -> all_df
all_df %>>%
	arrange(desc(year)) -> all_df
apply(all_df[,3:11],2, as.numeric) -> all_df[,3:11]
all_df %>>% write.csv('Desktop/webpage/abresler.github.io/data/espn/team_rankings.csv', row.names = F)
