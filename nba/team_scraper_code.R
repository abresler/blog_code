c('rvest','dplyr','pipeR') -> packages
#dplyr or pipeR use the install.packages function to install them, install.packages('dplyr') and install.packages('pipeR')
#If you don't have rvest do the following - install devtools, install.packages('devtools')
#Load devtools using library(devtools) and then install rvest by using install_github('hadley/rvest')
lapply(packages, library, character.only = T)
'http://www.basketball-reference.com/leagues/NBA_2014.html' -> url
'#team' -> css_page
'#team , #team a' -> css_id

url %>>%
	html %>>%
	html_nodes(css_page) %>>%
	html_table(header = F) %>>%
	data.frame() %>>%
	tbl_df() -> total_table

total_table %>>%
	dplyr::dplyr::filter(X.1 == 'Rk') %>>% as.character -> names

'Rk' %>>% grep(x = total_table$X.1) -> row_of_header #find where rank is
names %>>% tolower -> names(total_table)
names(total_table) %>>% (gsub('\\%|/','\\.',.)) -> names(total_table)
(row_of_header + 1) %>>% (total_table[.:nrow(total_table),]) -> total_table #skip that row and go to the end
NULL -> total_table$rk
total_table$team %>>% grepl(pattern = '\\*') -> total_table$playoff_team
total_table$team %>>% (gsub('\\*','',.)) -> total_table$team
total_table %>>%
	dplyr::filter(!team == 'League Average') -> total_table

total_table %>>% write.csv('Desktop/2014_team_data.csv', row.names = F) #pick where you want to save it

getBREFTeamStatTable <- function(season_end = 2015, table_name = 'team', date = T){
	c('rvest','dplyr','pipeR') -> packages
	lapply(packages, library, character.only = T)
	'http://www.basketball-reference.com/leagues/' -> base
	'NBA' -> league
	'#' %>>% paste0(table_name) -> css_page
	css_page %>>% paste0(" , ", css_page,' a') -> css_id
	table_name %>>% tolower -> table_name
	table_name %>>% paste('stats', sep = "_") -> table
	base %>>% paste0(league,'_',season_end,".html") -> url
	url %>>% ## get table
		html %>>%
		html_nodes(css_page) %>>%
		html_table(header = F) %>>% data.frame() %>>% tbl_df() -> df

	if(df$X.1[1] == 'Rk'){
		df %>>%
			dplyr::filter(X.1 == "Rk") %>>% as.character -> names
		'Rk' %>>% grep(x = df$X.1) -> row_of_header #find where rank is
		(row_of_header + 1) %>>% (df[.:nrow(df),]) -> df #skip that row and go to the end
		names %>>% tolower-> names(df)} else{
		df %>>%
			dplyr::filter(X.1 == "Rk") %>>% as.character -> names
		'Rk' %>>% grep(x = df$X.1) -> row_of_header #find where rank is
		(row_of_header + 1) %>>% (df[.:nrow(df),]) -> df #skip that row and go to the end
		names %>>% tolower-> names(df)
	}
	names(df) %>>% (gsub('\\%|/','\\.',.)) -> names(df)
	NULL -> df$rk
	c('team','arena') -> table_name_character
	df[,!(df %>>% names) %in% table_name_character] %>>% apply(2, function(x) gsub('\\,','',x) %>>%
																									 	as.numeric(x))  -> df[,!(df %>>% names) %in% table_name_character] #get rid of commas and make numeric
	df$team %>>% grepl(pattern = '\\*') -> df$playoff_team
	df$team %>>% (gsub('\\*','',.)) -> df$team
	df %>>% nrow() -1  -> rows
	df[1:rows,] -> df
	(season_end-1) %>>% paste0("-",season_end) -> season
	##Grab Team Ids
	url %>>% ## get table
		html %>>%
		html_nodes(css_id) %>>%
		html_attrs() %>>% unlist %>>% as.character -> stems
	stems[3:length(stems)] -> stems
	stems %>>% (gsub('\\/|.html|teams','',.)) %>>%
		(gsub(season_end,'',.)) -> bref_team_id
	data.frame(season,table_name = table, bref_team_id, df) -> df
	if(date == T){
		Sys.time() -> df$scrape_time
	}
	return(df)
}
