getBREFTeamStatTable <- function(season_end = 2015, table_name = 'team', date = T){
	c('rvest','dplyr','pipeR','RCurl', 'XML','reshape2') -> packages
	lapply(packages, library, character.only = T)
	'http://www.basketball-reference.com/leagues/' -> base
	(season_end-1) %>>% paste0("-",season_end) -> season
	'NBA' -> league
	table_name %>>% tolower -> table_name
	'#' %>>% paste0(table_name) -> css_page
	table_name %>>% paste('stats', sep = "_") -> table
	css_page %>>% paste0(" a") -> css_id
	base %>>% paste0(league,'_',season_end,".html") -> url
	if(table_name == 'standings'){
		pipeline({
			url
			html
			html_table(fill = T)
			~t
		})
		if(season_end >= 1971){
			t[2] %>>% data.frame %>>% tbl_df -> df
			c('team','wins','losses','win_pct','games_back','pts.g','opp_pts.g','srs') -> names(df)
			'Eastern' -> df$conference
			t[3] %>>% data.frame() %>>% tbl_df -> df2
			c('team','wins','losses','win_pct','games_back','pts.g','opp_pts.g','srs') -> names(df2)
			'Western' -> df2$conference
			rbind_list(df,df2) -> df
			df$team %>>% (grepl('\\*',.)) -> df$playoff_team
			df$team %>>% (gsub('\\*','',.)) -> df$team
			df$team %>>% (colsplit(.,'\\(',c('team','conference_rank'))) %>>%
				(.$conference_rank) -> conference_rank
			conference_rank %>>% (gsub('\\)','',.)) %>>% as.numeric -> df$conference_rank
			df$team %>>% (colsplit(.,'\\(',c('team','conference_rank'))) %>>%
				(.$team)  %>>% Trim -> df$team
		} else{
		t[2] %>>% data.frame %>>% tbl_df -> df
		c('team','wins','losses','win_pct','games_back','pts.g','opp_pts.g','srs') -> names(df)
		df$team %>>% (grep('Western Division',.)) %>>% as.numeric -> div
		'' -> df$conference
		'Eastern' -> df[1:div,'conference']
		'Western' -> df[div:nrow(df),'conference']
		if(grep('\\(',df$team) %>>% length > 0){
			df$team %>>% (colsplit(.,'\\(',c('team','conference_rank'))) %>>%
				(.$conference_rank) -> conference_rank
			conference_rank %>>% (gsub('\\)','',.)) %>>% as.numeric -> df$conference_rank
			df$team %>>% (colsplit(.,'\\(',c('team','conference_rank'))) %>>%
				(.$team)  %>>% Trim -> df$team
		}
		df$team %>>% (grepl('\\*',.)) -> df$playoff_team
		df$team %>>% (gsub('\\*','',.)) -> df$team
		}
		df[df$games_back == '—','games_back'] <- 0
		df$games_back %>>% as.numeric -> df$games_back
		df %>>%
			filter(team == 'Baltimore Bullets'|!is.na(wins)) -> df
		df$pts.g - df$opp_pts.g -> df$point_differential
		pipeline({
			url
			html
			xpathSApply(path = '//*[(@id = "all_standings")]//a',xmlAttrs)
			unlist
			~team_url_stems
		})
		NULL -> names(team_url_stems)
		team_url_stems %>>% (gsub('.html|\\/teams|\\/','',.)) %>>%
			(gsub(pattern = "[^[:alpha:]]", replacement = "", .)) -> bref_team_id
		data.frame(season,table_name = 'standings', bref_team_id, df) -> df
		'http://www.basketball-reference.com' %>>% paste0(team_url_stems) -> df$bref_team_season_url
	} else{
		url %>>% ## get table
			html %>>%
			html_nodes(css_page) %>>%
			html_table(header = F) %>>% data.frame() %>>% tbl_df() -> df
		if(df$X1[1] == 'Rk'){
			df %>>%
				filter(X1 == "Rk") %>>% as.character -> names
			'Rk' %>>% grep(x = df$X1) -> row_of_header #find where rank is
			(row_of_header + 1) %>>% (df[.:nrow(df),]) -> df #skip that row and go to the end
			names %>>% tolower-> names(df)} else{
				df %>>%
					filter(X1 == "Rk") %>>% as.character -> names
				'Rk' %>>% grep(x = df$X1) -> row_of_header #find where rank is
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
		paste0("//*[(@id = '",table_name,"')]//a") -> xpath
		##Grab Team Ids
		url %>>% ## get table
			html %>>%
			xpathSApply(xpath,xmlAttrs) %>>%
			unlist %>>% as.character -> stems
		stems %>>% (gsub('\\/|.html|teams','',.)) %>>%
			(gsub(season_end,'',.)) -> bref_team_id
		data.frame(season,table_name = table, bref_team_id, df) -> df
		'http://www.basketball-reference.com' %>>% paste0(stems) -> df$bref_team_season_url
	}
	if(date == T){
		Sys.time() -> df$scrape_time
	}
	return(df)
}