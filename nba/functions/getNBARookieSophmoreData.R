c("jsonlite","dplyr",'reshape2','pipeR') -> packages
lapply(packages,library, character.only = T)
getNBARookieSophomoreData <- function(season_end = 2015, table_name = "Per Game", player_season = "Rookies", season_type = "Regular Season", scrape_time = F){
	"http://stats.nba.com/stats/commonplayerinfo?LeagueID=00&PlayerID=" -> stats_stem
	"http://stats.nba.com/media/players/230x185/" -> photo_stem
	"http://stats.nba.com/media/img/teams/logos/" -> logo_stem
	season_type %>>% (gsub('\\ ','+',.)) -> st
	table_name %>>% (gsub('\\ ','',.)) -> tn
	substr(season_end,start = 3,4) %>>%
		(paste((season_end-1)
					 %>>% as.character(),.,sep = "-")) -> s
	"http://stats.nba.com/stats/leagueleaders?LeagueID=00&PerMode=" -> nba_stem
	nba_stem %>>% paste0(tn,"&Scope=",player_season,"&Season=",s,
											 "&SeasonType=",st,"&StatCategory=PTS") -> nba_json_url
	pipeline({
		nba_json_url
		fromJSON(simplifyDataFrame = T)
	}) -> df
	df$resultSet$headers %>>% tolower() -> headers
	'team_id' -> headers[headers %in% 'team']
	df$resultSet$rowSet %>>% data.frame() %>>% tbl_df -> data
	headers -> names(data)
	apply(data[,!names(data) %in% c('player','team_id')],2,as.numeric) ->
		data[,!names(data) %in% c('player','team_id')]
	season_end -> data$season_end
	table_name -> data$table_name
	season_type -> data$season_type
	player_season -> data$player_season
	stats_stem %>>% paste0(data$player_id) -> data$nba_stats_url
	photo_stem %>>% paste0(data$player_id,".png") -> data$nba_player_imageURL
	logo_stem %>>% paste0(data$team_id,"_logo.svg") -> data$nba_team_imageURL
	nba_json_url -> data$nba_jsonURL
	if(scrape_time == T){
		Sys.time() -> data$scrape_time
	}
	data$rank <- NULL
	return(data)
}