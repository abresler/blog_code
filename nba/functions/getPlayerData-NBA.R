c("jsonlite","dplyr",'reshape2','pipeR') -> packages
lapply(packages,library,character.only = T)
977-> player_id
getNBAPlayerData <- function(player_id){
	base = "http://stats.nba.com/stats/commonplayerinfo?LeagueID=00&PlayerID="
	base %>>% paste0(player_id) -> url
	pipeline({
		url
		fromJSON(simplifyDataFrame = T)
		~df
	})
	df$resultSets$name -> tables
	df$resultSets$headers %>>% length -> headers_length
	df$resultSets$headers[1] %>>% unlist %>>% tolower -> header

	c('player_id','player','weight_lbs') -> header[header %in% c('person_id','display_first_last','weight')]
	df$resultSets$rowSet[1] %>>% data.frame -> df_player_data
	header -> names(df_player_data)
	df_player_data$height %>>% colsplit("\\-",c('feet','inches')) %>>%
		(.$feet * 12 + .$inches) -> df_player_data$height_inches
	apply(df_player_data[,names(df_player_data) %in%
											 	c('from_year','to_year','weight_lbs')],2,as.numeric) -> df_player_data[,names(df_player_data) %in% c('from_year','to_year','weight_lbs')]

	df_player_data$birthdate %>>% as.Date('%Y-%m-%d') -> df_player_data$birthdate
	df_player_data %>>%
		mutate(seasons_player = to_year - from_year,
					 bmi = (weight_lbs/height_inches^2) * 703
		) -> df_player_data

	if(tables[tables %in% 'PlayerHeadlineStats'] == 'PlayerHeadlineStats'&df$resultSets$rowSet[2] %>>% data.frame %>>% nrow >0){
		df$resultSets$headers[2] %>>% unlist %>>% tolower -> header
		df$resultSets$rowSet[2] %>>% data.frame %>>% tbl_df -> df_player_results
		'player' -> header[2]
		header -> names(df_player_results)
		apply(df_player_results[,names(df_player_results) %in% c('pts','ast','reb','all_star_appearances')],2,as.numeric) -> df_player_results[,names(df_player_results) %in% c('pts','ast','reb','all_star_appearances')]
		'career_' %>>% paste0(names(df_player_results)[4:7]) -> names(df_player_results)[4:7]
		df_player_data %>>% merge(df_player_results) -> df_player_data
	}
	url -> df_player_data$nba_stats_url
	"Player Info" -> df$table_name
	return(df_player_data)
}