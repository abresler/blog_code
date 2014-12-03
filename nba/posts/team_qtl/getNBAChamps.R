getNBAChamps <- function(){
"http://www.basketball-reference.com/leagues/" %>>%
	html %>>%
	html_table(header = T) %>>% data.frame() %>>% tbl_df -> table
table[3:nrow(table),1:3] -> df
c('season','league','team') -> names(df)
df %>>%
	filter(league == 'NBA') -> df
TRUE -> df$league_champion
df$season %>>% (colsplit(.,'\\-',c('start','end'))) %>>%
	(.$start) %>>% (paste(.,.+1,sep = '-')) -> df$season
NULL -> df$league
return(df)
}