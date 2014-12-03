setwd("/Users/alexbresler/Desktop/Github/blog_code/nba/posts/team_qtl")
1951:2015 -> years
data.frame() %>>% tbl_df -> all_misc
data.frame() %>>% tbl_df -> all_team
data.frame() %>>% tbl_df -> all_standings

for(year in years){
	getBREFTeamStatTable(season_end = year,table_name = 'team',date = F) -> team
	rbind_list(all_team,team) -> all_team
	getBREFTeamStatTable(season_end = year,table_name = 'misc',date = F) -> misc
	rbind_list(all_misc,misc) -> all_misc
	getBREFTeamStatTable(season_end = year,table_name = 'standings',date = F) -> standings
	rbind_list(all_standings,standings) -> all_standings
}

all_standings[,!names(all_standings) %in% 'table_name'] %>>%
	(left_join(.,all_team[,!names(all_team) %in% 'table_name'])) -> df
df %>>%
	(left_join(.,all_misc[,!names(all_misc) %in% 'table_name'])) -> df
df %>>%
	filter(!bref_team_id	== 'INO') -> df

apply(df[,c('win_pct','X3par','ts.','efg.','ft.fga','efg..1','ft.fga.1','fg.','X3p.','X2p.','ft.','ftr')],2, function(x) x * 100) ->
	df[,c('win_pct','X3par','ts.','efg.','ft.fga','efg..1','ft.fga.1','fg.','X3p.','X2p.','ft.','ftr')]
apply(df[,c(18:19,21:22,24:25,27:28,30:37)],2, function(x) x/df$g) ->
	df[,c(18:19,21:22,24:25,27:28,30:37)]
'teams_logos_colors.csv' %>>% read.csv() %>>% tbl_df -> teams_logos_colors
data.frame(bref_id = 1:nrow(teams_logos_colors),teams_logos_colors) -> teams_logos_colors

merge(current, teams_logos_colors, all.y = T,by = c('bref_team_id_current','color','nba_team_imageURL')
teams_logos_colors[,c('bref_team_id','bref_team_id_current')] %>>% left_join(df) -> df
teams_logos_colors %>>%
	filter(bref_team_id_current == bref_team_id) %>>%
	select(team,bref_team_id_current) -> current
'current_team' -> names(current)[1]
teams_logos_colors %>>% left_join(current) -> teams_logos_colors

teams_logos_colors %>>% left_join(df) -> df
source('getNBAChamps.R')
getNBAChamps() -> df_champs
df %>>% left_join(df_champs) -> df
df[is.na(df$league_champion),'league_champion'] <- FALSE
df[,sort(df %>>% names)] -> df
df$season %>>% (colsplit(.,'\\-',c('a','season'))) %>>% (.$season) -> df$season_end
df %>>% write.csv('all_data.csv', row.names = F)
