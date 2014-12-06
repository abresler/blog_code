1951:2015 -> years
data.frame() %>>% tbl_df -> all_misc
data.frame() %>>% tbl_df -> all_team
data.frame() %>>% tbl_df -> all_misc
getBREFTeamStatTable(season_end = 1965, 'standings')
for(year in years){
	getBREFTeamStatTable(season_end = year,table_name = 'team',date = F) -> team
	rbind_list(all_team,team) -> all_team
	getBREFTeamStatTable(season_end = year,table_name = 'misc',date = F) -> misc
	rbind_list(all_misc,misc) -> all_misc
}
