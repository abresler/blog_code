options(stringsAsFactors = F)
###Load Packages and Bring In Function
c('pipeR','dplyr','rvest','RCurl','ggvis') -> packages
#install.packages('ggvis') if you don't have this package you best install it
lapply(packages,library,character.only = T)
'https://raw.githubusercontent.com/abresler/blog_code/master/nba/functions/getBREFTeamStats.R' ->
	dope_function
dope_function %>>% (getURL(url = .,ssl.verifypeer=FALSE)) %>>%
	(parse(text = .)) -> get
eval(expr = get)



### Historic Seasons
1950:2014 -> season_ends #these are the years
data.frame() %>>% tbl_df -> all_historic_years #create an empty dataframe

#our FOR loop for every season end from 1950 to 2014
for(s in season_ends){
	getBREFTeamStatTable(season_end = s, date = T,table_name = 'team')-> table
	s -> table$season_end #add a numeric year to make sorting easy
	all_historic_years %>>% rbind_list(table) -> all_historic_years
	table %>>% rm
}

#Save Historic Data Somewhere
#all_historic_years %>>%
	#write.csv('2014/november/loop_analyze_visualize/historic_team_data_1950_2014.csv',row.names = F)

#Pull in 2015
getBREFTeamStatTable(table_name = 'team',season_end = 2015, date = T) -> data_2015
2015 -> data_2015$season_end

#Bind them Both Together
all_historic_years %>>%
	rbind_list(data_2015) %>>% arrange(desc(season_end)) -> all_nba_team_data


all_nba_team_data %>>% summary #summary of the variables
all_nba_team_data$team %>>% unique #all of the NBA taems

## Add a columns
all_nba_team_data %>>%
	filter(!is.na(g)) %>>%
	mutate(points_total = g * pts.g,
				 points_per_fga = points_total / fga) -> all_nba_team_data
all_nba_team_data %>>%
	select(team,season,points_total, points_per_fga) #make sure it worked

#Best Ever
all_nba_team_data %>>%
	filter(max(points_per_fga) == points_per_fga) %>>%
	select(team, season_end, points_per_fga, playoff_team)

##By Year
all_nba_team_data %>>%
	group_by(season_end) %>>%
	filter(max(points_per_fga) == points_per_fga) %>>%
	select(team, season_end, points_per_fga, playoff_team) -> efficency_by_year
efficency_by_year %>>% summary


## Explore 3 Pointers
all_nba_team_data %>>%
	filter(!is.na(X3pa)) -> era3pt_shot

## Add Cuts to Create a Nominal Decade Fill
era3pt_shot$season_end %>>%
	cut(breaks = c(0,1989,1999,2009,2015), labels = c('1980s','1990s','2000s','2010s')) -> era3pt_shot$decade

era3pt_shot %>>%
	filter(!season_end == 2015) %>>%
	ggvis(x =~fga, y =~X3pa, fill=~factor(decade)) %>>%
	layer_points() %>>%
	add_relative_scales() %>>%
	add_axis("x", title = "Field Goal Attempts", title_offset = 50) %>>%
	add_axis("y", title = "3PT Attempts", title_offset = 55) %>>%
	add_legend("fill", title = "Decade")

#Find a way to look apples to apples
era3pt_shot %>>%
	mutate(X3pa_per_min = X3pa/mp,
				 X3pa_per_240_min = X3pa_per_min * 240) -> era3pt_shot

##
era3pt_shot %>>%
	ggvis(x =~season_end, y =~X3pa_per_min, fill = ~decade) %>>%
	add_axis("x", title = "", values = seq(from = 1980,to =2015,1), format="####", title_offset = 50,
					 properties = axis_props(
					 	ticks = list(stroke = "black"),
					 	majorTicks = list(strokeWidth = 2),
					 	labels = list(
					 		angle = 50,
					 		fontSize = 11,
					 		align = "left",
					 		baseline = "middle",
					 		dx = 3
					 	))) %>>%
	add_axis("y", title = "3PT Point Attempts Per Minute",
					 title_offset = 55, ticks = 20) %>>%
	layer_points() %>>%
	add_relative_scales() %>>%
	group_by(decade) %>>%
	layer_model_predictions(model = 'lm', stroke =~factor(decade), se  = T) %>>%
	add_legend(c("fill","stroke"),orient = 'right', title = "Decade",
						 properties = legend_props(
						 	title = list(fontSize = 12),
						 	labels = list(fontSize = 10, dx = 10),
						 	symbol = list(stroke = "black", strokeWidth = 1,
						 								size = 100)
						 ))


era3pt_shot %>>%
	group_by(season_end) %>>%
	summarise(total_3pt_attempts = sum(X3pa),
						total_games = sum(g),
						total_3pt_shots_per_game = total_3pt_attempts/total_games) %>%
	select(season_end,total_3pt_shots_per_game) -> per_game

per_game %>>%

era3pt_shot %>>%
	group_by(season_end) %>>%
	filter(X3pa_per_min == max(X3pa_per_min)) %>>%
	select(season_end, team,X3pa_per_min,X3pa_per_240_min) -> top_teams

#Bring in Colors
'http://asbcllc.com/data/NBA/team_colors.csv' %>>% read.csv  %>>% data.frame %>>%
	tbl_df -> active_colors
data.frame(team = top_teams$team %>>% unique) %>>% tbl_df %>>%
	arrange((team)) -> teams
teams %>>% merge(active_colors,all.x = T) -> teams
'#EE2944' -> teams[14,2] #Add Clippers Color
'#266A2E'-> teams[15,2] #Add Sonics Color


top_teams %>>%
	ggvis(x =~season_end, y =~X3pa_per_240_min,text := ~team, fill = ~team) %>>%
	add_axis("x", title = "", values = seq(from = 1980,to =2015,1), format="####", title_offset = 50,
					 properties = axis_props(
					 	ticks = list(stroke = "black"),
					 	majorTicks = list(strokeWidth = 2),
					 	labels = list(
					 		angle = 50,
					 		fontSize = 11,
					 		align = "left",
					 		baseline = "middle",
					 		dx = 3
					 	))) %>>%
	add_axis("y", title = "3PT Attempts Per 240 Minutes of Available Burn", format="####.00",
					 title_offset = 55, ticks = 20) %>>%
	layer_text() %>>%
	scale_nominal(property = 'fill',
								domain = as.character(teams$team),
								range = as.character(teams$primary_color)) %>>%
	add_legend("fill",orient = 'right', title = "Team",
						 properties = legend_props(
						 	title = list(fontSize = 12),
						 	labels = list(fontSize = 10, dx = 10),
						 	symbol = list(stroke = "black", strokeWidth = 1,
						 								size = 100)
						 	))
