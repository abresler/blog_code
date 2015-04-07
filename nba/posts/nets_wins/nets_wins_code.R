library(rvest)
"http://www.basketball-reference.com/teams/NJN/" %>%
	html %>%
	html_nodes("#NJN") %>%
	html_table %>% data.frame %>% tbl_df %>%
	.$Team %>% grepl('\\*',.) -> playoff_team
data.frame(year = 2015:1968) %>% tbl_df -> seasons
rep("BRK",3) %>% c(.,rep("NJN",35)) %>% c(.,'NYN') %>% c(.,rep("NYA",8)) %>% c('NJA') -> seasons$bref_id
playoff_team -> seasons$playoff_team


"http://www.basketball-reference.com/teams/" -> bref_stem
bref_stem %>% paste0(.,seasons$bref_id,'/',seasons$year,'_games.html') -> seasons$result_url
seasons$result_url -> urls
data.frame() %>% tbl_df -> all_results
for(u in urls){
u %>%
	html() %>%
	html_nodes('#teams_games') %>%
	html_table %>% data.frame %>% tbl_df -> df

df[,c(1:2,6:8,10:13)] %>%
	filter(!G == 'G') %>%
	filter(!is.na(Tm)) -> df
c('gameno','date','loc','opponent','outcome','pts','opp_pts','cumwins','cumloss') -> names(df)
df$loc %>% grepl('@',.) -> df$away_game
NULL -> df$loc
seasons %>% filter(result_url == u) %>% .$bref_id -> df$bref_id
seasons %>% filter(result_url == u) %>% .$year -> df$season
seasons %>% filter(result_url == u) %>% .$playoff_team -> df$playoff_team
all_results %<>% rbind(df)
rm(df);rm(u)
}
rownames(all_results)%<>%
	paste(all_results$season,all_results$gameno,sep = ".")

all_results %>%
	select(season,gameno,cumwins,bref_id,playoff_team) -> df

df %<>%
	mutate(group = ifelse(df$season == 2015, 1, 0))
df[,1:3] %<>% apply(2,as.numeric)
df %>% filter(season == 2015) -> nets_2015
df %>% filter(bref_id %in% c('BRK','NJN')) -> df
dt %>% tbl_df -> dt
dt$color <- ''
'red' -> dt[dt$season == 2015,'color']
'black' -> dt[dt$playoff_team == T,'color']
'grey' -> dt[dt$color == '','color']
df %>%
	ggplot(., aes(gameno, cumwins, group=season, color=group)) +
	geom_line(aes(group=season, color=factor(group))) +
	geom_line(data=nets_2015, aes(gameno, cumwins, group=season, color=factor(group)), lwd=1.5) +
	xlab("Game Number") + ylab("Total Wins") +
	scale_color_manual(values=c("black", "red")) +
	ggtitle("Nets Cumulative Wins by Season\nThrough January 2, 2015") +
	theme(
		plot.title = element_text(hjust=0,vjust=1, size=rel(1.7)),
		panel.background = element_blank(),
		panel.grid.major.y = element_line(color="gray65"),
		panel.grid.major.x = element_line(color="gray65"),
		panel.grid.minor = element_blank(),
		plot.background  = element_blank(),
		text = element_text(color="gray20", size=10),
		axis.text = element_text(size=rel(1.0)),
		axis.text.x = element_text(color="gray20",size=rel(1.5)),
		axis.text.y = element_text(color="gray20", size=rel(1.5)),
		axis.title.x = element_text(size=rel(1.5), vjust=0),
		axis.title.y = element_text(size=rel(1.5), vjust=1),
		axis.ticks.y = element_blank(),
		axis.ticks.x = element_blank(),
		legend.position = "none"
	)

df$gameno %>% as.numeric -> df$gameno
df %>% filter(playoff_team == T) %>% arrange(season,gameno) %>% data.frame-> pt
df %>% filter(!playoff_team == T) %>% filter(!season == "2015") %>% arrange(season,gameno) %>% data.frame -> npt
df %>% filter(season == "2015") %>% data.frame -> nets_2015
data.frame(x = c(0,0,0), y = c(7,10,13), text = c("2015 Season","Made Playoffs","Didn't Make Playoffs")) -> labels
figure(title = "Brooklyn/New Jersey Nets Cumulative Wins by Season", width = 1000, height = 525,tools = "pan,wheel_zoom,box_zoom,reset,resize") %>%
	lay_points(data = pt,x = gameno,y = cumwins,fill_color = 'black',
						 line_color = 'black',type = "circle", fill_alpha = .75,size = 4) %>%
	lay_points(data = npt,x = gameno,y = cumwins,fill_color = 'blue',
						 line_color = 'blue',type = "circle", fill_alpha = .75,size = 4) %>%
	lay_lines(data = nets_2015,x = gameno,y = cumwins, line_color = 'red',line_width = 3) %>%
	lay_text(x = x, y = y, data = labels, text_color = colors, text = text)

