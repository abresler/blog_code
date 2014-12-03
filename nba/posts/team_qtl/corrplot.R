library(qtlcharts)
setwd("/Users/alexbresler/Desktop/Github/blog_code/nba/posts/team_qtl")
'all_data.csv' %>>% read.csv() %>>% tbl_df -> df
df[,sort(df %>>% names)] -> df
'teams_logos_colors.csv' %>>% read.csv %>>% tbl_df -> teams_logos_colors
'column_names.csv' %>>% read.csv() %>>% tbl_df -> col_names
detail <-
	sprintf("<table><tbody><tr><td><span style='font-size:10px;'><span style='font-family:lucida sans unicode,lucida grande,sans-serif;'><img src='%1$s' height= '100px', width = '100px'></span></span></td><td style='text-align: center;'><span style='font-size:12px;'><span style='font-family:lucida sans unicode,lucida grande,sans-serif;'><strong>Season:</strong> %4$s<br><strong>Team:</strong> %2$s<br><strong>Current Team:</strong> %7$s<br><strong>Playoff Team:</strong> %8$s<br><strong>NBA Champion:</strong> %9$s<br><strong>Conference:</strong> %3$s<br><strong>Win Percentage:</strong> %5$s<br><strong>Wins:</strong> %6$s<br><strong>Losses:</strong> %10$s</span></td></tr></tbody></table>",
					df$nba_team_imageURL,
					df$team,
					df$conference,
					paste0(
						as.numeric(df$season_end)-1,"-",as.numeric(df$season_end)
					),
					df$win_pct,
					df$wins,
					df$current_team,
					df$playoff_team,
					df$league_champion,
					df$losses
	)
df$conference %>>%
	factor(levels = c('Western','Eastern'), labels = c(0,1)) -> df$eastern
'Eastern Conference' -> names(df)[67]
df$`Eastern Conference` %>>% as.numeric %>>% (.-1) -> df$`Eastern Conference`
df -> df_scale
col_names$column_name -> names(df_scale)[1:66]
T -> use_scale

if(use_scale == T){
		pipeline({
			df_scale[,!names(df_scale) %in% c("arena","season_id","team","bref_team_season_url","bref_team_id_current","Conference","nba_team_imageURL","color","bref_team_id","points","current_team","bref_id","Season")]
		scale()
		~plot_ds
	})
} else{
	pipeline({
		df_scale[,!names(df_scale) %in% c("arena","season_id","team","bref_team_season_url","bref_team_id_current","Conference","nba_team_imageURL","color","bref_team_id","points","current_team","bref_id")]
		~plot_ds
	})
}
detail -> rownames(plot_ds)
df$bref_id %>>% factor -> ids
plot_ds %>>% (iplotCorr(mat = .,ids, digits = 4,onefile = T,
												reorder = T,title="NBA Team Performance Correlation-Scatter Matrix<br>Data Scaled Mean to 0 and Standard Deviation 1<br>Colored by Current Franchise<br>1951-2015 [2015 Data Through 12/03/14]<br><a href = 'http://basketball-reference.com' target ='_blank'>Data Source</a><br><a href = 'http://asbcllc.com/blog/2014/december/nba_team_corr_matrix/unscaled' target ='_blank'>Unscaled Version Here</a>",
												caption = "The left panel is an interactive correlation matrix, where blue indicates <strong>negative</strong> correation and red indicates <strong>positive</strong> correlation.  The stronger the color, the higher the correlation<br><br>Hover over pixels in the correlation matrix on the left to see the correlation.
												Click the selected box activate scatter plot on the right.<br><br><strong>Example:</strong> Historically Free Throws Made Per Game are 97% positively correlated to Free Throw Attempts Per Game.",
												chartOpts=list(scatcolors=teams_logos_colors$color,
																			 rectcolor = "#ffffff",
																			 cortitle="Correlation Matrix",
																			 scattitle="Scatterplot of Selected Variables"
												))
							)
col_names$column_name -> names(df)[1:66]
F-> use_scale
if(use_scale == T){
	pipeline({
		df[,!names(df) %in% c("arena","season_id","team","bref_team_season_url","bref_team_id_current","Conference","nba_team_imageURL","color","bref_team_id","points","current_team","bref_id","Season")]
		scale()
		~plot_ds
	})
} else{
	pipeline({
		df[,!names(df) %in% c("arena","season_id","team","bref_team_season_url","bref_team_id_current","Conference","nba_team_imageURL","color","bref_team_id","points","current_team","bref_id")]
		~plot_ds
	})
}
detail -> rownames(plot_ds)
df$bref_id %>>% factor -> ids
plot_ds %>>% (iplotCorr(mat = .,ids, digits = 4,onefile = T,
												reorder = T,title="NBA Team Performance Correlation-Scatter Matrix<br>Colored by Current Franchise<br>1951-2015 [2015 Data Through 12/03/14]<br><a href = 'http://basketball-reference.com' target ='_blank'>Data Source</a><br><a href = 'http://asbcllc.com/blog/2014/december/nba_team_corr_matrix/scaled' target ='_blank'>Scaled Version Here</a>",
												caption = "The left panel is an interactive correlation matrix, where blue indicates <strong>negative</strong> correation and red indicates <strong>positive</strong> correlation.  The stronger the color, the higher the correlation<br><br>Hover over pixels in the correlation matrix on the left to see the correlation.
												Click the selected box activate scatter plot on the right.<br><br><strong>Example:</strong> Historically Free Throws Made Per Game are 97% positively correlated to Free Throw Attempts Per Game.",
												chartOpts=list(scatcolors=teams_logos_colors$color,
																			 rectcolor = "#ffffff",
																			 cortitle="Correlation Matrix",
																			 scattitle="Scatterplot of Selected Variables"
												))
)
