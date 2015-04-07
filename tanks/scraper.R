gc('dplyr','pipeR','data.table','reshape2','tidyr','magrittr') -> packages
lapply(packages,library,character.only = T)
#packages

#get table
"http://ww2db.com/vehicle.php?list=T" %>%
	html %>%
	readHTMLTable(header = F) %>>%
	(.[2] %>% data.frame %>% tbl_df -> tanks)

"http://ww2db.com/vehicle.php?list=T" %>%
	html %>%
	html_nodes("td a") %>% html_attrs() %>% unlist %>% as.character %>% .[14:89] %>% paste0("http://ww2db.com/",.) -> urls
apply(tanks,2,function(x) substr(x,1,nchar(x)-1)) %>% data.frame -> tanks
tanks$NULL.V4 %>% substr(.,1,nchar(.)-1) -> tanks$NULL.V4
tanks$NULL.V4 %>% substr(.,1,nchar(.)-1) -> tanks$NULL.V4
tanks$NULL.V3 %>% substr(.,1,nchar(.)-1) -> tanks$NULL.V3
tanks$NULL.V3 %>% substr(.,1,nchar(.)-1) -> tanks$NULL.V3
tanks$NULL.V1 %>% substr(.,1,nchar(.)-1) -> tanks$NULL.V3
c('tank_name','role','manufacturer','country') -> names(tanks)
items %in% c("Machinery","Suspension","Armament",
						 "Armor",'Crew','Length','Width','Height','Weight','Speed','Range') -> items
c('Crew','Length','Width','Height','Weight','Speed','Range') -> item_vals
urls -> tanks$url

#Build Scraper
tanks$url -> urls
data.frame() %>% tbl_df -> all_tanks
for(url in urls){
url %>%
html() %>% html_table(fill = TRUE)  %>% do.call("rbind_list",.) %>% tbl_df -> dfs
url %>% html %>% html_nodes("b i") %>% html_text -> tanks_id
c('item','value') -> names(dfs)[c(1:2)]
if(dfs %>% filter(item %in% items) %>% select(item,value) %>% tbl_df %>% nrow == 0){
	data.frame() %>% tbl_df -> df
} else{
dfs %>% filter(item %in% items) %>% select(item,value) %>% tbl_df -> df
if(df %>% nrow >= 5 ){
	c('item','value') -> names(df)
	url -> df$url
	df$value %>% reshape2::colsplit(.,'\\;',c('value','b')) %>% .$value -> df$value
	df[df$item %in% item_vals,'value'] -> v
	v$value %>% extract_numeric() -> df[df$item %in% item_vals,'value']
	'' -> df$tank
	1:nrow(df) -> df$id
	df  %>% data.table(key = 'id') -> df
	if(tanks_id %>% length > 1){
		seq(1,df %>% nrow,by = (df %>% nrow)/tanks_id %>% length) -> label_rows
		data.frame(id = label_rows, tank = tanks_id) %>% data.table(key = 'id') -> tank_id
		tank_id[df, roll=T] -> df
		df$i.tank = NULL
		df$id = NULL
		df %>% tbl_df -> df
	} else{
		tanks_id -> df$tank
		df$id = NULL
	}
	tanks[tanks$url == url,'country']
	df[,c('tank','item','value','url')] -> df
} else{
	'' -> df$item
	'' -> df$value
	url -> df$url
	tank_id -> df$tank
}
all_tanks %<>% rbind_list(df)
}
}

#Wide
all_tanks %>% left_join(tanks, by = 'url') -> all_tanks
all_tanks[,c('tank','item','value','country','role')] %>% data.frame %>% reshape(idvar  = c('tank','country','role'),timevar = 'item', direction = 'wide') %>% tbl_df -> wide_df
names(wide_df) %>% gsub('value.','',.) %>% tolower -> names(wide_df)
wide_df[,8:14] %<>% apply(2,as.numeric)

#Plot In Metrics Graphs
#devtools::install_github(hrbrmstr/metricsgraphics)  #install if you don't have it
library(metricsgraphics)

wide_df %>%
	filter(!is.na(weight)) %>%
	filter(speed > 0) %>%
	mjs_plot(x=weight, y=speed, width=600, height=500) %>%
	mjs_point(least_squares=TRUE,color_type = 'category',x_rug = T,y_rug = T,
						size_range=c(5,7,8,10),
						color_accessor = country,size_accessor = crew,
						color_range=c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC")
		mjs_labs(x="Weight in Tons", y="Speed in Kilometers Per Hour") %>%
	mjs_add_marker(42.13, "Mean Heavy Tank") %>%
	mjs_add_marker(7.83, "Mean Light Tank") %>%
	mjs_add_marker(30.78, "Mean Medium Tank")

	##Data.table
	all_tanks[,c('tank','tank_name','item','value','country','role','url')] %>% data.frame %>% reshape(idvar  = c('tank','tank_name','country','role','url'),timevar = 'item', direction = 'wide') %>% tbl_df -> wide_df2
	names(wide_df2) %>% gsub('value.','',.) %>% tolower -> names(wide_df2)

	#devtools::install_github(rstudio/DT)  #install if you don't have it
	library(DT)
	paste0("<a href ='",
				 wide_df2$url,
				 "' target = '_blank'>",
				 wide_df2$tank_name,"</a>") -> wide_df2$tank_name
	wide_df2$url = NULL
	wide_df2 %>%
	filter(!is.na(weight)) %>%
	filter(speed > 0) %>% datatable(options = list(
		lengthMenu = c(5,10,15,20),
		autoWidth = FALSE
	),escape = F)
