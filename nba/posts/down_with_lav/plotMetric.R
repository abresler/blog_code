metricChart <- function(data, xlabel, ylabel, height = 400, width = 600, title){
	options(viewer = NULL)
	library(rCharts)
	library(jsonlite)
	library(pipeR)

	#set up rCharts
	#key is to define how to handle the data
	rChartsMetrics <- setRefClass(
		'rChartsMetrics',
		contains = 'Dimple',
		methods = list(
			initialize = function(){
				callSuper();
			},
			getPayload = function (chartId) {
				# use jsonlite
				if(is.xts(params$data)){
					data = jsonlite::toJSON( data.frame(
						date = format(index(params$data))
						,params$data
					))
				} else {
					data = jsonlite::toJSON( params$data )
				}

				chart = toChain(params$chart, 'myChart')
				opts = toJSON2(params[!(names(params) %in% c('data', 'chart'))])
				list(opts = opts, data = data, chart = chart, chartId = chartId)
			},
			render = function (chartId = NULL, cdn = F, static = T, standalone = F)
			{
				params$dom <<- chartId %||% params$dom
				params$height <<- params$options$height
				params$width <<- params$options$width
				template = read_template(getOption("RCHART_TEMPLATE", templates$page))
				assets = Map("c", get_assets(LIB, static = static, cdn = cdn,
																		 standalone = standalone), html_assets)
				html = render_template(
					template
					, list(
						params = params
						, assets = assets
						, chartId = params$dom
						, width = params$options$width
						, height = params$options$height
						, script = .self$html(params$dom)
						, CODE = srccode
						, lib = LIB$name
						, tObj = tObj
						, container = container
					)
					, partials = list(
						chartDiv = templates$chartDiv
						, afterScript = templates$afterScript %||% "<script></script>"
					)
				)
			}
		)
	)

	# now make a rChart with our rChartsMetrics
	rM <- rChartsMetrics$new()
	rM$setLib('http://timelyportfolio.github.io/rCharts_metrics')
	#rM$setLib('.')
	rM$lib = 'metrics'
	rM$LIB$name = 'metrics'
	rM$setTemplate(
		script = "
		<script>

		var opts = {{{ opts }}};
		opts.options.data =  {{{ data }}};

		opts.options.target = '#' + opts.dom

		//handle date data - crude but works for now
		if( opts.options.x_accessor === 'date' && typeof opts.options.data[0][opts.options.x_accessor] ) {
		opts.options.data = convert_dates( opts.options.data, 'date' );
		}

		data_graphic(
		opts.options
		)
		</script>
		"
	)
	rM$set(
		data = data
		,options = list(
			x_accessor = xlabel
			,y_accessor = ylabel
			,width = width
			,height = height
			,title = title
		)
	)
	print(rM)
}
