library(rvest)

web_scraping = function(){
  
  url_oni = read_html("https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php")
  
  oni_data_scraping = url_oni %>%
    html_nodes("table") %>%
    html_table(header = T)
  
  oni_data_scraping = data.frame(oni_data_scraping[[9]])
  oni_data_df = oni_data_scraping
  n = length(oni_data_scraping$Year)

  row2delete = NULL
  for(i in 1:n){
    if(oni_data_scraping$Year[i] == 'Year'){
      row2delete = c(row2delete, i)
    }
  }
  oni_data_df = oni_data_df[(-row2delete),]
  oni_data_df <- mapply(oni_data_df, FUN=as.numeric)

  oni_data_ts = NULL
  for(i in 1:nrow(oni_data_df)){#i=1
    oni_data_ts = c(oni_data_ts, oni_data_df[i,2:13])
  }

  ret = list()
  ret$oni_data = oni_data_df
  ret$oni_ts = as.numeric(oni_data_ts)
  return(ret) 
}