#id: f0c87e31
#key: 84f049d50dd8aa26e2e48aff8ba0e61f
library(tidyverse)
library(jsonlite)

gb_file1 <- jsonlite::fromJSON("https://api.adzuna.com/v1/api/jobs/gb/search/1?app_id=f0c87e31&app_key=84f049d50dd8aa26e2e48aff8ba0e61f&results_per_page=50")
View(gb_file1$results)
gb_file2 <- jsonlite::fromJSON("https://api.adzuna.com/v1/api/jobs/gb/search/2?app_id=f0c87e31&app_key=84f049d50dd8aa26e2e48aff8ba0e61f&results_per_page=50")
View(gb_file2$results)
file <- cbind(gb_file1$results,gb_file2$results)
file <- bind_rows(gb_file1$results,gb_file2$results)


country_list <- c("gb","at","au","br","ca","de","fr","in","it","nl","nz","pl","ru","sg","us","za")
df <- data.frame()
df_larg <- data.frame()
for(i in country_list){
  for (p in 1:4) {
    file <- jsonlite::fromJSON(paste0("https://api.adzuna.com/v1/api/jobs/",i,"/search/",p,"?app_id=f0c87e31&app_key=84f049d50dd8aa26e2e48aff8ba0e61f&results_per_page=50"))
  df_new <- file$results 
  df_larg <- bind_rows(df,df_new)
  }
}
for(i in country_list){
      file <- jsonlite::fromJSON(paste0("https://api.adzuna.com/v1/api/jobs/",i,"/search/",p,"?app_id=f0c87e31&app_key=84f049d50dd8aa26e2e48aff8ba0e61f&results_per_page=50"))
      df_new <- file$results 
      df <- bind_rows(df,df_new)
}
t <- df[,-c(4,7,8)]
df1 <- as.data.frame(df$category)
df2 <- as.data.frame(df$company)
df3 <- as.data.frame(df$location)
ddf <- cbind(t,df1,df2,df3)
ddf$area <- as.character(ddf$area)
write.csv(ddf,"df1.csv")

