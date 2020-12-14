#id: f0c87e31
#key: 84f049d50dd8aa26e2e48aff8ba0e61f
library(tidyverse)
library(jsonlite)
library(rvest)

country_list <- c("gb","at","au","br","ca","de","fr","in","it","nl","nz","pl","ru","sg","us","za")

df <- data.frame()
df_2<- data.frame()
df_3<- data.frame()
df_4<- data.frame()

# for data scientists
for(i in country_list){
      file <- jsonlite::fromJSON(paste0("https://api.adzuna.com/v1/api/jobs/",i,"/search/1?app_id=f0c87e31&app_key=84f049d50dd8aa26e2e48aff8ba0e61f&results_per_page=50&what_or=data%20scientists%20statisticians"))
      df_new <- file$results 
      df <- bind_rows(df,df_new)
}
t <- df[,-c(2,6,15)]
df1 <- as.data.frame(df$category)
df2 <- as.data.frame(df$company)
df3 <- as.data.frame(df$location)
ddf <- cbind(t,df1,df2,df3)
ddf$area <- as.character(ddf$area)

for(i in country_list){
  file <- jsonlite::fromJSON(paste0("https://api.adzuna.com/v1/api/jobs/",i,"/search/2?app_id=f0c87e31&app_key=84f049d50dd8aa26e2e48aff8ba0e61f&results_per_page=50&what_or=data%20scientists%20statisticians"))
  df_new <- file$results 
  df_2 <- bind_rows(df_2,df_new)
}
t <- df_2[,-c(2,12,13)]
df1 <- as.data.frame(df_2$category)
df2 <- as.data.frame(df_2$company)
df3 <- as.data.frame(df_2$location)
ddf2 <- cbind(t,df1,df2,df3)
ddf2$area <- as.character(ddf2$area)
write.csv(ddf,"df2.csv")

for(i in country_list){
  file <- jsonlite::fromJSON(paste0("https://api.adzuna.com/v1/api/jobs/",i,"/search/3?app_id=f0c87e31&app_key=84f049d50dd8aa26e2e48aff8ba0e61f&results_per_page=50&what_or=data%20scientists%20statisticians"))
  df_new <- file$results 
  df_3 <- bind_rows(df_3,df_new)
}
t <- df_3[,-c(1,2,5)]
df1 <- as.data.frame(df_3$category)
df2 <- as.data.frame(df_3$company)
df3 <- as.data.frame(df_3$location)
ddf3 <- cbind(t,df1,df2,df3)
ddf3$area <- as.character(ddf3$area)
write.csv(ddf,"df3.csv")

for(i in country_list){
  file <- jsonlite::fromJSON(paste0("https://api.adzuna.com/v1/api/jobs/",i,"/search/4?app_id=f0c87e31&app_key=84f049d50dd8aa26e2e48aff8ba0e61f&results_per_page=50&what_or=data%20scientists%20statisticians"))
  df_new <- file$results 
  df_4 <- bind_rows(df_4,df_new)
}
t <- df_4[,-c(12,13,16)]
df1 <- as.data.frame(df_4$category)
df2 <- as.data.frame(df_4$company)
df3 <- as.data.frame(df_4$location)
ddf4 <- cbind(t,df1,df2,df3)
ddf4$area <- as.character(ddf4$area)
write.csv(ddf,"df4.csv")

dddf <- bind_rows(ddf,ddf2,ddf3,ddf4)
dddf <- dddf[,-c(23:31)]
dddf <- dddf[,-c(14,15,19,22)]

colnames(dddf)[16] <- "company_name"
colnames(dddf)[18] <- "location_name"
write.csv(dddf,"df.csv")

# us only
df_us <- data.frame()

for(i in 1:24){
  file <- jsonlite::fromJSON(paste0("https://api.adzuna.com/v1/api/jobs/us/search/",i,"?app_id=f0c87e31&app_key=84f049d50dd8aa26e2e48aff8ba0e61f&results_per_page=50&what_or=data%20scientists%20statisticians"))
  df_new <- file$results 
  df_us <- bind_rows(df_us,df_new)
}

df_us2 <- data.frame()
for(i in 25:48){
  file <- jsonlite::fromJSON(paste0("https://api.adzuna.com/v1/api/jobs/us/search/",i,"?app_id=f0c87e31&app_key=84f049d50dd8aa26e2e48aff8ba0e61f&results_per_page=50&what_or=data%20scientists%20statisticians"))
  df_new <- file$results 
  df_us2 <- bind_rows(df_us2,df_new)
}

df_us3 <- data.frame()
for(i in 49:72){
  file <- jsonlite::fromJSON(paste0("https://api.adzuna.com/v1/api/jobs/us/search/",i,"?app_id=f0c87e31&app_key=84f049d50dd8aa26e2e48aff8ba0e61f&results_per_page=50&what_or=data%20scientists%20statisticians"))
  df_new <- file$results 
  df_us3 <- bind_rows(df_us3,df_new)
}

df_us4 <- data.frame()
for(i in 73:96){
  file <- jsonlite::fromJSON(paste0("https://api.adzuna.com/v1/api/jobs/us/search/",i,"?app_id=f0c87e31&app_key=84f049d50dd8aa26e2e48aff8ba0e61f&results_per_page=50&what_or=data%20scientists%20statisticians"))
  df_new <- file$results 
  df_us4 <- bind_rows(df_us4,df_new)
}

df_us5 <- data.frame()
for(i in 97:120){
  file <- jsonlite::fromJSON(paste0("https://api.adzuna.com/v1/api/jobs/us/search/",i,"?app_id=f0c87e31&app_key=84f049d50dd8aa26e2e48aff8ba0e61f&results_per_page=50&what_or=data%20scientists%20statisticians"))
  df_new <- file$results 
  df_us5 <- bind_rows(df_us5,df_new)
}


df_us6 <- data.frame()
for(i in 121:144){
  file <- jsonlite::fromJSON(paste0("https://api.adzuna.com/v1/api/jobs/us/search/",i,"?app_id=f0c87e31&app_key=84f049d50dd8aa26e2e48aff8ba0e61f&results_per_page=50&what_or=data%20scientists%20statisticians"))
  df_new <- file$results 
  df_us6 <- bind_rows(df_us6,df_new)
}

df_us7 <- data.frame()
for(i in 145:168){
  file <- jsonlite::fromJSON(paste0("https://api.adzuna.com/v1/api/jobs/us/search/",i,"?app_id=f0c87e31&app_key=84f049d50dd8aa26e2e48aff8ba0e61f&results_per_page=50&what_or=data%20scientists%20statisticians"))
  df_new <- file$results 
  df_us7 <- bind_rows(df_us7,df_new)
}

full_us <- bind_rows(df_us,df_us2,df_us3,df_us4,df_us5,df_us6,df_us7)
t <- full_us[,-c(13,2,14)]
df1 <- as.data.frame(full_us$category)
df2 <- as.data.frame(full_us$company)
df3 <- as.data.frame(full_us$location)
ddf3 <- cbind(t,df1,df2,df3)
ddf3$area <- as.character(ddf3$area)
colnames(ddf3)[19] <- "company_name"
colnames(ddf3)[20] <- "location_name"
full_us <- ddf3[,-c(4:6,17,18,21)]
write.csv(full_us,"full_us.csv")

gb <- data.frame()
for(i in 1:100){
  file <- jsonlite::fromJSON(paste0("https://api.adzuna.com/v1/api/jobs/gb/search/",i,"?app_id=f0c87e31&app_key=84f049d50dd8aa26e2e48aff8ba0e61f&results_per_page=50&what_or=data%20scientist%20statistician"))
  df_new <- file$results 
  gb <- bind_rows(gb,df_new)
}


for (i in (1:length(gb$redirect_url))){
  detail <- tryCatch(
    html_text(html_node(read_html(as.character(gb$redirect_url[i])),'.job_details_body')),
    error=function(e){NA}
  )
  if (is.null(detail)){
    desc <- NA
  }
  gb$detail[i] <- detail
}

gb$detail <- gsub("\n","",gb$detail)
gb_withdesc <- filter(gb,is.na(gb$detail)==F)
t <- gb_withdesc[,-c(13,11,15)]
df1 <- as.data.frame(gb_withdesc$category)
df2 <- as.data.frame(gb_withdesc$company)
df3 <- as.data.frame(gb_withdesc$location)
gb_final <- cbind(t,df1,df2,df3)
gb_final$area <- as.character(gb_final$area)
write.csv(gb_final,"gb_final.csv",row.names = F)

for (i in (1:length(gb$redirect_url))){
  detail2 <- tryCatch(
    html_text(html_node(read_html(as.character(gb$redirect_url[i])),'.col-lg-offset-3')),
    error=function(e){NA}
  )
  if (is.null(detail2)){
    desc <- NA
  }
  gb$detail2[i] <- detail2
}

#'.job-description'

