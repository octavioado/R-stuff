# use youtube to catch estonian marketing perception

library(tuber)
library(ggplot2)
library(gtools)
library(ggthemes)
options(stringsAsFactors = FALSE) 
library(plyr)

# login credentials
yt_oauth("939559094296-25cra5r2otufulsu629lskl3bp9egrnj.apps.googleusercontent.com", "Yj1s5c8MvblcaLny6yyNqlEV")

# work in estonia channel ID = UCWVjpu-NkHTaQW1dNanFseA
# visit estonia channel   ID = UCw8wdYrQcit2Aqj79ZZefww
# life of boris channel   ID = UCS5tt2z_DFvG7-39J3aE-bQ


getAllChannelVideos <- function(channel_id=NULL){
    
    channelAct <- list_channel_activities(channel_id = channel_id ,part = "contentDetails")
    df <-  do.call(rbind, lapply(channelAct$items, data.frame))
    
   
}


getVideoStatsDF <- function(video_id){
    stats <- get_stats(video_id)
    return(data.frame(t(unlist(stats))))
}

getVideoDetailsDF <- function(video_id){
    details <- get_video_details(video_id)
    return(data.frame(t(unlist(details))))
}



getVchannelStatsDF <- function(channel_id){
    cstats <- get_channel_stats(channel_id)
    return(data.frame(t(unlist(cstats))))
}

allActivities <- getAllChannelVideos("UCWVjpu-NkHTaQW1dNanFseA")

list_of_video_ids <- allActivities$videoId

allVideoStats <- ldply(list_of_video_ids, .fun = getVideoStatsDF)

allVideoDetails <- ldply(list_of_video_ids, .fun = getVideoDetailsDF)

ggplot(allVideoStats,aes(y=viewCount,x=commentCount,col = likeCount, shape= dislikeCount))+geom_point()
