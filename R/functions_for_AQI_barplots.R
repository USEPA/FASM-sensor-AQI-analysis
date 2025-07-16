# functions to go with the R script 'template_for_sensor_vs_monitor.R'

# use the source() function to load these functions into the environment when 
# working through the template script

if(!require("MazamaCoreUtils"))
  install.packages("MazamaCoreUtils")
if(!require("dplyr"))
  install.packages("dplyr")
if(!require("AirMonitor"))
  install.packages("AirMonitor")
if(!require("AirMonitorPlots"))
  devtools::install_github('mazamascience/AirMonitorPlots', build_vignettes=TRUE)
if(!require("tidyverse"))
  install.packages("tidyverse")
if(!require("AirSensor2"))
  devtools::install_github('mazamascience/AirSensor2')
if(!require("ggpubr"))
  install.packages("ggpubr")

library(MazamaCoreUtils)
library(dplyr)
library(AirMonitor)
library(AirMonitorPlots)
library(tidyverse)
library(AirSensor2)
library(ggpubr)

findAdjacentPairs <- function(
    sensor,
    airnow,
    radius = 500
) {
  
  # Add distance information to sensor$meta
  sensor$meta$airnow_closestDistance <- as.numeric(NA)
  sensor$meta$airnow_id <- as.character(NA)
  
  for ( i in seq_len(nrow(sensor$meta)) ) {
    distances <-
      MazamaLocationUtils::table_getDistanceFromTarget(
        airnow$meta,
        sensor$meta$longitude[i],
        sensor$meta$latitude[i]
      )
    minDistIndex <- which.min(distances$distanceFromTarget)
    sensor$meta$airnow_closestDistance[i] <- distances$distanceFromTarget[minDistIndex] # meters
    
    airnow_id <-
      airnow$meta %>%
      dplyr::filter(locationID == distances$locationID[minDistIndex]) %>%
      dplyr::pull(deviceDeploymentID)
    
    sensor$meta$airnow_id[i] <- airnow_id
  }
  
  # All sensors within 'radius' meters of an AirNow monitor
  sensor_near_airnow <-
    sensor %>%
    monitor_filter(airnow_closestDistance <= radius) %>%
    monitor_arrange(airnow_closestDistance)
  
  return(sensor_near_airnow)
  
}

create_AQI_unlisted <- function(
    sensor_adjacent,
    airnow_adjacent
) {
  
  # Calculate AQI categories for each sensor/monitor pair
  breaks <- c(-Inf, 9.0, 35.5, 55.5, 125.5, 225.5, Inf)
  
  pm25ToAQICategory <- function(x) {
    return(.bincode(x, breaks))
  }
  
  adjacent_pairs_AQI_list <- list()
  
  for ( i in seq_len(nrow(sensor_adjacent$meta)) ) {
    
    sensor_id <- sensor_adjacent$meta$deviceDeploymentID[i]
    airnow_id <- sensor_adjacent$meta$airnow_id[i]
    
    # Extract individual monitors
    sensor_mon <-
      sensor_adjacent %>%
      monitor_select(sensor_id)
    
    airnow_mon <-
      airnow_adjacent %>%
      monitor_select(sensor_mon$meta$airnow_id)
    
    # Create combined monitor
    mon <- AirMonitor::monitor_combine(airnow_mon, sensor_mon)
    
    # Calculate AQI category
    AQI_categories <-
      mon %>%
      monitor_mutate(pm25ToAQICategory) %>%
      monitor_getData()
    
    # Makes a list of AQI categories for each adjacent pair
    adjacent_pairs_AQI_list[[i]] <- AQI_categories
  }
  
  # De-selecting the datetime columns for each data frame in the list
  for (i in seq_along(adjacent_pairs_AQI_list)) {
    
    adjacent_pairs_AQI_list[[i]] <- adjacent_pairs_AQI_list[[i]] %>%
      
      dplyr::select(-datetime)
    
  }
  
  # Creating unlisted AQI data frame 
  airnow_AQI_df <- data.frame(matrix(nrow = nrow(airnow_adjacent$data), 
                                     ncol = nrow(sensor_adjacent$meta)))
  sensor_AQI_df <- data.frame(matrix(nrow = nrow(airnow_adjacent$data), 
                                     ncol = nrow(sensor_adjacent$meta)))
  
  for (i in seq_along(adjacent_pairs_AQI_list)) {
    
    airnow_AQI_df[i] <- as.data.frame(adjacent_pairs_AQI_list[[i]][1])
    
    airnow_AQI_unlist <- data.frame(airnow = unlist(airnow_AQI_df))
    
    sensor_AQI_df[i] <- as.data.frame(adjacent_pairs_AQI_list[[i]][2])
    
    sensor_AQI_unlist <- data.frame(sensor = unlist(sensor_AQI_df))
    
    AQI_unlisted <- cbind(airnow_AQI_unlist, sensor_AQI_unlist)
    
    AQI_unlisted <- na.omit(AQI_unlisted)
  }
  
  return(AQI_unlisted)
  
}


create_combined_barplot <- function(
    AQI_unlisted,
    threshold_1,
    threshold_2
) {
  
  # Calculate the deviation between the sensor and monitor AQI category
  AQI_diff <- AQI_unlisted[,2]-    # sensor minus monitor
    AQI_unlisted[,1]
  
  AQI_diff<-as.factor(AQI_diff)
  
  AQI_diff_df <- as.data.frame(table(AQI_diff))
  
  # Wrangle data frame into format suitable for plotting frequencies 
  AQI_diff_df <- AQI_diff_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 2)) %>%
    dplyr::rename(deviations = "AQI_diff",
                  count = "Freq",
                  percentage = "percent")
  
  # Create bar plot of deviations for all AQI categories combined 
  plot <- ggplot(AQI_diff_df, aes(x=deviations, y=percentage)) +
    geom_bar(stat="identity",
             color = "black",
             fill = "gray") +
    scale_x_discrete(limits = factor(c(-5:5)))+
    geom_text(aes(label=percentage), vjust = -0.6, color="black", size = 5) +
    #geom_hline(yintercept = 5, lty = 2, linewidth = 0.8) +
    #geom_hline(yintercept = 1, lty = 2, linewidth = 0.8) +
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, linewidth = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, linewidth = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, linewidth = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, linewidth = 1) +
    labs(y = "frequency (%)",
         x = "AQI Difference (Sensor - AirNow)") +
    theme_minimal(base_size = 16) +
    ylim(0, 100)
  
  return(plot)
}

create_categories_barplot_hazardous <- function(
    AQI_unlisted,
    threshold_1,
    threshold_2
) {
  
  # ----- AirNow = Good --------------------------------------------------------
  
  # Calculate AQI deviations when AirNow reports good
  AQI_unlisted_good <- AQI_unlisted %>%
    dplyr::filter(airnow == '1')
  
  AQI_diff_good <- AQI_unlisted_good[,2]- 
    AQI_unlisted_good[,1]
  
  AQI_diff_good_df <- as.data.frame(table(AQI_diff_good))
  
  # Wranlge data frame into format suitable for plotting 
  AQI_diff_good_df <- AQI_diff_good_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 0)) %>%
    dplyr::rename(deviations = "AQI_diff_good",
                  count = "Freq",
                  percentage = "percent")
  
  # Create dummy variable to be used in plot header 
  AQI_diff_good_df$tempvar <- "AirNow = Good"
  
  # Calculate the number of observations where AirNow reports good
  obs <- sum(AQI_diff_good_df$count)
  
  # Create plot of AQI deviations 
  good <- ggplot(AQI_diff_good_df, 
                 aes(x=deviations, y=percentage, fill=deviations)) +
    geom_bar(stat="identity", color = "black") +
    coord_cartesian(ylim=c(0,100))+
    scale_fill_manual(values=c("0" = "#00E400",
                               "1" = "#FFFF00",
                               "2" = "#FF7E00",
                               "3" = "#FF0000",
                               "4" = "#8F3F97",
                               "5" = "#7E0023"))+
    scale_x_discrete(limits = factor(c(-5:5)))+
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(size=15))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, size = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, size = 1) +
    geom_text(aes(label=percentage), vjust = -0.3, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 5) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#00E400"),
          strip.text = element_text(size=15, colour="black"))
  
  # ----- AirNow = Moderate ----------------------------------------------------
  
  # Calculate AQI deviations when AirNow reports moderate
  AQI_unlisted_moderate <- AQI_unlisted %>%
    dplyr::filter(airnow == '2')
  
  AQI_diff_moderate <- AQI_unlisted_moderate[,2]- 
    AQI_unlisted_moderate[,1]
  
  AQI_diff_moderate_df <- as.data.frame(table(AQI_diff_moderate))
  
  # Wranlge data frame into format suitable for plotting 
  AQI_diff_moderate_df <- AQI_diff_moderate_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 0)) %>%
    dplyr::rename(deviations = "AQI_diff_moderate",
                  count = "Freq",
                  percentage = "percent")
  
  # Create dummy variable to be used in plot header 
  AQI_diff_moderate_df$tempvar <- "AirNow = Moderate"
  
  # Calculate the number of observations where AirNow reports moderate
  obs <- sum(AQI_diff_moderate_df$count)
  
  # Create plot of AQI deviations 
  moderate <- ggplot(AQI_diff_moderate_df, 
                     aes(x=deviations, y=percentage, fill=deviations)) +
    geom_bar(stat="identity", color = "black") +
    coord_cartesian(ylim=c(0,100))+
    scale_fill_manual(values=c("-1" = "#00E400",
                               "0" = "#FFFF00",
                               "1" = "#FF7E00",
                               "2" = "#FF0000",
                               "3" = "#8F3F97",
                               "4" = "#7E0023"))+
    scale_x_discrete(limits = factor(c(-5:5)))+
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(size=15))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, size = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, size = 1) +
    geom_text(aes(label=percentage), vjust = -0.3, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 5) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#FFFF00"),
          strip.text = element_text(size=15, colour="black"))
  
  # ----- AirNow = USG ---------------------------------------------------------
  
  # Calculate AQI deviations when AirNow reports USG
  AQI_unlisted_USG <- AQI_unlisted %>%
    dplyr::filter(airnow == '3')
  
  AQI_diff_USG <- AQI_unlisted_USG[,2]- 
    AQI_unlisted_USG[,1]
  
  AQI_diff_USG_df <- as.data.frame(table(AQI_diff_USG))
  
  # Wranlge data frame into format suitable for plotting 
  AQI_diff_USG_df <- AQI_diff_USG_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 0)) %>%
    dplyr::rename(deviations = "AQI_diff_USG",
                  count = "Freq",
                  percentage = "percent")
  
  # Create dummy variable to be used in plot header 
  AQI_diff_USG_df$tempvar <- "AirNow = USG"
  
  # Calculate the number of observations where AirNow reports usg
  obs <- sum(AQI_diff_USG_df$count)
  
  # Create plot of AQI deviations 
  usg <- ggplot(AQI_diff_USG_df, 
                aes(x=deviations, y=percentage, fill=deviations)) +
    geom_bar(stat="identity", color = "black") +
    coord_cartesian(ylim=c(0,100))+
    scale_fill_manual(values=c("-2" = "#00E400",
                               "-1" = "#FFFF00",
                               "0" = "#FF7E00",
                               "1" = "#FF0000",
                               "2" = "#8F3F97",
                               "3" = "#7E0023"))+
    scale_x_discrete(limits = factor(c(-5:5)))+
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(size=15))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, size = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, size = 1) +
    geom_text(aes(label=percentage), vjust = -0.3, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 5) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#FF7E00"),
          strip.text = element_text(size=15, colour="black"))
  
  # ----- AirNow = Unhealthy ---------------------------------------------------
  
  # Calculate AQI deviations when AirNow reports unhealthy
  AQI_unlisted_unhealthy <- AQI_unlisted %>%
    dplyr::filter(airnow == '4')
  
  AQI_diff_unhealthy <- AQI_unlisted_unhealthy[,2]- 
    AQI_unlisted_unhealthy[,1]
  
  AQI_diff_unhealthy_df <- as.data.frame(table(AQI_diff_unhealthy))
  
  # Wranlge data frame into format suitable for plotting 
  AQI_diff_unhealthy_df <- AQI_diff_unhealthy_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 0)) %>%
    dplyr::rename(deviations = "AQI_diff_unhealthy",
                  count = "Freq",
                  percentage = "percent")
  
  # Create dummy variable to be used in plot header 
  AQI_diff_unhealthy_df$tempvar <- "AirNow = Unhealthy"
  
  # Calculate the number of observations where AirNow reports unhealthy
  obs <- sum(AQI_diff_unhealthy_df$count)
  
  # Create plot of AQI deviations 
  unhealthy <- ggplot(AQI_diff_unhealthy_df, 
                      aes(x=deviations, y=percentage, fill=deviations)) +
    geom_bar(stat="identity", color = "black") +
    coord_cartesian(ylim=c(0,100))+
    scale_fill_manual(values=c("-3" = "#00E400",
                               "-2" = "#FFFF00",
                               "-1" = "#FF7E00",
                               "0" = "#FF0000",
                               "1" = "#8F3F97",
                               "2" = "#7E0023"))+
    scale_x_discrete(limits = factor(c(-5:5)))+
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(size=15))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, size = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, size = 1) +
    geom_text(aes(label=percentage), vjust = -0.3, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 5) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#FF0000"),
          strip.text = element_text(size=15, colour="black"))
  
  # ----- AirNow = Very Unhealthy ----------------------------------------------
  
  # Calculate AQI deviations when AirNow reports very unhealthy
  AQI_unlisted_veryunhealthy <- AQI_unlisted %>%
    dplyr::filter(airnow == '5')
  
  AQI_diff_veryunhealthy <- AQI_unlisted_veryunhealthy[,2]- 
    AQI_unlisted_veryunhealthy[,1]
  
  AQI_diff_veryunhealthy_df <- as.data.frame(table(AQI_diff_veryunhealthy))
  
  # Wranlge data frame into format suitable for plotting 
  AQI_diff_veryunhealthy_df <- AQI_diff_veryunhealthy_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 0)) %>%
    dplyr::rename(deviations = "AQI_diff_veryunhealthy",
                  count = "Freq",
                  percentage = "percent")
  
  # Create dummy variable to be used in plot header 
  AQI_diff_veryunhealthy_df$tempvar <- "AirNow = Very Unhealthy"
  
  # Calculate the number of observations where AirNow reports very unhealthy
  obs <- sum(AQI_diff_veryunhealthy_df$count)
  
  # Create plot of AQI deviations 
  veryunhealthy <- ggplot(AQI_diff_veryunhealthy_df, 
                          aes(x=deviations, y=percentage, fill=deviations)) +
    geom_bar(stat="identity", color = "black") +
    coord_cartesian(ylim=c(0,100))+
    scale_fill_manual(values=c("-4" = "#00E400",
                               "-3" = "#FFFF00",
                               "-2" = "#FF7E00",
                               "-1" = "#FF0000",
                               "0" = "#8F3F97",
                               "1" = "#7E0023"))+
    scale_x_discrete(limits = factor(c(-5:5)))+
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(size=15))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, size = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, size = 1) +
    geom_text(aes(label=percentage), vjust = -.3, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 5) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#8F3F97"),
          strip.text = element_text(size=15, colour="white"))
  
  # ----- AirNow = Hazardous ---------------------------------------------------
  
  # Calculate AQI deviations when AirNow reports hazardous
  AQI_unlisted_hazardous <- AQI_unlisted %>%
    dplyr::filter(airnow == '6')
  
  AQI_diff_hazardous <- AQI_unlisted_hazardous[,2]- 
    AQI_unlisted_hazardous[,1]
  
  AQI_diff_hazardous_df <- as.data.frame(table(AQI_diff_hazardous))
  
  # Wranlge data frame into format suitable for plotting 
  AQI_diff_hazardous_df <- AQI_diff_hazardous_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 0)) %>%
    dplyr::rename(deviations = "AQI_diff_hazardous",
                  count = "Freq",
                  percentage = "percent")
  
  # Create dummy variable to be used in plot header 
  AQI_diff_hazardous_df$tempvar <- "AirNow = Hazardous"
  
  # Calculate the number of observations where AirNow reports hazardous
  obs <- sum(AQI_diff_hazardous_df$count)
  
  # Create plot of AQI deviations 
  hazardous <- ggplot(AQI_diff_hazardous_df, 
                      aes(x=deviations, y=percentage, fill=deviations)) +
    geom_bar(stat="identity", color = "black") +
    coord_cartesian(ylim=c(0,100))+
    scale_fill_manual(values=c("-5" = "#00E400",
                               "-4" = "#FFFF00",
                               "-3" = "#FF7E00",
                               "-2" = "#FF0000",
                               "-1" = "#8F3F97",
                               "0" = "#7E0023"))+
    scale_x_discrete(limits = factor(c(-5:5)))+
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(size=15))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, size = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, size = 1) +
    geom_text(aes(label=percentage), vjust = -0.3, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 5) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#7E0023"),
          strip.text = element_text(size=15, colour="white"))
  
  # ----- arrange all 6 plots into a grid --------------------------------------
  plot <- ggarrange(good, moderate, usg, unhealthy, veryunhealthy, hazardous,
                    ncol = 3, nrow = 2, common.legend = FALSE)
  
  return(plot)
  
}

create_categories_barplot_unhealthy <- function(
    AQI_unlisted,
    threshold_1,
    threshold_2
) {
  
  # ----- AirNow = Good --------------------------------------------------------
  
  # Calculate AQI deviations when AirNow reports good
  AQI_unlisted_good <- AQI_unlisted %>%
    dplyr::filter(airnow == '1')
  
  AQI_diff_good <- AQI_unlisted_good[,2]- 
    AQI_unlisted_good[,1]
  
  AQI_diff_good_df <- as.data.frame(table(AQI_diff_good))
  
  # Wranlge data frame into format suitable for plotting 
  AQI_diff_good_df <- AQI_diff_good_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 0)) %>%
    dplyr::rename(deviations = "AQI_diff_good",
                  count = "Freq",
                  percentage = "percent")
  
  # Create dummy variable to be used in plot header 
  AQI_diff_good_df$tempvar <- "AirNow = Good"
  
  # Calculate the number of observations where AirNow reports good
  obs <- sum(AQI_diff_good_df$count)
  
  # Create plot of AQI deviations 
  good <- ggplot(AQI_diff_good_df, 
                 aes(x=deviations, y=percentage, fill=deviations)) +
    geom_bar(stat="identity", color = "black") +
    coord_cartesian(ylim=c(0,100))+
    scale_fill_manual(values=c("0" = "#00E400",
                               "1" = "#FFFF00",
                               "2" = "#FF7E00",
                               "3" = "#FF0000",
                               "4" = "#8F3F97",
                               "5" = "#7E0023"))+
    scale_x_discrete(limits = factor(c(-5:5)))+
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(size=15))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, size = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, size = 1) +
    geom_text(aes(label=percentage), vjust = -0.3, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 5) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#00E400"),
          strip.text = element_text(size=15, colour="black"))
  
  # ----- AirNow = Moderate ----------------------------------------------------
  
  # Calculate AQI deviations when AirNow reports moderate
  AQI_unlisted_moderate <- AQI_unlisted %>%
    dplyr::filter(airnow == '2')
  
  AQI_diff_moderate <- AQI_unlisted_moderate[,2]- 
    AQI_unlisted_moderate[,1]
  
  AQI_diff_moderate_df <- as.data.frame(table(AQI_diff_moderate))
  
  # Wranlge data frame into format suitable for plotting 
  AQI_diff_moderate_df <- AQI_diff_moderate_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 0)) %>%
    dplyr::rename(deviations = "AQI_diff_moderate",
                  count = "Freq",
                  percentage = "percent")
  
  # Create dummy variable to be used in plot header 
  AQI_diff_moderate_df$tempvar <- "AirNow = Moderate"
  
  # Calculate the number of observations where AirNow reports moderate
  obs <- sum(AQI_diff_moderate_df$count)
  
  # Create plot of AQI deviations 
  moderate <- ggplot(AQI_diff_moderate_df, 
                     aes(x=deviations, y=percentage, fill=deviations)) +
    geom_bar(stat="identity", color = "black") +
    coord_cartesian(ylim=c(0,100))+
    scale_fill_manual(values=c("-1" = "#00E400",
                               "0" = "#FFFF00",
                               "1" = "#FF7E00",
                               "2" = "#FF0000",
                               "3" = "#8F3F97",
                               "4" = "#7E0023"))+
    scale_x_discrete(limits = factor(c(-5:5)))+
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(size=15))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, size = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, size = 1) +
    geom_text(aes(label=percentage), vjust = -0.3, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 5) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#FFFF00"),
          strip.text = element_text(size=15, colour="black"))
  
  # ----- AirNow = USG ---------------------------------------------------------
  
  # Calculate AQI deviations when AirNow reports USG
  AQI_unlisted_USG <- AQI_unlisted %>%
    dplyr::filter(airnow == '3')
  
  AQI_diff_USG <- AQI_unlisted_USG[,2]- 
    AQI_unlisted_USG[,1]
  
  AQI_diff_USG_df <- as.data.frame(table(AQI_diff_USG))
  
  # Wranlge data frame into format suitable for plotting 
  AQI_diff_USG_df <- AQI_diff_USG_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 0)) %>%
    dplyr::rename(deviations = "AQI_diff_USG",
                  count = "Freq",
                  percentage = "percent")
  
  # Create dummy variable to be used in plot header 
  AQI_diff_USG_df$tempvar <- "AirNow = USG"
  
  # Calculate the number of observations where AirNow reports usg
  obs <- sum(AQI_diff_USG_df$count)
  
  # Create plot of AQI deviations 
  usg <- ggplot(AQI_diff_USG_df, 
                aes(x=deviations, y=percentage, fill=deviations)) +
    geom_bar(stat="identity", color = "black") +
    coord_cartesian(ylim=c(0,100))+
    scale_fill_manual(values=c("-2" = "#00E400",
                               "-1" = "#FFFF00",
                               "0" = "#FF7E00",
                               "1" = "#FF0000",
                               "2" = "#8F3F97",
                               "3" = "#7E0023"))+
    scale_x_discrete(limits = factor(c(-5:5)))+
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(size=15))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, size = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, size = 1) +
    geom_text(aes(label=percentage), vjust = -0.3, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 5) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#FF7E00"),
          strip.text = element_text(size=15, colour="black"))
  
  # ----- AirNow = Unhealthy ---------------------------------------------------
  
  # Calculate AQI deviations when AirNow reports unhealthy
  AQI_unlisted_unhealthy <- AQI_unlisted %>%
    dplyr::filter(airnow == '4')
  
  AQI_diff_unhealthy <- AQI_unlisted_unhealthy[,2]- 
    AQI_unlisted_unhealthy[,1]
  
  AQI_diff_unhealthy_df <- as.data.frame(table(AQI_diff_unhealthy))
  
  # Wranlge data frame into format suitable for plotting 
  AQI_diff_unhealthy_df <- AQI_diff_unhealthy_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 0)) %>%
    dplyr::rename(deviations = "AQI_diff_unhealthy",
                  count = "Freq",
                  percentage = "percent")
  
  # Create dummy variable to be used in plot header 
  AQI_diff_unhealthy_df$tempvar <- "AirNow = Unhealthy"
  
  # Calculate the number of observations where AirNow reports unhealthy
  obs <- sum(AQI_diff_unhealthy_df$count)
  
  # Create plot of AQI deviations 
  unhealthy <- ggplot(AQI_diff_unhealthy_df, 
                      aes(x=deviations, y=percentage, fill=deviations)) +
    geom_bar(stat="identity", color = "black") +
    coord_cartesian(ylim=c(0,100))+
    scale_fill_manual(values=c("-3" = "#00E400",
                               "-2" = "#FFFF00",
                               "-1" = "#FF7E00",
                               "0" = "#FF0000",
                               "1" = "#8F3F97",
                               "2" = "#7E0023"))+
    scale_x_discrete(limits = factor(c(-5:5)))+
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(size=15))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, size = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, size = 1) +
    geom_text(aes(label=percentage), vjust = -0.7, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 5) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#FF0000"),
          strip.text = element_text(size=15, colour="black"))
  
  # ----- arrange all 4 plots into a grid --------------------------------------
  plot <- ggarrange(good, moderate, usg, unhealthy,
                    ncol = 2, nrow = 2, common.legend = FALSE)
  
  return(plot)
  
}


create_categories_barplot_very_unhealthy <- function(
    AQI_unlisted,
    threshold_1,
    threshold_2
) {
  
  # ----- AirNow = Good --------------------------------------------------------
  
  # Calculate AQI deviations when AirNow reports good
  AQI_unlisted_good <- AQI_unlisted %>%
    dplyr::filter(airnow == '1')
  
  AQI_diff_good <- AQI_unlisted_good[,2]- 
    AQI_unlisted_good[,1]
  
  AQI_diff_good_df <- as.data.frame(table(AQI_diff_good))
  
  # Wranlge data frame into format suitable for plotting 
  AQI_diff_good_df <- AQI_diff_good_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 0)) %>%
    dplyr::rename(deviations = "AQI_diff_good",
                  count = "Freq",
                  percentage = "percent")
  
  # Create dummy variable to be used in plot header 
  AQI_diff_good_df$tempvar <- "AirNow = Good"
  
  # Calculate the number of observations where AirNow reports good
  obs <- sum(AQI_diff_good_df$count)
  
  # Create plot of AQI deviations 
  good <- ggplot(AQI_diff_good_df, 
                 aes(x=deviations, y=percentage, fill=deviations)) +
    geom_bar(stat="identity", color = "black") +
    coord_cartesian(ylim=c(0,100))+
    scale_fill_manual(values=c("0" = "#00E400",
                               "1" = "#FFFF00",
                               "2" = "#FF7E00",
                               "3" = "#FF0000",
                               "4" = "#8F3F97",
                               "5" = "#7E0023"))+
    scale_x_discrete(limits = factor(c(-5:5)))+
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(size=15))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, size = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, size = 1) +
    geom_text(aes(label=percentage), vjust = -0.3, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 5) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#00E400"),
          strip.text = element_text(size=15, colour="black"))
  
  # ----- AirNow = Moderate ----------------------------------------------------
  
  # Calculate AQI deviations when AirNow reports moderate
  AQI_unlisted_moderate <- AQI_unlisted %>%
    dplyr::filter(airnow == '2')
  
  AQI_diff_moderate <- AQI_unlisted_moderate[,2]- 
    AQI_unlisted_moderate[,1]
  
  AQI_diff_moderate_df <- as.data.frame(table(AQI_diff_moderate))
  
  # Wranlge data frame into format suitable for plotting 
  AQI_diff_moderate_df <- AQI_diff_moderate_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 0)) %>%
    dplyr::rename(deviations = "AQI_diff_moderate",
                  count = "Freq",
                  percentage = "percent")
  
  # Create dummy variable to be used in plot header 
  AQI_diff_moderate_df$tempvar <- "AirNow = Moderate"
  
  # Calculate the number of observations where AirNow reports moderate
  obs <- sum(AQI_diff_moderate_df$count)
  
  # Create plot of AQI deviations 
  moderate <- ggplot(AQI_diff_moderate_df, 
                     aes(x=deviations, y=percentage, fill=deviations)) +
    geom_bar(stat="identity", color = "black") +
    coord_cartesian(ylim=c(0,100))+
    scale_fill_manual(values=c("-1" = "#00E400",
                               "0" = "#FFFF00",
                               "1" = "#FF7E00",
                               "2" = "#FF0000",
                               "3" = "#8F3F97",
                               "4" = "#7E0023"))+
    scale_x_discrete(limits = factor(c(-5:5)))+
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(size=15))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, size = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, size = 1) +
    geom_text(aes(label=percentage), vjust = -0.8, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 5) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#FFFF00"),
          strip.text = element_text(size=15, colour="black"))
  
  # ----- AirNow = USG ---------------------------------------------------------
  
  # Calculate AQI deviations when AirNow reports USG
  AQI_unlisted_USG <- AQI_unlisted %>%
    dplyr::filter(airnow == '3')
  
  AQI_diff_USG <- AQI_unlisted_USG[,2]- 
    AQI_unlisted_USG[,1]
  
  AQI_diff_USG_df <- as.data.frame(table(AQI_diff_USG))
  
  # Wranlge data frame into format suitable for plotting 
  AQI_diff_USG_df <- AQI_diff_USG_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 0)) %>%
    dplyr::rename(deviations = "AQI_diff_USG",
                  count = "Freq",
                  percentage = "percent")
  
  # Create dummy variable to be used in plot header 
  AQI_diff_USG_df$tempvar <- "AirNow = USG"
  
  # Calculate the number of observations where AirNow reports usg
  obs <- sum(AQI_diff_USG_df$count)
  
  # Create plot of AQI deviations 
  usg <- ggplot(AQI_diff_USG_df, 
                aes(x=deviations, y=percentage, fill=deviations)) +
    geom_bar(stat="identity", color = "black") +
    coord_cartesian(ylim=c(0,100))+
    scale_fill_manual(values=c("-2" = "#00E400",
                               "-1" = "#FFFF00",
                               "0" = "#FF7E00",
                               "1" = "#FF0000",
                               "2" = "#8F3F97",
                               "3" = "#7E0023"))+
    scale_x_discrete(limits = factor(c(-5:5)))+
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(size=15))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, size = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, size = 1) +
    geom_text(aes(label=percentage), vjust = -0.3, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 5) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#FF7E00"),
          strip.text = element_text(size=15, colour="black"))
  
  # ----- AirNow = Unhealthy ---------------------------------------------------
  
  # Calculate AQI deviations when AirNow reports unhealthy
  AQI_unlisted_unhealthy <- AQI_unlisted %>%
    dplyr::filter(airnow == '4')
  
  AQI_diff_unhealthy <- AQI_unlisted_unhealthy[,2]- 
    AQI_unlisted_unhealthy[,1]
  
  AQI_diff_unhealthy_df <- as.data.frame(table(AQI_diff_unhealthy))
  
  # Wranlge data frame into format suitable for plotting 
  AQI_diff_unhealthy_df <- AQI_diff_unhealthy_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 0)) %>%
    dplyr::rename(deviations = "AQI_diff_unhealthy",
                  count = "Freq",
                  percentage = "percent")
  
  # Create dummy variable to be used in plot header 
  AQI_diff_unhealthy_df$tempvar <- "AirNow = Unhealthy"
  
  # Calculate the number of observations where AirNow reports unhealthy
  obs <- sum(AQI_diff_unhealthy_df$count)
  
  # Create plot of AQI deviations 
  unhealthy <- ggplot(AQI_diff_unhealthy_df, 
                      aes(x=deviations, y=percentage, fill=deviations)) +
    geom_bar(stat="identity", color = "black") +
    coord_cartesian(ylim=c(0,100))+
    scale_fill_manual(values=c("-3" = "#00E400",
                               "-2" = "#FFFF00",
                               "-1" = "#FF7E00",
                               "0" = "#FF0000",
                               "1" = "#8F3F97",
                               "2" = "#7E0023"))+
    scale_x_discrete(limits = factor(c(-5:5)))+
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(size=15))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, size = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, size = 1) +
    geom_text(aes(label=percentage), vjust = -0.3, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 5) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#FF0000"),
          strip.text = element_text(size=15, colour="black"))
  
  # ----- AirNow = Very Unhealthy ----------------------------------------------
  
  # Calculate AQI deviations when AirNow reports very unhealthy
  AQI_unlisted_veryunhealthy <- AQI_unlisted %>%
    dplyr::filter(airnow == '5')
  
  AQI_diff_veryunhealthy <- AQI_unlisted_veryunhealthy[,2]- 
    AQI_unlisted_veryunhealthy[,1]
  
  AQI_diff_veryunhealthy_df <- as.data.frame(table(AQI_diff_veryunhealthy))
  
  # Wranlge data frame into format suitable for plotting 
  AQI_diff_veryunhealthy_df <- AQI_diff_veryunhealthy_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 0)) %>%
    dplyr::rename(deviations = "AQI_diff_veryunhealthy",
                  count = "Freq",
                  percentage = "percent")
  
  # Create dummy variable to be used in plot header 
  AQI_diff_veryunhealthy_df$tempvar <- "AirNow = Very Unhealthy"
  
  # Calculate the number of observations where AirNow reports very unhealthy
  obs <- sum(AQI_diff_veryunhealthy_df$count)
  
  # Create plot of AQI deviations 
  veryunhealthy <- ggplot(AQI_diff_veryunhealthy_df, 
                          aes(x=deviations, y=percentage, fill=deviations)) +
    geom_bar(stat="identity", color = "black") +
    coord_cartesian(ylim=c(0,100))+
    scale_fill_manual(values=c("-4" = "#00E400",
                               "-3" = "#FFFF00",
                               "-2" = "#FF7E00",
                               "-1" = "#FF0000",
                               "0" = "#8F3F97",
                               "1" = "#7E0023"))+
    scale_x_discrete(limits = factor(c(-5:5)))+
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(size=15))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, size = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, size = 1) +
    geom_text(aes(label=percentage), vjust = -1.5, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 5) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#8F3F97"),
          strip.text = element_text(size=15, colour="white"))
  
  # ----- arrange all 5 plots into a grid --------------------------------------
  plot <- ggarrange(good, moderate, usg, unhealthy, veryunhealthy, 
                    ncol = 3, nrow = 2, common.legend = FALSE)
  
  return(plot)
  
}

create_categories_barplot_usg <- function(
    AQI_unlisted,
    threshold_1,
    threshold_2
) {
  
  # ----- AirNow = Good --------------------------------------------------------
  
  # Calculate AQI deviations when AirNow reports good
  AQI_unlisted_good <- AQI_unlisted %>%
    dplyr::filter(airnow == '1')
  
  AQI_diff_good <- AQI_unlisted_good[,2]- 
    AQI_unlisted_good[,1]
  
  AQI_diff_good_df <- as.data.frame(table(AQI_diff_good))
  
  # Wranlge data frame into format suitable for plotting 
  AQI_diff_good_df <- AQI_diff_good_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 0)) %>%
    dplyr::rename(deviations = "AQI_diff_good",
                  count = "Freq",
                  percentage = "percent")
  
  # Create dummy variable to be used in plot header 
  AQI_diff_good_df$tempvar <- "AirNow = Good"
  
  # Calculate the number of observations where AirNow reports good
  obs <- sum(AQI_diff_good_df$count)
  
  # Create plot of AQI deviations 
  good <- ggplot(AQI_diff_good_df, 
                 aes(x=deviations, y=percentage, fill=deviations)) +
    geom_bar(stat="identity", color = "black") +
    coord_cartesian(ylim=c(0,100))+
    scale_fill_manual(values=c("0" = "#00E400",
                               "1" = "#FFFF00",
                               "2" = "#FF7E00",
                               "3" = "#FF0000",
                               "4" = "#8F3F97",
                               "5" = "#7E0023"))+
    scale_x_discrete(limits = factor(c(-5:5)))+
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(size=15))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, size = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, size = 1) +
    geom_text(aes(label=percentage), vjust = -0.3, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 5) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#00E400"),
          strip.text = element_text(size=15, colour="black"))
  
  # ----- AirNow = Moderate ----------------------------------------------------
  
  # Calculate AQI deviations when AirNow reports moderate
  AQI_unlisted_moderate <- AQI_unlisted %>%
    dplyr::filter(airnow == '2')
  
  AQI_diff_moderate <- AQI_unlisted_moderate[,2]- 
    AQI_unlisted_moderate[,1]
  
  AQI_diff_moderate_df <- as.data.frame(table(AQI_diff_moderate))
  
  # Wranlge data frame into format suitable for plotting 
  AQI_diff_moderate_df <- AQI_diff_moderate_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 0)) %>%
    dplyr::rename(deviations = "AQI_diff_moderate",
                  count = "Freq",
                  percentage = "percent")
  
  # Create dummy variable to be used in plot header 
  AQI_diff_moderate_df$tempvar <- "AirNow = Moderate"
  
  # Calculate the number of observations where AirNow reports moderate
  obs <- sum(AQI_diff_moderate_df$count)
  
  # Create plot of AQI deviations 
  moderate <- ggplot(AQI_diff_moderate_df, 
                     aes(x=deviations, y=percentage, fill=deviations)) +
    geom_bar(stat="identity", color = "black") +
    coord_cartesian(ylim=c(0,100))+
    scale_fill_manual(values=c("-1" = "#00E400",
                               "0" = "#FFFF00",
                               "1" = "#FF7E00",
                               "2" = "#FF0000",
                               "3" = "#8F3F97",
                               "4" = "#7E0023"))+
    scale_x_discrete(limits = factor(c(-5:5)))+
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(size=15))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, size = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, size = 1) +
    geom_text(aes(label=percentage), vjust = -0.3, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 5) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#FFFF00"),
          strip.text = element_text(size=15, colour="black"))
  
  # ----- AirNow = USG ---------------------------------------------------------
  
  # Calculate AQI deviations when AirNow reports USG
  AQI_unlisted_USG <- AQI_unlisted %>%
    dplyr::filter(airnow == '3')
  
  AQI_diff_USG <- AQI_unlisted_USG[,2]- 
    AQI_unlisted_USG[,1]
  
  AQI_diff_USG_df <- as.data.frame(table(AQI_diff_USG))
  
  # Wranlge data frame into format suitable for plotting 
  AQI_diff_USG_df <- AQI_diff_USG_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 0)) %>%
    dplyr::rename(deviations = "AQI_diff_USG",
                  count = "Freq",
                  percentage = "percent")
  
  # Create dummy variable to be used in plot header 
  AQI_diff_USG_df$tempvar <- "AirNow = USG"
  
  # Calculate the number of observations where AirNow reports usg
  obs <- sum(AQI_diff_USG_df$count)
  
  # Create plot of AQI deviations 
  usg <- ggplot(AQI_diff_USG_df, 
                aes(x=deviations, y=percentage, fill=deviations)) +
    geom_bar(stat="identity", color = "black") +
    coord_cartesian(ylim=c(0,100))+
    scale_fill_manual(values=c("-2" = "#00E400",
                               "-1" = "#FFFF00",
                               "0" = "#FF7E00",
                               "1" = "#FF0000",
                               "2" = "#8F3F97",
                               "3" = "#7E0023"))+
    scale_x_discrete(limits = factor(c(-5:5)))+
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(size=15))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, size = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, size = 1) +
    geom_text(aes(label=percentage), vjust = -0.3, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 5) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#FF7E00"),
          strip.text = element_text(size=15, colour="black"))
  
  # ----- arrange all 3 plots into a grid --------------------------------------
  plot <- ggarrange(good, moderate, usg, 
                    ncol = 2, nrow = 2, common.legend = FALSE)
  
  return(plot)
  
}

create_categories_barplot_moderate <- function(
    AQI_unlisted,
    threshold_1,
    threshold_2
) {
  
  # ----- AirNow = Good --------------------------------------------------------
  
  # Calculate AQI deviations when AirNow reports good
  AQI_unlisted_good <- AQI_unlisted %>%
    dplyr::filter(airnow == '1')
  
  AQI_diff_good <- AQI_unlisted_good[,2]- 
    AQI_unlisted_good[,1]
  
  AQI_diff_good_df <- as.data.frame(table(AQI_diff_good))
  
  # Wranlge data frame into format suitable for plotting 
  AQI_diff_good_df <- AQI_diff_good_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 0)) %>%
    dplyr::rename(deviations = "AQI_diff_good",
                  count = "Freq",
                  percentage = "percent")
  
  # Create dummy variable to be used in plot header 
  AQI_diff_good_df$tempvar <- "AirNow = Good"
  
  # Calculate the number of observations where AirNow reports good
  obs <- sum(AQI_diff_good_df$count)
  
  # Create plot of AQI deviations 
  good <- ggplot(AQI_diff_good_df, 
                 aes(x=deviations, y=percentage, fill=deviations)) +
    geom_bar(stat="identity", color = "black") +
    coord_cartesian(ylim=c(0,100))+
    scale_fill_manual(values=c("0" = "#00E400",
                               "1" = "#FFFF00",
                               "2" = "#FF7E00",
                               "3" = "#FF0000",
                               "4" = "#8F3F97",
                               "5" = "#7E0023"))+
    scale_x_discrete(limits = factor(c(-5:5)))+
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(size=15))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, size = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, size = 1) +
    geom_text(aes(label=percentage), vjust = -0.3, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 5) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#00E400"),
          strip.text = element_text(size=15, colour="black"))
  
  # ----- AirNow = Moderate ----------------------------------------------------
  
  # Calculate AQI deviations when AirNow reports moderate
  AQI_unlisted_moderate <- AQI_unlisted %>%
    dplyr::filter(airnow == '2')
  
  AQI_diff_moderate <- AQI_unlisted_moderate[,2]- 
    AQI_unlisted_moderate[,1]
  
  AQI_diff_moderate_df <- as.data.frame(table(AQI_diff_moderate))
  
  # Wranlge data frame into format suitable for plotting 
  AQI_diff_moderate_df <- AQI_diff_moderate_df %>%
    mutate(percent = (Freq/sum(Freq))*100) %>%
    mutate(percent = round(percent, 0)) %>%
    dplyr::rename(deviations = "AQI_diff_moderate",
                  count = "Freq",
                  percentage = "percent")
  
  # Create dummy variable to be used in plot header 
  AQI_diff_moderate_df$tempvar <- "AirNow = Moderate"
  
  # Calculate the number of observations where AirNow reports moderate
  obs <- sum(AQI_diff_moderate_df$count)
  
  # Create plot of AQI deviations 
  moderate <- ggplot(AQI_diff_moderate_df, 
                     aes(x=deviations, y=percentage, fill=deviations)) +
    geom_bar(stat="identity", color = "black") +
    coord_cartesian(ylim=c(0,100))+
    scale_fill_manual(values=c("-1" = "#00E400",
                               "0" = "#FFFF00",
                               "1" = "#FF7E00",
                               "2" = "#FF0000",
                               "3" = "#8F3F97",
                               "4" = "#7E0023"))+
    scale_x_discrete(limits = factor(c(-5:5)))+
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(size=15))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, size = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, size = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, size = 1) +
    geom_text(aes(label=percentage), vjust = -0.3, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 5) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#FFFF00"),
          strip.text = element_text(size=15, colour="black"))
  
  # ----- arrange all 2 plots into a grid --------------------------------------
  plot <- ggarrange(good, moderate, 
                    ncol = 2, nrow = 1, common.legend = FALSE)
  
  return(plot)
  
}