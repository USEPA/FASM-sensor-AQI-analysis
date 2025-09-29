# functions to go with the R script 'template_for_sensor_vs_monitor.R'

# use the source() function to load these functions into the environment when 
# working through the template script

# =============================
# Function: findAdjacentPairs
# =============================
#' findAdjacentPairs
#'
#' This function identifies sensor locations that are within a specified radius of AirNow monitoring stations. 
#' It calculates the distance between each sensor and its nearest AirNow station, then filters the sensors that 
#' have a nearby AirNow station within the given radius. The function returns a data frame of sensors 
#' that are within the specified radius of AirNow stations, sorted by their distance to their nearest AirNow station.
#'
#' @param sensor A list containing sensor data and metadata, including `longitude` and `latitude`.
#' @param airnow A list containing AirNow station data and metadata, including locations.
#' @param radius A numeric value specifying the radius (in meters) within which sensors should be considered 
#' adjacent to AirNow stations. Default is 500 meters.
#'
#' @return A filtered and sorted data frame of sensors within the specified radius of AirNow stations.
#'
#' @examples
#' # Example usage:
#' sensors_near_airnow <- findAdjacentPairs(sensor_data, airnow_data, radius = 500)
#'
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


# =============================
# Function: create_AQI_unlisted
# =============================
#' create_AQI_unlisted
#'
#' This function calculates Air Quality Index (AQI) categories for each pair of sensor and AirNow monitor
#' that are adjacent. It combines data from both sources, computes AQI categories based on PM2.5 levels, 
#' and returns a data frame with AQI values for AirNow and sensor data, unlisted and combined.
#'
#' @param sensor_adjacent A list containing metadata and data for sensors that are adjacent to AirNow monitors.
#' @param airnow_adjacent A list containing metadata and data for AirNow monitors that are adjacent to sensors.
#'
#' @return A data frame containing unlisted AQI values for both AirNow monitors and sensors.
#'
#' @examples
#' # Example usage:
#' AQI_data <- create_AQI_unlisted(sensor_adjacent_data, airnow_adjacent_data)
#'
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

# =============================
# Function: create_combined_barplot
# =============================
#' create_combined_barplot
#'
#' This function creates a bar plot to visualize the differences in AQI categories between sensor readings and AirNow monitor readings.
#' It calculates the percentage of sensor NowCast data points that deviate from AirNow readings across different AQI categories
#' and plots these differences. Threshold lines can be added to the plot for reference.
#'
#' @param AQI_unlisted A data frame containing unlisted AQI values for both AirNow monitors and sensors.
#'                      The first column should represent AirNow AQI values and the second column should represent sensor AQI values.
#' @param threshold_1 A numeric value specifying the position of the first threshold line in the plot.
#' @param threshold_2 A numeric value specifying the position of the second threshold line in the plot.
#'
#' @return A ggplot object representing the bar plot of AQI category deviations.
#'
#' @examples
#' # Example usage:
#' plot <- create_combined_barplot(AQI_unlisted_data, threshold_1 = 5, threshold_2 = 1)
#' print(plot)
#'
create_combined_barplot <- function(
    AQI_unlisted,
    threshold_1,
    threshold_2
) {
  
  # Calculate the deviation between the sensor and monitor AQI category
  AQI_diff <- AQI_unlisted[, 2] - AQI_unlisted[, 1]  # sensor minus monitor
  
  # Convert differences to a factor for plotting
  AQI_diff <- factor(AQI_diff, levels = -5:5)
  
  # Create a data frame of deviations and calculate percentages
  AQI_diff_df <- as.data.frame(table(AQI_diff)) %>%
    dplyr::mutate(
      percentage = round((Freq / sum(Freq)) * 100, 2)
    ) %>%
    dplyr::rename(
      deviations = AQI_diff,
      count = Freq
    )
  
  # Create bar plot of deviations for all AQI categories combined
  plot <- ggplot(AQI_diff_df, aes(x = deviations, y = percentage)) +
    geom_bar(stat = "identity", color = "black", fill = "gray") +
    scale_x_discrete(limits = factor(-5:5)) +
    geom_text(aes(label = percentage), vjust = -0.6, color = "black", size = 5) +
    geom_segment(aes(x = 4.5, y = threshold_1, xend = 5.5, yend = threshold_1), lty = 2, linewidth = 1) +
    geom_segment(aes(x = 6.5, y = threshold_1, xend = 7.5, yend = threshold_1), lty = 2, linewidth = 1) +
    geom_segment(aes(x = 0.5, y = threshold_2, xend = 4.5, yend = threshold_2), lty = 2, linewidth = 1) +
    geom_segment(aes(x = 7.5, y = threshold_2, xend = 11.5, yend = threshold_2), lty = 2, linewidth = 1) +
    labs(
      y = "Percentage of Sensor NowCast Datapoints",
      x = "AQI Category Difference (Sensor - AirNow)"
    ) +
    theme_minimal(base_size = 16) +
    ylim(0, 100)
  
  return(plot)
}


# =============================
# Function: create_plots_by_category 
# =============================
#' create_plots_by_category
#'
#' This function creates a bar plot to visualize the differences in AQI categories between sensor readings and AirNow monitor readings.
#' It calculates the percentage of sensor NowCast data points that deviate from AirNow readings across different AQI categories
#' and plots these differences. Threshold lines can be added to the plot for reference.
#'
#' @param AQI_unlisted A data frame containing unlisted AQI values for both AirNow monitors and sensors.
#'                      The first column should represent AirNow AQI values and the second column should represent sensor AQI values.
#' @param threshold_1 A numeric value specifying the position of the first threshold line in the plot.
#' @param threshold_2 A numeric value specifying the position of the second threshold line in the plot.
#'
#' @return A ggplot object representing the bar plot of AQI category deviations.
#'
#' @examples
#' # Example usage:
#' plot <- create_combined_barplot(AQI_unlisted_data, threshold_1 = 5, threshold_2 = 1)
#' print(plot)
#'
create_plots_by_category <- function(
    AQI_unlisted,
    num_plots,
    threshold_1,
    threshold_2
) {
  
  # ----- AirNow = Good --------------------------------------------------------
  
  if (num_plots >= 1) {
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
              size = 4) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#00E400"),
          strip.text = element_text(size=15, colour="black"))
  }
  
  # ----- AirNow = Moderate ----------------------------------------------------
  
  if (num_plots >= 2) {
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
              size = 4) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#FFFF00"),
          strip.text = element_text(size=15, colour="black"))
  }
  
  # ----- AirNow = USG ---------------------------------------------------------
  
  if (num_plots >= 3) {
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
              size = 4) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#FF7E00"),
          strip.text = element_text(size=15, colour="black"))
  }
  
  # ----- AirNow = Unhealthy ---------------------------------------------------
  
  if (num_plots >= 4) {
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
              size = 4) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#FF0000"),
          strip.text = element_text(size=15, colour="black"))
  }
  
  # ----- AirNow = Very Unhealthy ----------------------------------------------
  
  if (num_plots >= 5) {
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
    geom_text(aes(label=percentage), vjust = -0.3, color="black", size = 5) +
    geom_text(x = 9, y = 90, 
              label = sprintf("Obs = %s", obs),
              size = 4) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#8F3F97"),
          strip.text = element_text(size=15, colour="white"))
  }
  
  # ----- AirNow = Hazardous ---------------------------------------------------
  if (num_plots >= 6) {
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
              size = 4) +
    theme(legend.position = "none") +
    facet_grid(. ~ tempvar) +
    theme(strip.background = element_rect(fill="#7E0023"),
          strip.text = element_text(size=15, colour="white"))
  }
  
  # ----- arrange plots into a grid --------------------------------------------
  
  if (num_plots == 1) {
    plot <- ggarrange (good,
                       ncol = 1, nrow = 1)
  }
  
  if (num_plots == 2) {
    plot <- ggarrange (good, moderate,
                       ncol = 2, nrow = 1, common.legend = FALSE)
  }
  
  if (num_plots == 3) {
    plot <- ggarrange (good, moderate, usg,
                       ncol = 2, nrow = 2, common.legend = FALSE)
  }
  
  if (num_plots == 4) {
    plot <- ggarrange (good, moderate, usg, unhealthy,
                       ncol = 2, nrow = 2, common.legend = FALSE)
  }
  
  if (num_plots == 5) {
    plot <- ggarrange (good, moderate, usg, unhealthy, veryunhealthy,
                       ncol = 3, nrow = 2, common.legend = FALSE)
  }
  
  if (num_plots == 6) {
    plot <- ggarrange (good, moderate, usg, unhealthy, veryunhealthy, hazardous,
                       ncol = 3, nrow = 2, common.legend = FALSE)
  }
  
  annotate_figure(plot,
                  bottom = text_grob("AQI Category Difference (Sensor - AirNow)",
                                     hjust = 0.5,
                                     size = 15),
                  left = text_grob("Percentage of Sensor NowCast Datapoints",
                                   rot = 90,
                                   size = 15))
  
}


