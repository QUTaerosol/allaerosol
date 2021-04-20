# library(devtools)
# install_github("marchtaylor/sinkr")

## VARIOUS USEFUL SCRIPTS FOR DATA PROCESSING

#Generic function for outputting a png image from a ggplot object using the Magick package
Generate.Plot <- function(pl_obj, pl_width=960, pl_height=340, pl_res=72, pl_name="Plot.png") {
  require("magick")
  fig <- image_graph(width=pl_width,height=pl_height,res=pl_res)
  print(pl_obj)
  dev.off()
  image_write(fig,path=pl_name, format="png")
  graphics.off()
  rm(pl_obj,pl_width,pl_height,pl_name)
}

#Save a dataframe as an RDS file, using the dataframe name and the current date
Backup.Data <- function(dframe,targetdir = as.character(NULL)) {
  saveRDS(dframe,paste0(targetdir,deparse(substitute(dframe)),"_",as.Date(Sys.time(), tz="Australia/Brisbane"),".rds"))
}

## Calculate a central rolling average, for a continuous dataset ##
Rolling.Average <- function(dataset,cols_to_avg,avg_window,avg_col_name=NA){
  #COLS_TO_AVG: If multiple column indices are provided, these columns will be combined together into a single averaged column
  #AVG_WINDOW: For each record, averaging will be performed over avg_window/2 rows before and after the current record.
  #AVG_COL_NAME: User-specified column name for the averaged data. By default, this is based on the name of the first column of data to be averaged
  library(dplyr)
  
  start <- 1:nrow(dataset) - ((avg_window-1) %/% 2 + (avg_window-1) %% 2)
  end <- 1:nrow(dataset) + (avg_window-1) %/% 2
  
  start[start < 1] <- 1
  end[end > nrow(dataset)] <- nrow(dataset)
  
  dataset$newcol <- NA
  for (i in 1:nrow(dataset)){
    dataset$newcol[i] <- mean(unlist(dataset[start[i]:end[i], cols_to_avg]), na.rm = T)
  }
  
  if(is.na(avg_col_name)) {
    names(dataset)[ncol(dataset)] <- paste0(names(dataset)[cols_to_avg[1]],"_rollav")
  } else {
    names(dataset)[ncol(dataset)] <- avg_col_name
  }
  
  return(dataset)
}

#Geometric mean calculator
Gm.Mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

#Generate a vector of tick positions for log scale axes
logticks <- function(datavals,type) {
  
  minimum <- 1/10^abs(floor(log10(min(datavals, na.rm=TRUE))))
  maximum <- 1*10^abs(floor(log10(max(datavals, na.rm=TRUE)))+1)
  multiple <- floor(log10(maximum/minimum))
  yourtickvector <- c()
  
  if (type=="breaks") {
    yourtickvector <- c(minimum)
    for (x in seq(0,multiple)) {
      andadd <- seq(minimum*10^x,minimum*10^(x+1),minimum*10^x)[-1]
      yourtickvector <- c(yourtickvector,andadd)
    }
  } else if (type=="labels") {
    for (x in seq(0,multiple)) {
      andadd <- c(minimum*10^x,2*minimum*10^x,rep("",2),5*minimum*10^x,rep("",4))
      yourtickvector <- c(yourtickvector,andadd)
    }
    yourtickvector <- c(yourtickvector,minimum*10^multiple)
  }
  return(yourtickvector)
}

#Luke's time averaging function
TimeAv <- function(data, dates, avg.time, vector = F, tzone = "UTC"){
  library(circular)
  if (vector == T){
    dates <- dates[which(!is.na(data))]
    data <- data[which(!is.na(data))]
    data <- circular(data, units = "degrees")
  }
  hrs <- cut(dates, breaks = avg.time)
  avg_data <- aggregate(data, list(hrs), FUN = mean, na.rm = T)
  sd_data <- aggregate(data, list(hrs), FUN = sd, na.rm = T)
  
  names(avg_data) <- names(sd_data) <- c("date", names(avg_data)[which(names(avg_data) != "Group.1")])
  ##Dates need to be put into R format average aggregation
  avg_data$date <- as.POSIXct(avg_data$date, tz = tzone)
  sd_data$date <- as.POSIXct(sd_data$date, tz = tzone)
  ##Create output data frames of same length as avg.time
  avg <- data.frame(date = avg.time, avg_data[rep(1, length(avg.time)), which(names(avg_data) != "date")])
  sd <- data.frame(date = avg.time, sd_data[rep(1, length(avg.time)), which(names(sd_data) != "date")])
  ##Set values to NA
  avg[, which(names(avg_data) != "date")] <- NA
  sd[, which(names(sd_data) != "date")] <- NA
  ##Fill out data frames
  avg[match(avg_data$date, avg$date), which(names(avg_data) != "date")] <- avg_data[, which(names(avg_data) != "date")]
  sd[match(sd_data$date, sd$date), which(names(avg_data) != "date")] <- sd_data[, which(names(sd_data) != "date")]
  ##Rename out data frames
  names(avg) <- c("date", names(avg_data)[which(names(avg_data) != "date")])
  names(sd) <- c("date", names(sd_data)[which(names(sd_data) != "date")])
  ##Return data
  return(list(avg, sd))
}

## Functions for dealing with date import problems between R and Excel
date.XlsToR = function (raw_data, col_for_conversion) {
  # DATE.XLSTOR takes dates in Excel's numeric date format and converts them to R's POSIXct format
  # RAW_DATA contains a data frame which includes the dates for conversion
  # COL_FOR_CONVERSION contains the index of the column containing dates
  #
  # REQUIRES: library("openxlsx")
  # NOTE: After conversion, all date/times reflect original data but the timezone is set to Brisbane time
  # Converts imported dates. DAYS is # days since 1/1/1900. TIMES is fraction of a day since 0:00AM. DATES_R is the date in R format,
  # DATES_POS is the date/times in POSIXct format.
  working = data.frame(days=floor(raw_data[,col_for_conversion]))
  working$times = raw_data[,col_for_conversion]-working$days
  working$dates_R = as.Date(working$days-1, origin="1899-12-31", tz="GMT")
  # working$dates_POS = .POSIXct(round((unclass(working$dates_R)+working$times)*86400-36000))
  working$dates_POS = .POSIXct(round((unclass(working$dates_R)+working$times)*86400), tz="GMT")
  # working$dates_POS = .POSIXct(round((unclass(working$dates_R)+working$times)*86400-36000), tz="Australia/Brisbane")
  # working$dates_POS_zoned <- as.POSIXct(as.numeric(working$dates_POS), origin="1970-01-01", tz="GMT")
  
  return(working$dates_POS)
  # return(working$dates_POS_zoned)
}

## OLD UNNECESSARY JUNK CODE

# merge.xls.datetimes = function (raw_data) {
#   #MERGE.XLS.DATETIMES merges separate date entries and time entries, formatted by Excel
#   # Assumes all dates were originally in mm/dd/yy HH:MM:SS format, but that Excel has assumed dd/mm/yy HH:MM:SS format
#   # NOTE: After cleanup, all date/times reflect original data but the timezone is set to Brisbane time
# 
#   # For days > 12, Excel will not have recognised them as valid date/time entries and will have left the year in 2 digit format
#   # For days <= 12, Excel will have used 4 digit year format
# 
#   # RAW_DATA contains a two column data frame. The first column contains the dates, the second contains the times
#   # REQUIRES: library("splitstackshape")
# 
#   # Initialise data frame for cleaned dates & set all non-numeric dates to NA
#   merged = as.data.frame(cSplit(raw_data, 1, sep = "/"))
#   short_yrs = which(nchar(merged[,4]) == 2)
#   merged[short_yrs,4] = paste("20", merged[short_yrs,4], sep = "")
#   merged$Date = paste(merged[,4], merged[,2], merged[,3], sep = "/")
#   merged$DateTime = paste(merged$Date, merged$Start.Time, sep = " ")
#   merged$DateTime = as.POSIXct(merged$DateTime, origin = "1970-01-01", tz = "Australia/Brisbane")
# 
#   return(merged$DateTime)
# }

# clean.dates = function (raw_data, col_for_cleanup) {
#   # CLEAN.DATES corrects faulty date/time data which has been incorrectly converted by Excel
#   # Assumes all dates were originally in mm/dd/yy HH:MM:SS format, but that Excel has assumed dd/mm/yy HH:MM:SS format
#   # NOTE: After cleanup, all date/times reflect original data but the timezone is set to Brisbane time
# 
#   # For days =< 12, the month & day field will be incorrectly assigned, and the data will be in Excel's numeric date format
#   # These are converted to R numeric format (POSIXlt), month & day fields are swapped and then converted to POSIXct format
#   # For days > 12, Excel does not recognise them as valid date/time entries and will export them in character format
#   # These are converted directly to POSIXct format with the correct day & month
# 
#   # RAW_DATA contains a data frame which includes the faulty data
#   # COL_FOR_CLEANUP contains the index of the faulty column
#   # REQUIRES: library("openxlsx")
# 
# 
#   # Initialise data frame for cleaned dates & set all non-numeric dates to NA
#   # raw_data = read.table("h_run_dates.csv", sep = ",", header = TRUE, stringsAsFactors=FALSE)
#   cleaned = data.frame(as.numeric(raw_data[,col_for_cleanup]))
# 
#   # Lists of indices for dates which were/weren't successfully transferred
#   date_ind = which(!is.na(cleaned))
#   na_ind = which(is.na(cleaned))
# 
#   # Clean imported numeric dates. Days is # days since 1/1/1900. Times is fraction of a day since 0:00AM. dates_R is the date in R format,
#   # based on the incorrect assignment of day and month. posixct is the incorrect date/times in posixct format.
#   working = data.frame(days=floor(cleaned[date_ind,1]))
#   working$times = cleaned[date_ind,1]-working$days
#   working$dates_R = as.Date(working$days-1, origin="1899-12-31")
#   working$posixct = .POSIXct(round((unclass(working$dates_R)+working$times)*86400), tz="Australia/Brisbane")
#   working$posixlt = as.POSIXlt(working$posixct)
#   working$day_new = working$posixlt$mon+1
#   working$month_new = working$posixlt$mday
#   working$year_new = working$posixlt$year+1900
#   working$new_date_str = paste(as.character(working$year_new), "/", as.character(working$month_new), "/", as.character(working$day_new), " 00:00:00", sep="")
#   working$new_date_num = as.numeric(strptime(working$new_date_str, format="%Y/%m/%d %H:%M:%S", tz="Australia/Brisbane"))
#   working$date_fixed = as.POSIXct(working$new_date_num+round(86400*working$times), tz="Australia/Brisbane", origin = "1970-01-01")
#   cleaned[date_ind,1] = working$date_fixed
# 
#   # Replace NAs with formatted dates
#   cleaned[na_ind,1] = as.POSIXct(strptime(raw_data[na_ind,col_for_cleanup], "%m/%d/%y %H:%M:%S", tz="Australia/Brisbane"))
#   return(cleaned)
# }


# plot_GFD_ts = function (dates, growth_factors, GFDs, limits_TS, num_colours) {
#   # PLOT_GFD_TS generates a 2D intensity plot timeseries of the growth factor distributions (GFD) resulting from TDMA data inversion
#   # DATES is a POSIct (or numeric?) vector of scan date/times
#   # GROWTH_FACTORS is a numeric vector of the growth factors corresponding to each GFD column
#   # GFDs is a numeric matrix of GFDs by row
#   # LIMITS_TS can be either NA (to limit the x range to the provided DATES), or a two-element vector providing the start and end date
#   # NUM_COLOURS is the number of colour bins that should be used
# 
#   # library("colorRamps") Out of date
#   #   library("fields")
#   jet.colours <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
#                                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
#   brownish <- colorRampPalette(c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404"))
#   reddish <- colorRampPalette(c("#fef0d9", "#fdcc8a", "#fc8d59", "#d7301f"), bias=1.4)
#   bluish <- colorRampPalette(c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494"))
# 
#   # Generate colour bins for the full range of possible counts
#   min_cts = GFDs[which(GFDs==min(GFDs, na.rm = TRUE), arr.ind=T)]
#   max_cts = GFDs[which(GFDs==max(GFDs, na.rm = TRUE), arr.ind=T)]
# 
#   # ... using exponential scale to allow max differentiation at low end of the scale
#   bins = exp(seq(log(1),log(1+max_cts[1]),log(1+max_cts[1])/num_colours))-1
# 
#   # bins = log(seq(exp(min_cts[1]),exp(max_cts[1]),exp(max_cts[1])/num_colours))-1
# 
#   # ... using a second order polynomial scale to give moderate differentiation at the low end of the scale
#     # bins = sqrt(seq((min_cts[1])^2,(max_cts[1])^2,((max_cts[1])^2)/num_colours))
#     # bins = (seq(sqrt(min_cts[1]),sqrt(max_cts[1]),sqrt(max_cts[1])/num_colours))^2
# 
#   # ... using a linear scale where no weighting is required towards either end of the scale
#   # bins = seq(min_cts[1],max_cts[1],(max_cts[1]-min_cts[1])/num_colours)
# 
#   missing = (num_colours+1)-length(bins)
#   if (missing > 0) {
#     for (i in 1:missing) {
#       bins = c(min_cts[1]/(i+1), bins)
#     }
#   }
# 
#   # Generate intensity plot
#   if (is.na(limits_TS[1])) {
#     limits_TS <- c(min(dates), max(dates))
#   }
#      #     image.plot(dates, growth_factors, GFDs, col = matlab.like(100), breaks=bins, xlim = limits_TS, xlab = "", ylab = "", xaxt = "n")
#   # image(x=dates, y=growth_factors, z=GFDs, col = bluish(num_colours), breaks=bins, xlim = limits_TS,
#   #       xlab = "", ylab = "", xaxt = "n")
# 
#   # image(dates, growth_factors, GFDs, col = matlab.like(100), breaks=bins, axes=F)
#   #   image(dates, growth_factors, GFDs, col = matlab.like(100), breaks=bins, xlab = "", ylab = "")
#   # imageScale(z=GFDs, zlim=c(min(GFDs), max(GFDs)), col = bluish(num_colours),
#   #                         breaks=bins, axis.pos = 4)
#   return(list(col = bluish(num_colours), breaks = bins)) #returning parameters for use in plotting the scale
# }

# squared_trans <- function() {
#   require(scales)
#   trans_new("squared", transform = function(x) x^2, inverse = function(x) x^(1/2))
# }
# 
# cubed_trans <- function() {
#   require(scales)
#   trans_new("cubed", transform = function(x) x^3, inverse = function(x) x^(1/3))
# }
#
# last <- function(x) {
#   # LAST is a quick way to use the tail function to return the returns the final element of a vector, matrix, table, data frame or function.
#   tail(x, n = 1)
# }

