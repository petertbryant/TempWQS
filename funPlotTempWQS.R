plotTempWQS <- function(new_data, df.all) {
  #new_data <- sdadm_one_station
  new_data$Sampled <- as.POSIXct(strptime(new_data$date, format = '%Y-%m-%d'))  
  x.min <- min(new_data$Sampled) #min of subset date
  x.max <- max(new_data$Sampled) #max of subset date
  x.lim <- c(x.min, x.max) ####define the data domain for graph
  y.min <- if(floor(min(new_data$sdadm, na.rm = TRUE))<=0 ){ #min of data for graph& log.scale=="y"
    1 #set minimum y value for log scale to one
  }else{
    10
    #floor(min(new_data$sdadm, na.rm = TRUE))
  }
  y.max <- ceiling(max(new_data$sdadm, na.rm = TRUE)) #max of data for graph
  y.lim <- c(y.min,y.max) ####define the data range
  title <- paste0(unique(df.all$Station_Description)[unique(df.all$Station_ID) == new_data[1,"id"]], 
                  ", ID = ", 
                  new_data[1,"id"]) #, " , river mile = ",river.mile
  x.lab <- "month"
  y.lab <- "Temperature (7DADM)"
  ####plot the timeseries
  #file.name.ts <- paste0(station,"_timeseries",parameter.graph.name,".png")
  #png(filename=file.name.ts ,width = 700, height = 400) ####create a .png with the station name in the filepath specified above
  par(xpd=NA,oma=c(0,0,4,0), mar=c(5.1,4.1,3.1,2.1)) 
  plot(new_data$Sampled, new_data$sdadm, xlim=x.lim, ylim=y.lim, xlab="", ylab=y.lab, bty="L") ####plot the points 
  title(main=title, cex.main=1.2, outer=TRUE)
  exceeds.points <- new_data[new_data$exceedsummer | new_data$exceedspawn,]
  points(exceeds.points$Sampled, exceeds.points$sdadm, col="red", pch=20) ####plot the exceedances
  ####Draw WQS
  spn_index <- which(new_data$criteria_value == 13)
  spn_diff <- diff(spn_index)
  
  if (all(spn_diff == 1)) {
    if (length(spn_index) > 0) {
      spn_1 <- max(spn_index)
      
      if (spn_1 == nrow(new_data)) {
        #Plot non-spawn time-period
        lines(x = c(new_data[1, 'Sampled'],
                    new_data[spn_index[1] - 1, 'Sampled']),
              y = c(unique(new_data[1:(spn_index[1] - 1),'criteria_value']),
                    unique(new_data[1:(spn_index[1] - 1),'criteria_value'])), lty = 3)
      } else {
        #Plot non-spawn time-period
        lines(x = c(new_data[spn_1 + 1, 'Sampled'],
                    new_data[nrow(new_data), 'Sampled']),
              y = c(unique(new_data[(spn_1 + 1):nrow(new_data),'criteria_value']),
                    unique(new_data[(spn_1 + 1):nrow(new_data),'criteria_value'])), lty = 3)
      }
      #Plot spawn time period
      lines(x = c(new_data[spn_index[1],'Sampled'],
                  new_data[spn_1,'Sampled']),
            y = c(unique(new_data[spn_index[1]:spn_1,'criteria_value']),
                  unique(new_data[spn_index[1]:spn_1,'criteria_value'])), lty=2)
    } else {
      lines(x = c(new_data[1,'Sampled'],
                  new_data[nrow(new_data),'Sampled']),
            y = c(unique(new_data[1:nrow(new_data),'criteria_value']),
                  unique(new_data[1:nrow(new_data),'criteria_value'])), lty = 3)
    }
    
  } else {
    spn_stop <- spn_index[which(spn_diff > 1)]
    spn_start <- spn_index[which(spn_diff > 1) + 1]
    nspn_start <- spn_stop + 1
    nspn_stop <- spn_start - 1
    
    
    for (i in 1:length(spn_start)) {
      if (i < length(spn_start)) {
        #Plot next spawn time period
        lines(x = c(new_data[spn_start[i],'Sampled'],
                    new_data[spn_stop[i + 1],'Sampled']),
              y = c(unique(new_data[spn_start[i]:spn_stop[i + 1],'criteria_value']),
                    unique(new_data[spn_start[i]:spn_stop[i + 1],'criteria_value'])), lty = 2)
        
        #Plot non-spawn time period
        lines(x = c(new_data[nspn_start[i],'Sampled'],
                    new_data[nspn_stop[i], 'Sampled']),
              y = c(unique(new_data[nspn_start[i]:nspn_stop[i],'criteria_value']),
                    unique(new_data[nspn_start[i]:nspn_stop[i],'criteria_value'])), lty = 3)
      } else {
        #Plot last spawn-time period
        lines(x = c(new_data[spn_start[i],'Sampled'],
                    new_data[max(spn_index),'Sampled']),
              y = c(unique(new_data[spn_start[i]:max(spn_index),'criteria_value']),
                    unique(new_data[spn_start[i]:max(spn_index),'criteria_value'])), lty = 2)
        
        #Plot non-spawn time period
        lines(x = c(new_data[nspn_start[i],'Sampled'],
                    new_data[nspn_stop[i], 'Sampled']),
              y = c(unique(new_data[nspn_start[i]:nspn_stop[i],'criteria_value']),
                    unique(new_data[nspn_start[i]:nspn_stop[i],'criteria_value'])), lty = 3)
        
        #Plot last non-spawn time period
        if (new_data[nrow(new_data),'criteria_value'] != 13) {
          lines(x = c(new_data[max(spn_index) + 1,'Sampled'],
                      new_data[nrow(new_data), 'Sampled']),
                y = c(unique(new_data[(max(spn_index) + 1):nrow(new_data),'criteria_value']),
                      unique(new_data[(max(spn_index) + 1):nrow(new_data),'criteria_value'])), lty = 3)
        }
      }
    }
    
    #Plot first non-spawn time period TODO: Add functionality to check if start of data is in spawning or non-spawning
    if (spn_index[1] != 1) {
      lines(x = c(new_data[1,'Sampled'],
                  new_data[spn_index[1] - 1, 'Sampled']),
            y = c(unique(new_data[1:(spn_index[1] - 1), 'criteria_value']),
                  unique(new_data[1:(spn_index[1] - 1), 'criteria_value'])), lty = 3)
    }
    
    #Plot first spawn time period
    lines(x = c(new_data[spn_index[1],'Sampled'],
                new_data[spn_stop[1],'Sampled']),
          y = c(unique(new_data[spn_index[1]:spn_stop[1],'criteria_value']),
                unique(new_data[spn_index[1]:spn_stop[1],'criteria_value'])), lty=2)
    
  }
  
  legend(x=par("usr")[1],y=par("usr")[3], legend=c("Spawning criterion", 
                                                   "Non-spawning criterion"), 
         lty=c(2,3), col=c("black","black"), lwd=c(1,1), 
         xjust=-0.01, yjust=-8., box.lty=0, cex=1.0, horiz=TRUE)
}
