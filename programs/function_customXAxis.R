#---------------------------------------
#This is a custom x-axis script
#We can center x labels between ticks, skip some labels, add minor ticks, etc...
#--------------------------------------

customXAxis <- function(myplot, center = TRUE, skip = 0, labelstart = 1, minorticks = NULL,
                        shift = FALSE, removelast = FALSE, lastadjust = 0, rmfirsttick = FALSE,
                        rmlasttick = FALSE, return_ggplot = FALSE) {
  if(is.null(myplot)) {
    stop("Function you are trying to call is missing the ggplot or grob plot object")
  }
  #First check if plot is ggplot or grob; if not grob, convert
  grobcheck1 <- gregexpr('gtable', toString(myplot))
  grobcheck2 <- gregexpr('gTree', toString(myplot))
  
  if(grobcheck1[[1]][1] == -1 & grobcheck2[[1]][1] == -1) {
    myplot <- ggplotGrob(myplot)
  }
  #Find x-axis
  xaxis <- myplot$grob[[which(myplot$layout$name == "axis-b")]]
  
  #Get the grob containing tick marks and labels
  check <- gregexpr('polyline', toString(xaxis$children[[1]]))
  if(check[[1]][1] == 1) {rn <- 2} else {rn <- 1}
  ticks <- xaxis$children[[rn]]
  if (class(ticks)[[1]] == "zeroGrob"){
    ticks <- xaxis$children[[ c(1,2) [xaxis$childrenOrder == 'axis'] ]]
  }
  #Get the tick marks & labels
  check <- gregexpr('polyline', toString(ticks$grobs[[1]]))
  if(check[[1]][1]==1) {
    marks = ticks$grobs[[1]] #polyline
    labels = ticks$grobs[[2]] #titleGrob
  } else {
    marks = ticks$grobs[[2]]
    labels = ticks$grobs[[1]]
  }
  
  #Center labels between ticks if center = TRUE
  
  if(center == TRUE && length(labels$children[[1]]$x) != 1 && shift == FALSE) { #Do not attempt to move labels if only 1 tick is drawn.
    # Remove last label if centering pushes it beyond right axis (beyond 1native)
    for(i in 1:(length(marks$x)/2)*2) {
      if(i < length(marks$x)) {
        movelabel <- (as.numeric(marks$x[i+1]) - as.numeric(marks$x[i]))/2
      } else {
        movelabel <- (as.numeric(marks$x[i]) - as.numeric(marks$x[i-2])) / 2
      }
      if((as.numeric(marks$x[i]) + movelabel) > 1){
        # If the last adjust option is not set, the last label will be removed (when it would otherwise appear past the right axis)
        # else the lastadjust option will horizontally adjust the last label in the units of "native"
        if(lastadjust == 0) { labels$children[[1]]$label[i/2] <- ""}
        else{
          labels$children[[1]]$x[i/2] <- labels$children[[1]]$x[i/2] + unit(movelabel, "native") + unit(lastadjust, "native")
        }
      }
      else{
      labels$children[[1]]$x[i/2] <- labels$children[[1]]$x[i/2] + unit(movelabel, "native")
      }
    }
  }
  #Skip labels or adjust label start
  if(skip!= 0 || labelstart != 1){
    skipindex <- seq(from = labelstart, to = length(labels$children[[1]]$label), by = (skip + 1))
    keep <- labels$children[[1]]$label[skipindex]
    labels$children[[1]]$label[1:length(labels$children[[1]]$label)] <- ""
    labels$children[[1]]$label[skipindex] <- keep
  }
  #Remove last label if desired...

  if(removelast == TRUE){
    labels$children[[1]]$label[length(labels$children[[1]]$label)] <- ""
  }
  #For bars wtihout dates on the x-axis, shift ticks around teh bars. Doesn't work for date bars.

  if(shift == TRUE) {
    movetick <- (as.numeric(marks$x[3]) - as.numeric(marks$x[2]))/2
    firsttick <- marks$x[1]
    marks$x <- marks$x - unit(movetick, 'native')
    if(as.numeric(firsttick) < movetick) {
      marks$x[1:2] <- unit(0, "native")
    }
    marks$x[(length(marks$x) + 1): (length(marks$x)+2)] <- marks$x[length(marks$x)] + 2*unit(movetick, "native")
    marks$y[1:length(marks$x)] <- marks$y[1:2]
    marks$id.lengths[1:(length(marks$x)/2)] <- marks$id.lengths[1]
  }
  
  #Replace xaxis values
  ticks$grobs[[1]] = marks
  ticks$grobs[[2]] = labels
  xaxis$children[[2]] = ticks
  myplot$grobs[[which(myplot$layout$name == "axis-b")]] = xaxis
  
  
  #Add in minor ticks
  if(!is.null(minorticks)){
    steps = minorticks
    
    #Set custom ticks
    #And retain original ticks.
    o.length <- length(marks$x)
    f.length <- o.length + o.length*(steps)
    
    x.inc.numeric <- (as.numeric(marks$x[3]) - as.numeric(marks$x[2])) * (1/(steps+1))
    
    
    ## Create a vecotr of distance sbetween each major tick pair
    for(i in 1:(o.length/2)){
      if(((i*2)+1) <= o.length) {
        x.inc.numeric[] <- (as.numeric(marks$x[(i*2)+1]) - as.numeric(marks$x[(i*2)-1]))*(1/(steps+1))
      }
    }
    
    x.inc <- unit(x.inc.numeric, "native")
    
    for(z in 1:steps) {
      s.index <- length(marks$x) + 1
      f.index <- (s.index-1)+o.length
      tickmax <- as.numeric(marks$x[o.length]) + as.numeric(x.inc[length(x.inc)]) * z
      if(tickmax > 1.0) {
        marks$x[s.index:f.index] <- unit.c(marks$x[1:o.length] + z*x.inc)
        marks$x[f.index] = marks$x[1]
        marks$x[f.index-1] = marks$x[1] #unit 0 native...?
      } else {
        marks$x[s.index:f.index] <- unit.c(marks$x[1:o.length] + z*x.inc)
      }
      z <- z + 1
    }
    
    #Find ticklength and draw in minor ticks at that length
    findlen <- strsplit(toString(marks$y[1]), ",")[[1]][2]
    findlen <- as.numeric(strsplit(findlen, "cm")[[1]][1])
    
    #Lengthen the major ticks:
    o.ylength <- length(marks$y)
    majorticks <- seq(1,length(marks$y), 2)
    
    rep.y <- marks$y
    rep.y[1:(o.length/2)*2-1] <- unit.c(marks$y[1:(o.length/2)*2-1])
    marks$y[(o.length+1):f.length] = rep(rep.y, steps)
    
    #check if minor ticks need to be added prior to first ticks
    addminor <- as.numeric(marks$x[1]) / x.inc.numeric[1]
    if(addminor > 1) {
      for(z in 1:floor(addminor)) {
        marks$x[(length(marks$x) + 1): (length(marks$x)+2)] <- unit.c(marks$x[1] - z*x.inc)
        marks$y[length(marks$y)+1] <- unit.c(marks$y[1])
        marks$y[length(marks$y)+1] <- marks$y[2]
      }
    }
    
    marks$id.lengths[1:(length(marks$id.lengths)+length(marks$id.lengths)*steps+floor(addminor))] = 2
    
    marks$y[majorticks] <- marks$y[majorticks] + unit(findlen/1.5, "cm")
  }
  
  #Removing first/last tick marks if requested....
  if(rmfirsttick + rmlasttick > 0){
    firsttick = list(c(), 10)
    lasttick = list(c(), -10)
    
    #Finding indices corresponding to first and last ticks and sotring them in first/lasttick
    for (i in 1:length(marks$x)) {
      if(typeof(marks$x[i][[1]]) == 'double'){
        if (!(as.numeric(marks$x[i][[1]]) %in% c(0,1))) {
          if(as.numeric(marks$x[i][[1]]) < firsttick[2]){
            firsttick = list(c(i), as.numeric(marks$x[i][[1]]))
          } else if (as.numeric(marks$x[i][[1]]) == firstick[2]) {
            firsttick[[1]] = append(firsttick[[1]], i)
          }
          
          if (as.numeric(marks$x[i][[1]]) > lasttick[2]) {
            lasttick = list(c(i), as.numeric(marks$x[i][[1]]))
          } else if (as.numeric(marks$x[i][[1]]) == firsttick[2]) {
            lasttick[[1]] = append(firsttick[[1]], i)
          }
        }
      } else if (typeof(marks$x[i][[1]]) == 'list') {
        tmpX = as.numeric(marks$x[i][[1]][[2]]) + (as.numeric(marks$x[i][[1]][[3]][[2]]) * as.numeric(marks$x[i][[1]][[3]][[3]]))
        if(!(tmpX %in% c(0,1))) { #don't want/need to purge vertical axes
          if (tmpX < firsttick[2]){
            firsttick = list(c(i), tmpX)
          } else if (tmpX == firsttick[2]) {
            firsttick[[1]] = append(firsttick[[1]], i)
          }
          if (tmpX > lasttick[2]){
            lasttick = list(c(i), tmpX)
          } else if (tmpX == lasttick[2]) {
            lasttick[[1]] = append(lasttick[[1]], i)
          }
        }
      }
    }
  #Removing verticality from bad ticks
  if(prod(c(rmfirsttick, rmlasttick) == c(T,T)) == 1) {
    badticks = append(firsttick[[1]], lasttick[[1]])
    } else if (prod(c(rmfirsttick, rmlasttick) == c(F,T)) == 1){
      badticks = lasttick[[1]]
      } else {
        badticks = firsttick[[1]]
      }
    marks$y[badticks] = unit(0,"native")
  }
  # Update plot object with ticks and labels
  ticks$grobs[[1]] = marks
  ticks$grobs[[2]] = labels
  xaxis$children[[2]] = ticks
  myplot$grobs[[which(myplot$layout$name == "axis-b")]] = xaxis
  
  if(return_ggplot == TRUE){
    myplot = ggpubr::as_ggplot(myplot)
  }
  return(myplot)
}














