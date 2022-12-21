# getev and Plot.ev are old functions from Gottfried Pestal. 
getev <- function(ev.list,ev.mat,avg.last.yr){
  mod.EV = ev.mat[,ev.list]
  if(length(ev.list)==1){tmp.ev = mod.EV}
  if(length(ev.list)>1){tmp.ev = apply(mod.EV,1,mean, na.omit=TRUE)} # previously would turn NA's into 0 but here just omit
  names(tmp.ev) <- ev.mat[,"yr"] # label the elements
  
  # calc mean only up to avg.last.yr (need this option for consistency with old code: new code needs one extra BY to run properly, but this way the devs are same)
  if(!is.na(avg.last.yr)){tmp.mean = mean(tmp.ev[ev.mat[,"yr"]<=avg.last.yr], na.rm=TRUE)}
  if(is.na(avg.last.yr)){tmp.mean = mean(tmp.ev, na.rm=TRUE)}
  
  #env=(tmp.ev[!is.na(tmp.ev)]-tmp.mean)
  env=(tmp.ev-tmp.mean)		# changed to retain the NA, they get filtered out in the data cross-check later (except last 4)
  env
}

Plot.ev <- function(ev=NULL,
                    main="Insert Label",
                    yr.offset=2,
                    main.brood=2015,
                    cyc=F,axis.label = "temp",adj1=1.5, adj2=0.2,fry=F){
  # this function adapted from code by Bronwyn MacDonald
  # "ev" is a time series of deviations from the mean
  # calculated using the getev() function housed in FC_CoreFunctions.R script
  # note that the default for the year labels is a 2-yr offest for ocean entry
  
  # ev.dat <- t(ev); 
  # colnames(ev.dat)=stock$yr+2  # this gives misaligned x-axis labels
  # ev.dat <-  ev.dat[,!is.na(ev.dat)]  # doesn't this shift the plotted bars if there are missing values?
  
  yrs <- as.numeric(names(ev))+yr.offset
  ev.dat <- ev   # for consistency
  names(ev.dat) <- NULL  # to stop automatic x-axis labelling
  
  
  cols <- c("dodgerblue", "firebrick1")[(ev.dat > 0)+1] 
  
  mid.pts <- barplot(ev.dat, main=main, col=cols, 
                     cex.main=1.0, cex.names = 0.7,axes=TRUE) # axes = TRUE gives only y axis!
  box()
  abline(h=0)
  mtext(side=1, "Year", line=3, cex=1.5)
  if(cyc==T){
    i<- main.brood %% 4
    cyc.idx<- which((yrs-yr.offset-i) %% 4==2)
	if(fry==T) cyc.idx<- which((yrs-yr.offset-i) %% 4==3)
    points(mid.pts[cyc.idx],ev.dat[cyc.idx],pch=19,cex=2)
  }
  if(axis.label=="temp"){mtext(side=2, expression(Temperature~(degree ~ C)), line=2,cex=0.8) }
  if(axis.label!="temp"){mtext(side=2, axis.label, line=2,cex=0.8) }

  x.axis.labels.idx <- yrs %in% c(seq(1900,2100,by=10))
  atpt<-mid.pts[x.axis.labels.idx]
  lab<-yrs[x.axis.labels.idx]
  axis(side=1,at= c(atpt,atpt[length(atpt)]*2-atpt[length(atpt)-1]),
       labels=c(lab,lab[length(lab)]*2-lab[length(lab)-1]))
  #axis(side=1,at= seq(1950,2020,by=10),labels =seq(1950,2020,by=10) )
  main.brood.idx <-  yrs %in% main.brood
  abline(v=mid.pts[main.brood.idx],col="firebrick1")
  #if(yr.offset==2) text(mid.pts[main.brood.idx],par("usr")[3]+adj2,labels=expression('4'[2]*' entry') ,col="firebrick1",xpd=NA) #for sockeye only
  #if(yr.offset==1) text(mid.pts[main.brood.idx],par("usr")[3]+adj1,labels=expression('2'[1]*' entry') ,col="firebrick1",xpd=NA) 
  # paste(c(expression(" (",degree,"C)"))) , line=3)
  
}


