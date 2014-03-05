#'This Functions plots the results from routing
#'\code{fx.sens.plots} Given a list of dataframes or a dataframe with hours (Timer) inflow (Tilløpslom),Volume (Volum), outflow (Avløpsflom) and Water-level (vannstand), its plots the results om one graph.
#'@param L a List or dataframe with items Timer,Tilløpsflom,Volum,Vannstand, Avløpsflom
#'@param magasin Name of Reservoir
#'@param QT a text string denoting return period e.g QT="Q1000"
#'@param addtable Logical. addtable=TRUE adds a summary table to the results. Default is FALSE
#'@param rnd Rouding function. can be either round,ceiling or floor. used to format axes limits.Default is round.
#'@param damkrone Elevation of the Top of the dam. Default is null. If not NULL, the summary table contains the max elevation of the water surface above the top of the dam.
#'@param lpos legend positio either (top,bottom,center,bottomright,topright, topleft.topright)
#'@return A summary table conatining the maximum values of the Tilløpsflom,Avløpsflom, maximum water level and rise in water level relative to the HRV (highest Regulated water level) or water level at the beginning of the routing.
#'@author Emmanuel Jjunju, ejjunju@gmail.com
#'@export
fx.sens.plots<-function(L,magasin,QT,addtable=F,rnd=round,damkrone=NULL,lpos="bottomright",ncol=1){
  sens<-T
  if(is.data.frame(L)){L<-list(L);sens<-F}
  nL<-length(L)
  require(zoo)
  tsteps<-L[[1]]$Timer
  Tilløpsflom<-do.call("cbind",lapply(L,function(x) x$Tilløpsflom))
  Avløpsflom<-do.call("cbind",lapply(L,function(x) x$Avløpsflom))
  Vannstand<-do.call("cbind",lapply(L,function(x) x$Vannstand))
  
  #Plots
  #par(mar=c(5,5,3,4))
  par(mar=c(4,5,4,5),oma=c(1,0,0,2))
  if(addtable==T){
    layout(rbind(1,2), heights=c(8,4)) 
    if(ncol(Tilløpsflom)>1){
      layout(rbind(1,2,3), heights=c(8,2,4))
    }
  } else {
    layout(rbind(1), heights=c(8)) 
    if(ncol(Tilløpsflom)>1){
      layout(rbind(1,2), heights=c(8,2)) 
    }    
  }
  H<-round_any(max(Tilløpsflom),50,ceiling)-max(Tilløpsflom)
  H2<-round_any(max(tsteps),12,rnd)-max(tsteps)
  plot(tsteps,Tilløpsflom[,1],type="l",lty=1,col="cornflowerblue",lwd=2,
       xlab="Tid(timer)",
       ylab=bquote(.("\nVannføring (") ~ m^3 ~"/s)"),
       ylim=c(0,max(Tilløpsflom)+H), xlim=c(0,max(tsteps)+H2),
       yaxs="i",xaxs="i",
       axes=F)
  box()
  if(ncol(Tilløpsflom)>1){for(i in 1:ncol(Tilløpsflom)){lines(tsteps,Tilløpsflom[,i],col="cornflowerblue",lty=i,lwd=2)}}
  #rect(0,0,max(tsteps)+H2,max(Tilløpsflom)+H,col="#d3d3d335")
  Y<-seq(0,max(pretty(Tilløpsflom)),diff(pretty(Tilløpsflom))[1]/2)
  abline(h=Y,lty=2,col="lightblue")
  axis(2,Y,Y,las=1,col.ticks="lightblue",col.axis = 'blue', col.lab = 'blue')
  title(paste("Vannstand og vannføring",QT,magasin),cex=0.8)
  
  if(ncol(Tilløpsflom)>1){Kol<-"red"} else {Kol<-"darkblue"}
  for(i in 1:ncol(Avløpsflom)){lines(tsteps,Avløpsflom[,i],lwd=3,col=Kol,lty=i)}
  
  #stage
  par(new=T)
  (Y<-seq(min(pretty(Vannstand)),max(pretty(Vannstand)),diff(pretty(Vannstand))[1]/2))
  if(!(ncol(Vannstand)>1)){LTY<-3}else{LTY<-1}
  plot(tsteps,Vannstand[,1],type="l",axes=F,ylab="",xlab="",lwd=2,
       ylim=range(Y),yaxs="i",xaxs="i",xlim=c(0,max(tsteps)+H2),lty=LTY)
  if(ncol(Vannstand)>1){for(i in 2:ncol(Vannstand)){lines(tsteps,Vannstand[,i],lwd=2,col=1,lty=i)}}
  axis(4,Y,sprintf("%5.2f",Y),las=2,col.axis = 'gray20', col.lab = 'gray20')
  mtext("Magasinvannstand (moh)",side=4,padj=5,cex=1)
  abline(h=Y,lty=3,col="grey80")
  
  (TSTEPS<-seq(min(tsteps),max(tsteps)+H2,12))
  axis(1,TSTEPS,TSTEPS,las=1)
  abline(v=TSTEPS,lty=2,col="grey")
  box()
  
  #par(mar=c(0,0,0,0))
  #plot.new()
  # c(bottom, left, top, right)
  
  legend(lpos,c("Tilløpsflom","Flom ut av magasinet","Vannstand i magasinet"), lty = c(1,1,LTY),lwd=c(2,3,2),col=c("cornflowerblue",Kol,"black"),  bty="y",   ncol=1,bg =rgb(1,1,1,0.9),cex=1) 
  
  if(ncol(Tilløpsflom)>1){
    par(mar=c(0,0,0,0))
    plot.new()
    #Legend
    if(sens==T){legend("center",leg=names(L),lty=1:length(L),bty="y",ncol=ncol)}
  }
  
  #add table of max. values
  tab<-lapply(L,function(out) {tab<-rbind(round(out[1,],2),round(apply(out,2,max),2))
                               tab<-rbind(tab,diff=round(apply(tab,2,diff),2))
                               tab2<-tab[-1];  colnames(tab2)<-names(tab)[-1]
                               rownames(tab2)<-c("Start","Maks","Diff")
                               tab<-(c(round(tab2[2,1],0),round(tab2[2,4],0),tab2[1,3],tab2[2,3],tab2[3,3]))
                               tab[-c(1:2)]<-sapply(tab[-c(1:2)],function(x)sprintf("%.2f",x));return(tab)}
  )
  
  nom<-c("Kulminasjonsverdi for tilløpsflom         (m3/s)",
         "Kulminasjonsverdi for avløpsflom        (m3/s)",
         "Vannstand ved flommens begynnelse(moh.)",
         "Høyeste vannstand i magasinet            (moh.)",
         "Maksimalvannstandstigning                        (m)")
  tab<-do.call("cbind",tab)
  tab<-data.frame(nom,tab) 
  colnames(tab)<-c(paste(magasin,QT),names(L))
  if(!is.null(damkrone)){
    nom2<-paste("Overtopping av brystning (",damkrone,"moh.)     (m)")
    tab<-apply(tab,2,as.character)
    dh1<-(as.numeric(tab[4,-1])-damkrone);
    dh1<-sapply(dh1,function(x) max(0,x))
    dh<-sprintf("%.2f",dh1)
    tab<-rbind(tab,tab[nrow(tab),])
    #print(tab[nrow(tab),])
    tab[nrow(tab),]<-c(nom2,dh)
    
  }
  
  if(addtable==TRUE){
    require(plotrix)
    par(mar=c(1,0,0,0))
    plot(1:5,1:5,pch="",xaxt="n",yaxt="n",ylab="",xlab="",axes=F)
    addtable2plot(x="top",table=tab,bty="o",cex=1.1,display.rownames=F,hlines=F,vlines=F)  
    box("outer", col="blue")
  }
  
  par(mfrow=c(1,1),oma=c(1,1,1,0),mar=c(4,4,2,1)+.1) 
  print(tab)
  return(tab)
}

#'This function routes a given inflow hydrogram (Tilløpsflom) through a reservoir
#'Given the reservor curve, spillway curve
#'\code{fx.route} Routes an inflow hydrograph through a resrvoir.
#'@param Tilløpsflom inflow hydrograph (series)
#'@param rescurve Reservoir curve; a data frame ("Vol_Mm3","Vannstand_masl")
#'@param spillcurve spillway capacity curve; a dataframe ("Vannstand_masl", "Discharge_m3s")
#'@param HRV Highest Regulated water level. Used as beginning water level. Defaults to lowest elevel in the spillcurve
#'@param damkrone Elevation of the dam top. Deafult ois NULL
#'@param tsteps Series of time steps in units of hoursfrom o to the end.
#'@param QT A string for the return period; e.g=QT="Q1000"
#'@param magasin name of the reservoir
#'@param rnd Rounding function needed by plotting function. Default is ceiling. can be either ceiling, floor or round
#'@param addtable
#'@param plots Logical. plots=TRUE plots a figure.
#'@mthd Ruting routine. can either be "SIC" (Storage Indication Method) or "SWECO" which is a routine originally implemented in an Excel worksheet by SWECO. the results should be the same or not very different
#'@param err Used in conjuction with "SWECO". An minimum error whiloe iterating for the Volume. Default is 0.001
#'@param starts Start Timestep. Default is 1 (first timestep Timer=0)
#'@param ends  End timestep. default is length of Tilløpsflom.
#'@param lpos legend positio either (top,bottom,center,bottomright,topright, topleft.topright)
#'@return Data frame with Tilløpsflom, Volum, vannstand and Avløpsflom
#'@author Emmanuel Jjunju, ejjunju@gmail.com
#'@export
fx.route<-function(Tilløpsflom,
                   rescurve,
                   spillcurve,
                   HRV=spillcurve[spillcurve[,2]==0,1],damkrone=NULL,
                   tsteps=(1:length(Tilløpsflom))-1,tstep=diff(tsteps)[1],
                   QT="Q1000",
                   magasin="",
                   rnd=ceiling #ceiling,floor,round (controls axes)
                   ,addtable=T,
                   plots=F,
                   mthd="SIC",
                   err=0.001, #used with SWECO,
                   starts=1,
                   ends=length(Tilløpsflom),
                   lpos="bottomright",
                   ncol=1
){
  #packages and functions needed
  need<-c("Hmisc","plyr","plotrix","tcltk2") #needed packages for a job
  ins<-installed.packages()[,1] #find out which packages are installed
  (Get<-need[which(is.na(match(need,ins)))]) # check if the needed packages are installed
  if(length(Get)>0){install.packages(Get)} #install the needed packages if they are not-installed
  eval(parse(text=paste("require(",need,")")))#load the needed packages
  
  #Check start and end and select the needed data
  if(ends>length(Tilløpsflom)){ends<-length(Tilløpsflom)}
  Tilløpsflom=Tilløpsflom[starts:ends];
  tsteps<-tsteps[1:length(Tilløpsflom)]
  
  #Containers
  Avløpsflom<-rep(NA,length(Tilløpsflom))
  Vannstand<-Volum<-Avløpsflom
  nT<-length(Avløpsflom)
  
  #Curves yfx|y=f(x)
  qfh<-approxfun(spillcurve)
  hfq<-approxfun(spillcurve[,2:1])
  hfv<-approxfun(rescurve)
  vfh<-approxfun(rescurve[,2:1])
  
  #Storage-indication Method
  if(mthd=="SIC"){
    (Dt<-tstep*3600)
    (Volum[1]<-vfh(HRV)*1e6) #S1 #Initial Storage in m3
    (Vannstand[1]<-hfv(Volum[1]/1e6))
    print(O<-spillcurve[,2]) #Select a value of O)
    print(S<-vfh(spillcurve[,1])*1e6)#Determine S from the H-Q then S-Hcurve.
    (S.plusO.Dtby2<-S+O*Dt/2) #S+ODt/2
    SIC<-approxfun(S.plusO.Dtby2,O);invSIC<-approxfun(O,S.plusO.Dtby2)
    
    for(i in 1:(nT-1)){
      (I<-(Tilløpsflom[i]+Tilløpsflom[i+1])/2) #Average Inflow (I1+I2)/2
      (Avløpsflom[i]<-qfh(Vannstand[i])) ##Avløpsflom at start O1
      (S2.plus.O2.DTby2<-I*Dt+ (Volum[i]-0.5*Avløpsflom[i]*Dt)) # S2 + 0.5*O2Dt
      (Avløpsflom[i+1]<-SIC(S2.plus.O2.DTby2))#O2
      print("###################################################################")
      print(a<-(Volum[i+1]<-S2.plus.O2.DTby2-0.5*Avløpsflom[i+1]*Dt))
      print(b<-(Volum[i+1]<-I*Dt + Volum[i]-0.5*Dt*(Avløpsflom[i]+Avløpsflom[i+1])))
      #print(cbind(a,b))
      (Vannstand[i+1]<-hfv(Volum[i+1]/1e6))
    }
  }
  
  #Sweco method
  if(mthd=="SWECO"){
    (Volum[1]<-vfh(HRV))
    for(i in 1:(nT-1)){
      V1<-Volum[i] # Volume_start_timestep 
      Z1<-Vannstand[i]<-hfv(V1) #Stage_start_timestep
      o1<-Avløpsflom[i]<-qfh(Z1) #Discharge_start_timestep 
      i1<-Tilløpsflom[i] #Inflow_start_timestep
      i2<-Tilløpsflom[i+1]#Inflow_end_timestep
      o2<-o1#Discharge_end_timestep = Discharge_start_timestep
      V2<-0 #Volume_end_timestep
      for(j in 1:50){
        dummy<-V2 #Dummy = Volume_end_timestep
        DV<-tstep*3600 * ((1 / 2) * (i1 + i2) - (1 / 2) * (o1 + o2)) / 1000000
        V2<-V1+DV # Volume_end_timestep
        Z2<-hfv(V2)# Stage_end_timestep
        o2<-qfh(Z2)#Discharge_end_timestep
        success<-FALSE
        #print(dummy)
        if(abs(dummy-V2)<=err){
          success<-TRUE
          Volum[i+1]<-V2
          Avløpsflom[i+1]<-o2
          Vannstand[i+1]<-Z2
        }
        if(success==TRUE){break()}
      }
      if(success==FALSE){
        tk_messageBox(type="ok",
                      message=paste("Failed to converge at timestep ", i,". Computation aborted"))
        stopifnot(success==TRUE)
      }
    }
  }
  
  
  out<-data.frame(Timer=tsteps,Tilløpsflom, Volum,Vannstand,Avløpsflom)
  #print(out)
  #(head(out))
  #print(apply(out,2,function(x)round(x,1)))
  
  if(plots==T){
    #     par(mfrow=c(2,2))
    #     plot(spillcurve)
    #     plot(rescurve)
    #     plot(O,S.plusO.Dtby2,main="Storage Indication Curve",type="l",ylab="S+0.5*O*Dt")
    par(mfrow=c(1,1))
    fx.sens.plots(out,magasin=magasin,QT=QT,addtable=addtable,rnd=rnd,damkrone=damkrone,lpos=lpos,ncol=ncol)
  }
  
  return(out)
}




rem.rows.with.any.na<-function(y){y[!apply(y,1,function(x)any(is.na(x))),]}

fx.read.xls<-function(xls,sheet,pat="(mill_m3)",housekeep=F){
  need<-c("gdata","tcltk2","plyr","Hmisc") #needed packages for a job
  ins<-installed.packages()[,1] #find out which packages are installed
  (Get<-need[which(is.na(match(need,ins)))]) # check if the needed packages are installed
  if(length(Get)>0){install.packages(Get)} #install the needed packages if they are not-installed
  eval(parse(text=paste("require(",need,")")))#load the needed packages
  
  #check Ruting
  d<-read.xls(xls=xls,sheet=sheet,pattern=pat)
  #Tilløpsflom
  Tilløpsflomdf<-(d[1:170,1:2])
  Tilløpsflomdf<-rem.rows.with.any.na(Tilløpsflomdf)
  tsteps<-as.numeric(as.character(Tilløpsflomdf[,1]))
  Tilløpsflom<-Tilløpsflomdf[,2];head(Tilløpsflom<-as.numeric(as.character(Tilløpsflom)))
  
  #RESERVOIR CURVE
  rescurve<-(d[1:29,5:6]);
  rescurve<-rem.rows.with.any.na(rescurve)
  rescurve<-apply(rescurve,2,as.numeric)
  colnames(rescurve)<-c("Vol_Mm3","Vannstand_masl")
  head(rescurve<-rescurve[apply(rescurve,1,function(x) !any(is.na(x))),])
  
  ##RESERVOIR CURVE
  spillcurve<-(d[1:29,8:9]);
  spillcurve<-rem.rows.with.any.na(spillcurve)
  spillcurve<-apply(spillcurve,2,as.numeric)
  colnames(spillcurve)<-c("Vannstand_masl","Discharge_m3s")
  head(spillcurve<-spillcurve[apply(spillcurve,1,function(x) !any(is.na(x))),])
  
  if(housekeep==TRUE){#Housekeeping so that minZ(reskuve)==minZ(magcurve)
    #extrapolate
    (minZspill<-apply(spillcurve,2,min)[1])
    (minZres<-apply(rescurve,2,min)[2])
    if(minZspill<minZres){
      ex<-do.call("cbind",approxExtrap(rescurve[,2],rescurve[,1],minZspill))
      colnames(ex)<-rev(colnames(rescurve))
      rescurve<-rbind(ex[,2:1],rescurve)
    }
    if(minZspill>minZres){
      ex<-do.call("cbind",approxExtrap(spillcurve[,1],spillcurve[,2],minZres))
      colnames(ex)<-colnames(spillcurve)
      spillcurve<-rbind(ex,spillcurve)
      
    }
  }
  
  out<-list(Tilløpsflom=Tilløpsflom,spillcurve=spillcurve,rescurve=rescurve,tsteps=tsteps)
  
}

savex<-function(x) save(x,file=paste(file=deparse(substitute(x)),".Rdata",sep=""))
save.scenario<-function(i,scenario,files){
  eval(parse(text=paste(scenario[1],c(".in",".ut"),"<-",c("inp","res"),sep=""))) #create objects
  eval(parse(text=paste("save(",paste(scenario[1],c(".in",".ut"),sep="",collapse=","),",file=files[",1,"])")))
}
