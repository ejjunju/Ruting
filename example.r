#graphics.off()
rm(list=ls())
source.with.encoding('Ruting.R', encoding='UTF-8')
library(WriteXLS)
library(tcltk2)
choice<-tk_messageBox(type="yesno",message="Use R to Word Connection?")
if(choice=="yes"){# install.packages("R2wd")
# library(help=R2wd)
#http://www.r-statistics.com/2010/05/exporting-r-output-to-ms-word-with-r2wd-an-example-session/
#install.packages("RDCOMClient")
library(RDCOMClient)
require(R2wd)
wdGet()
}
#Routing##############################
#options(digits=2)
#general

#Input data
choice<-tk_messageBox(type="yesno",message="YES: Input from excel?\nNO Load Saved input.Rdata")
if(choice=="yes"){
require(gdata)
Q<-read.xls("Ruting_Holmvatn_inputs.xlsm",sheet="Tillopsflommer",pattern="Timer") #Flow
Scen<-names(Q)[-1]
Scen<-names(Q)[-1]<-unlist(lapply(strsplit(Scen,"X"),function(x) x[length(x)]))
K<-read.xls("Ruting_Holmvatn_inputs.xlsm",sheet="Kurver",pattern="(mill_m3)") #curves
K<-apply(K,2,as.numeric);inp<-list()
spillcurve<-K[,4:5];names(spillcurve)<-c("Vannstand_masl","Discharge_m3s")
rescurve<-K[,1:2];names(rescurve)<-c("Vol_Mm3","Vannstand_masl")
rescurve<-rescurve[!apply(rescurve,1, function(x) any(is.na(x))),]
spillcurve<-spillcurve[!apply(spillcurve,1, function(x) any(is.na(x))),]
inp<-list(Q=Q,rescurve=rescurve,spillcurve=spillcurve,Scen=Scen)
save(Q,rescurve,spillcurve,Scen,file="input.Rdata")
} else {load("input.Rdata")}

#functions for routing
source.with.encoding('Ruting.R', encoding='UTF-8')

#scenarios
QTS<-unlist(lapply(strsplit(Scen,"_"),function(x)x[1]))
QTS[-c(1,2,length(QTS)-1,length(QTS))]<-Scen[-c(1,2,length(QTS)-1,length(QTS))]
files<-paste(Scen,".rData",sep="")
RES<-vector("list",length=length(Scen));names(RES)<-Scen
#Magasin
magasin<-"MYDAM"
HRV=275

#Route
for( i in 1:(ncol(Q)-1)){
  ix<-i
  vc<-rescurve;sc<-spillcurve;Qi=Q[,ix+1];ts1<-Q[,1] #assign variiables
  print(res<-fx.route(Qi,vc,sc,HRV=HRV,tsteps=ts1,magasin=magasin,rnd=round,plots=T,damkrone=276.50,QT=QTS[ix],addtable=T,mthd="SIC",err=0.001,starts=30,ends=150,lpos="topright")) #route
 dev.copy(png,file=paste(Scen[i],"with-table","jpg",sep="."),width=834,height=804);dev.off()
  res<-fx.route(Qi,vc,sc,HRV=HRV,tsteps=ts1,magasin=magasin,rnd=round,plots=T,damkrone=276.50,QT=QTS[ix],addtable=F,mthd="SIC",err=0.001,starts=30,ends=150,lpos="topright")
 dev.copy(png,file=paste(Scen[i],"only-graph","jpg",sep="."),width=834,height=804);dev.off()
  #save.scenario(ix,Scen,files) #save R.data
  RES[[ix]]<-res;#eval(parse(text=paste("Result.",ix,"<-res",sep="")))
}

#Export Q1000 to excel
q1<-as.data.frame(RES[[1]]);q1$Volum<-q1$Volum/1e6
q2<-as.data.frame(RES[[2]]);q2$Volum<-q2$Volum/1e6
WriteXLS(x=c("q1","q2"),ExcelFileName="Ruting_Holmvatn_Result_Q1000_2.xlsx",perl="C:/Perl64/bin/perl.exe",BoldHeaderRow=TRUE, AdjWidth=TRUE, AutoFilter=TRUE, 
         SheetNames=c("Q1000","1.5Q1000") ,Encoding="latin1")

#Sensitivity 
LL<-c(TRUE,FALSE);XX<-c("with-table","only-graph")
for(i in 1:2){
Kulm<-fx.sens.plots(RES[c(1,3,5)],QT="Q1000",magasin="Holmvtan",addtable=LL[i],rnd=round,lpos="topright",damkrone=276.50,ncol=1)
dev.copy(png,file=paste("Qmom.Qdøgn",XX[i],"jpg",sep="."),width=834,height=804);dev.off()
Klima<-fx.sens.plots(RES[c(1,7,8)],QT="Q1000",magasin="Holmvtan",addtable=LL[i],rnd=round,lpos="topright",damkrone=276.50,ncol=1)
dev.copy(png,file=paste("Klimaendring",XX[i],"jpg",sep="."),width=834,height=804);dev.off()
}

