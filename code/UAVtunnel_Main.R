#Accompanying: "A wind-tunnel assessment of parameters that may impact spray drift during 
  #UAV pesticide application" by Shanique Grant et al.
#Description: This file creates most figures and tables, and runs statistics. Sections below
  #are labeled by the figures, tables, and analyses they include.
#Last saved: 2022-07-07
#Author: Farah Abi-Akar


# Data prep: Import deposition data ------------------------------------------------------------------

  library(reshape)
  library(reshape2)
  library(plyr)
  library(dplyr)
  library(ggplot2)
  library(GGally)
  library(gridExtra)
  library(car)

  #Function to format axis labels
  axisFormat<-function(x){
    fList<-c()
    
    for(val in x){
      fVal=""
      if(is.na(val)){
        fVal=val
      }else{
        if(val>=10000){
          fVal=format(val, big.mark=",", big.interval=3L,scientific=FALSE)
        }else{
          fVal=val
        }
      }
      fList<-append(fList,fVal)
    }
    return(fList)
  }
  
  # change hyphen to minus sign for labels
  axisMinus<-function(x){
    fList<-c()
    
    for(val in x){
      fVal=""
      
      if(is.na(val)){
        fVal=val
      }else{
        if(val<0){
          fVal=gsub("-", "\U2212", val )
        }else{
          fVal=val
        }
      }
      fList<-append(fList,fVal)
    }
    return(fList)
    
  }

  #Change these locations to yours:
    infolder<- "YOUR FILE PATH"
    outfolder<- "YOUR FILE PATH"
  
  #Import, column names
    setwd(infolder)
    main<- read.csv("DepositionSlidesData.csv") #Dep_flux is in ng/cm2
    colnames(main)<- tolower(colnames(main))
    main<- reshape::rename(main, c(dep_flux = "mass", ws.m_s="ws", nozzle_dsd="nozz"))
  
  #ID replicates simply as 1-3
    main$rep<- ave(main$testid, main$rep, FUN=dense_rank)
    main<- subset(main, select=-testid)
  
  #Make variables into factors
    main$volume<- as.factor(main$volume)
    main$nozz<- gsub("11001", "",main$nozz)
    main$nozz<- as.factor(main$nozz)
    main$wsf<- as.factor (main$ws)
  
  #Convert measured wind speed from ft/min to m/s (3.280840 ft/1m) and make it relative
    main$ws.meas<- main$ws.meas / (3.280840 * 60)
    main$ws.rel<- main$ws.meas - main$ws
    # ggplot(main, aes(x=wsf, y=ws.rel)) + geom_boxplot() + geom_jitter()
    
  #Interaction variable to test later
    main$r.t.w<- main$rh * main$ws.rel/ main$temp.c
    
  #Limit to distances greater than or equal to 2m for spray drift analysis
    main1<- subset(main, distance>1.1)
  #Remove the one low outlier: N=Lechler, dist=20, vol=10, ws=1.5
    main1<- subset(main1, mass>.2)
    

# Data prep: Import 3D anemometer data --------------------------------------

  ane2<- read.csv("3D_Anemometer_2L.csv")
  ane10<- read.csv("3D_Anemometer_10L.csv")
  ane2$vol <- 2
  ane10$vol<- 10
  ane<- rbind(ane2, ane10);  rm(ane2, ane10)
    
  colnames(ane)<- tolower(colnames(ane))
  ane<- reshape::rename(ane, c(distance_m = "distance", vol="volume", nom_ws="ws")) 
  
  #Factors
    ane$width<- as.factor(ane$width)
    ane$width<- factor(ane$width, levels=c("0.99", "2.1",  "3.3"),
                        labels=c("L","C","R"))
    ane$width<- factor(ane$width, levels=c("L","C","R"))
    ane$height<- as.factor (substr(ane$location, 1,1))
    ane$vel_dir<- as.factor(ane$vel_dir)
    
  #Subset, and retain a set with all distances for graphing
    aneMore<- subset(ane, select=-c(source, velocity_mph, vel_dir_label, location))
    ane<- subset(aneMore, distance>1.1)
  
    aneMore$height2<- revalue(aneMore$height, c("B"="Bottom","M"="Middle","T"="Top"))
    aneMore$width2<- revalue(aneMore$width, c('L'='Left',"C"="Center", "R"="Right"))
  
  

# Data prep: 3D data structure, metrics ----------------------------------

  ane$lab<- paste(ane$vel_dir, ane$height, ane$width, sep="")
  
  #Center values within nominal velocity and payload volume
    mymean<- function (x) {mean(na.omit(x))}
    ane$mnvel <- ave(ane$velocity_ms, ane$lab, ane$ws, ane$volume, FUN = "mymean")
    ane$velc<- ane$velocity_ms - ane$mnvel
    #Checks.  Before and after. 
      # ggplot(ane, aes(x=ws, y=velocity_ms)) + geom_point() + facet_wrap(~lab) + geom_smooth(method="lm")
      # ggplot(ane, aes(x=ws, y=velc)) + geom_point() + facet_wrap(~lab) + geom_smooth(method="lm")
  
  #Cast
    anec<- dcast(ane, ws + volume + distance ~ lab, value.var="velc")

  #Metrics to test statistically. Mean of walls at all heights.
    #X: 
      anec$x.blr<- with(anec, (-XBL+ XBR)/2)
      anec$x.mlr<- with(anec, (-XML+ XMR)/2) 
      anec$x.tlr<- with(anec, (-XTL+ XTR)/2) 
    #Z: 
      anec$z.blr<- with(anec, (ZBL+ ZBR)/2) 
      anec$z.mlr<- with(anec, (ZML+ ZMR)/2) 
      anec$z.tlr<- with(anec, (ZTL+ ZTR)/2) 
    #Y: 
      anec$y.blr<- with(anec, (YBL+ YBR)/2)
      anec$y.mlr<- with(anec, (YML+ YMR)/2) 
      anec$y.tlr<- with(anec, (YTL+ YTR)/2)

  #Tag these on to main1
    temp<- subset(anec, select=c(ws, volume, distance, x.blr, x.mlr, x.tlr,z.blr, z.mlr, 
                                 z.tlr, XBC,ZBC,ZMC,y.blr, y.mlr, y.tlr, YMC,YBC))
    main1<- merge (main1, temp, all=T);  rm(temp)

      
# Fig A3, A9.  3D anemometer graphs -----------------------------------------------

  #3D anemometer dimensions and locations vs each other (showing multicollinearity)
    forgg<- subset(anec, select=c(volume, YBC, YMC, y.blr, y.mlr, y.tlr, x.blr, x.tlr,
                                  ZBC, ZMC, z.blr, z.mlr, z.tlr))
    forgg$volume<- as.factor(forgg$volume)
    ggpairs(forgg, lower=list(continuous="smooth", mapping = aes(color=volume))) +
      theme(text=element_text(size=8))
  
  #Fig A9.  3D anemometer curves, all versions, across distance
    g.3dcurve.15<- ggplot(subset(aneMore, ws==1.5), 
           aes(x=distance, y=velocity_ms, color=height2, linetype=as.factor(volume))) +theme_bw() +
      facet_grid(vel_dir~width2) + geom_hline(yintercept = 0, color="gray80", size=1)+
      geom_vline(xintercept = 0, color="gray80", size=1) + geom_line() +
      scale_y_continuous(labels=function(x) axisMinus(x))+
      ylab("Velocity (m/s)") +xlab("Distance (m)") + ggtitle("Wind speed (m/s): 1.5") +
      scale_color_manual("Height", values=c("black","dodgerblue","chocolate3"))+
      scale_linetype_discrete("Payload (L)") + theme(legend.position = "bottom", text=element_text(size=10))
    g.3dcurve.30<- ggplot(subset(aneMore, ws==3), 
           aes(x=distance, y=velocity_ms, color=height2, linetype=as.factor(volume))) +
      facet_grid(vel_dir~width2) + geom_hline(yintercept = 0, color="gray80", size=1)+
      geom_vline(xintercept = 0, color="gray80", size=1) + geom_line() +
      scale_y_continuous(labels=function(x) axisMinus(x))+
      theme_bw()  + ylab("Velocity (m/s)") + xlab("Distance (m)") +  ggtitle("Wind speed (m/s): 3.0") +
      scale_color_manual("Height", values=c("black","dodgerblue","chocolate3"))+
      scale_linetype_discrete("Payload (L)") + theme(legend.position = "bottom", text=element_text(size=10))
    g.3dcurve.45<- ggplot(subset(aneMore, ws==4.5), 
           aes(x=distance, y=velocity_ms, color=height2, linetype=as.factor(volume))) +
      facet_grid(vel_dir~width2) + geom_hline(yintercept = 0, color="gray80", size=1)+
      geom_vline(xintercept = 0, color="gray80", size=1) + geom_line() +
      scale_y_continuous(labels=function(x) axisMinus(x))+
      theme_bw()  + ylab("Velocity (m/s)") + xlab("Distance (m)") +  ggtitle("Wind speed (m/s): 4.5") +
      scale_color_manual("Height", values=c("black","dodgerblue","chocolate3"))+
      scale_linetype_discrete("Payload (L)") + theme(legend.position = "bottom", text=element_text(size=10))
    setwd(outfolder)
    ggsave(g.3dcurve.15, file="g.3dcurve.15_RAug10.jpg", width=6.5, height=7)
    ggsave(g.3dcurve.30, file="g.3dcurve.30_RAug10.jpg", width=6.5, height=7)
    ggsave(g.3dcurve.45, file="g.3dcurve.45_RAug10.jpg", width=6.5, height=7)
    

  #Fig A3.  Centering figure _________________________________________
    #before
    p1.before<- ggplot(subset(ane, lab=="ZBC" & distance>1.1 ), 
                       aes(x=distance, y=velocity_ms, color=as.factor(ws), linetype=as.factor(volume))) + 
      geom_line() + ylab("Velocity:\nZ, bottom, center (m/s)") + xlab("Distance (m)") +theme_bw()+
      scale_y_continuous(labels=function(x) axisMinus(x))+
      scale_color_manual("Tunnel wind speed (m/s)", values = c('cyan2','cornflowerblue','navyblue')) +
      scale_linetype_manual("Payload (L)", values=c(1,2)) +
      theme(legend.position = c(.58,.18), legend.direction="horizontal",
            legend.spacing = unit(.01,"cm"), text=element_text(size=9),
            legend.key.height = unit(.1,"cm"), legend.key.width = unit(.43,"cm")) +
      ggtitle("A. Before centering, as-is") 
    p2.before<- ggplot(subset(ane, lab=="ZBC" & distance>1.1),aes(x=ws, y=velocity_ms)) + 
      geom_point(aes(color=factor(ws), shape=as.factor(volume))) + theme_bw() + ggtitle("") +
      scale_y_continuous(labels=function(x) axisMinus(x))+
      geom_smooth(method="lm", color="gray50", size=.5) +
      scale_shape_manual ("Payload (L)", values=c(1,2)) +
      scale_x_continuous("Tunnel wind speed (m/s)", breaks=c(1.5, 3, 4.5)) +
      ylab("Velocity:\nZ, bottom, center (m/s)") + theme_bw()+
      scale_color_manual("Tunnel wind speed (m/s)", values = c('cyan2','cornflowerblue','navyblue')) +
      theme(legend.position = "none", text=element_text(size=9))
    #after
    p1.after<- ggplot(subset(ane, lab=="ZBC" & distance>1.1), 
                      aes(x=distance, y=velc, color=as.factor(ws), linetype=as.factor(volume))) + 
      ggtitle("B. After centering") +
      geom_line() + ylab("Centered velocity:\nZ, bottom, center (m/s)") + xlab("Distance (m)") +theme_bw()+
      scale_color_manual("Tunnel wind speed (m/s)", values = c('cyan2','cornflowerblue','navyblue')) +
      scale_linetype_manual("Payload (L)", values=c(1,2)) + 
      scale_y_continuous(breaks=c(0,-1), labels=function(x) axisMinus(x)) +
      theme(legend.position = c(.58,.18), legend.direction="horizontal",
            legend.spacing = unit(.01,"cm"), text=element_text(size=9),
            legend.key.height = unit(.1,"cm"), legend.key.width = unit(.43,"cm")) +
      geom_hline(yintercept = 0, linetype=2, color="gray80")
    p2.after<- ggplot(subset(ane, lab=="ZBC" & distance>1.1), aes(x=ws, y=velc))+ 
      geom_hline(yintercept = 0, linetype=2, color="gray80") + 
      geom_point(aes(color=factor(ws), shape=as.factor(volume)))+theme_bw()+ggtitle("") +
      geom_smooth(method="lm", color="gray50", size=.5)+
      scale_shape_manual ("Payload (L)", values=c(1,2)) +
      scale_y_continuous(breaks=c(0,-1), labels=function(x) axisMinus(x)) +
      scale_x_continuous("Tunnel wind speed (m/s)", breaks=c(1.5, 3, 4.5)) +
      ylab("Centered velocity:\nZ, bottom, center (m/s)") +
      scale_color_manual("Tunnel wind speed (m/s)", values = c('cyan2','cornflowerblue','navyblue')) +
      theme(legend.position = "none", text=element_text(size=9))
    
    p2.legendonly<- ggplot(ane,aes(x=ws, y=velocity_ms)) + 
      geom_point(aes(color=factor(ws), shape=as.factor(volume)))+
      scale_shape_manual ("Payload (L)", values=c(1,2)) + theme_bw()+
      scale_color_manual("Tunnel wind speed (m/s)", values = c('cyan2','cornflowerblue','navyblue')) +
      theme(text=element_text(size=9))
    
    setwd(outfolder)
    g.cntrBefore<- grid.arrange(p1.before, p2.before, ncol=2, 
                                heights=3, widths=unit(c(4,2.5), c("in")))
    g.cntrAfter<- grid.arrange(p1.after, p2.after, ncol=2, 
                               heights=3, widths=unit(c(4,2.5), c("in")))
    ggsave(g.cntrBefore, file="g.cntrBefore2_RAug10.jpg", width=6.5, height=2.3)
    ggsave(g.cntrAfter, file="g.cntrAfter2_RAug10.jpg", width=6.5, height=2.3)
    ggsave(p2.legendonly, file="p2.legendonly_RAug10.jpg", width=2, height=2)

        

# Fig 4.  Oxford Lasers graph and ANOVA -----------------------------------------------------------------

  #Import, strings, factors 
    setwd(infolder)
    ox2<- read.csv("OxfordReps.csv")
    ox2$both<- paste(ox2$Condition, ox2$Nozzle, sep=", ")
    ox2$Nozzle<- gsub("TTI", "TT", ox2$Nozzle)
    ox2$Nozzle<- gsub("IDK", "Lechler IDK\n", ox2$Nozzle)
    ox2$Nozzle<- as.factor(ox2$Nozzle)
    ox2$Condition<- as.factor(ox2$Condition)
    ox2$Condition<- factor(ox2$Condition, levels=rev(levels(ox2$Condition)))
    
  #Calc mean among replicates in new columns, for graphing
    ox2$mn.10<- ave(ox2$Dv10, ox2$Nozzle, ox2$Condition, FUN="mean")
    ox2$mn.50<- ave(ox2$Dv50, ox2$Nozzle, ox2$Condition, FUN="mean")
    ox2$mn.90<- ave(ox2$Dv90, ox2$Nozzle, ox2$Condition, FUN="mean")

  #Fig 4. Nozzle DVs 
    g.ox2<- ggplot(ox2, aes(color=Condition, x=Nozzle, y=mn.50,ymin=mn.10, ymax=mn.90)) +
      ylab(expression(paste(Dv[10], ", ", Dv[50], ", ", Dv[90] ~ (mu*m) )))    + theme_bw() + 
      geom_pointrange(aes(), position=position_dodge(width=.75), shape=5, size=.5)    +
      scale_color_manual (values=c("black","dodgerblue", "chocolate3")) +
      geom_point(aes(y=Dv90), position=position_jitterdodge(dodge.width=0.7), size=.6) +
      geom_point(aes(y=Dv50), position=position_jitterdodge(dodge.width=0.7), size=.6) +
      geom_point(aes(y=Dv10), position=position_jitterdodge(dodge.width=0.7), size=.6)
    setwd(outfolder)
    ggsave(g.ox2, file="g.ox3.jpg", width=4.2, height=2.5)

  #ANOVA  ________________________________________________________________________
  #2-way ANOVA
    afit<- aov(Dv50 ~ Nozzle + Condition, data=ox2)
    summary(afit)
    #without medium to check whether Off driven by Med nozzle
    afitT<- aov(Dv50 ~ Nozzle + Condition, data=subset(ox2, Nozzle!="TT (medium)"))
    summary(afitT)
    #ONLY medium, therefore can't include nozzle variable (only 1)
    afit1<- aov(Dv50 ~ Condition, data=subset(ox2, Nozzle=="TT (medium)"))
    summary(afit1)

  #Checks: normality, variance tests on ANOVA
    shapiro.test (residuals(afit)) 
    bartlett.test (Dv50 ~ both    , data=ox2) 

  #Post-hoc: a Tukey to distinguish which
    TukeyHSD(afit)  #all nozzles; use Nozzle results, not condition
    TukeyHSD(afit1) #Only TT; use condition.

    

# Fig 5.  In-swath trays and slides -----------------------------------------------------------

  #Import tray data
    tray<- read.csv("TrayVolumes.csv")
    colnames(tray)<- c('nozz','payload','ws.meas','temp.c','rh','trayup','trayin','traydown','total')

  #Convert ws.meas (measured wind speed) from ft/min to m/s (3.280840 ft/1m)
    tray$ws.meas<- tray$ws.meas / (3.280840 * 60)
    tray$wsf<- ifelse(tray$ws.meas<2.2, 1.5, 3.0)
    tray$wsf [tray$ws.meas>3.9] <- 4.5

  #Make into factors
    tray$wsf  <- as.factor(tray$wsf )
    tray$nozz<- as.factor(tray$nozz)
    tray$payload<- as.factor(tray$payload)
    tray$nozz<- revalue(tray$nozz, c("Lechler IDK 110-04"="Lechler IDK  (coarse)",
                                     "TT11001"="TT (medium)", "XR11001"="XR (fine)"))
  #Melt
    traym<- reshape2::melt (tray, id=c("wsf","nozz","payload",'temp.c','rh','ws.meas'), 
                            measure.vars=c('trayup','trayin','traydown'))
    traym$variable<- revalue(traym$variable, c('trayup'="-1", 'trayin'='0','traydown'='1'))
    
  #Glass slides: subset to -3 to +3m
    inslide<- subset(main, distance<4)

  #Prep for merge 
    pret<- traym
    pret$distance<- as.integer (as.character(pret$variable))
    pret<- reshape::rename(pret, c(value="trayvol"))
    inslide<- reshape::rename(inslide, c(volume="payload"))
    pret<- subset(pret, select=c(distance, nozz, wsf, payload, trayvol))
    
  #Mean among the three replicates
    pret<- pret %>%
      group_by (wsf,nozz,payload, distance) %>%
      summarise (trayvolm = mean(trayvol)) 
    prec<- inslide %>%
      group_by (wsf,nozz,payload, distance) %>%
      summarise (massm = mean(mass))
    
  #Merge
    both<- merge(prec, pret, all=T)
    
  #Melt again: strategic to anticipate 2nd Y axis
    both$trayvolm2<- both$trayvolm*1065.699 #Only a scale; back-calculated within the figure.
    bothm<- melt(both, id.vars = c('wsf','nozz','payload','distance'), 
                 measure.vars = c('trayvolm2','massm'))
    bothm$variable<- revalue(bothm$variable, c(massm="Glass slides",trayvolm2 = 'Trays'))
      
  #Fig 5. In-swath
    setwd(outfolder)
      g.ins<- ggplot(bothm, aes(x=distance, y=value, linetype=payload, color=variable, shape=variable)) + 
        geom_vline(xintercept = 0, size=.75, color="gray80")+geom_point() + geom_line() +
        facet_grid(paste(wsf, "m/s")~nozz) +theme_bw() + theme(text = element_text(size=9)) +
        scale_y_continuous(sec.axis = sec_axis(~./1065.699, name = "Tray volume (mL)"), labels=function(x) axisFormat(x))+
        scale_x_continuous(labels=function(x) axisMinus(x))+
        scale_linetype_discrete("Payload (L)") + 
        scale_color_manual("Substrate",values=c("chocolate3", "dodgerblue")) +
        scale_shape_manual ("Substrate", values=c(1,2)) +
        ylab(expression(Deposition~(ng/cm^2)~on~glass~slides)) + xlab("Distance (m)") 
      setwd(outfolder)
      ggsave(g.ins, file="g.ins3_RAug10.jpg", width=6.5, height=4.3)

  
# Fig 6, A2, A4, A5 and Table 2 -------------------------------------------------------------

  library(ggplot2)
  
  #Fig 6.  Overview of downwind drift deposition.___________________________
    temp<- main1
    temp$lab.pay<- revalue(temp$volume, c("2"="Payload = 2 L", "10"="Payload = 10 L"))
    g.asis.meas<- ggplot(temp, aes(x=distance, y=mass, color=as.factor(ws))) + geom_point(size=1) + 
      geom_line(aes(group=paste(ws, rep))) + ylab(expression(Deposition~(ng/cm^2))) +
      facet_grid(lab.pay ~ nozz) + xlab("Distance (m)") +theme_bw() +
      theme(panel.grid.minor=element_blank(),legend.position = "bottom") + 
      scale_color_manual("Wind speed (m/s)", values=c("black","dodgerblue","chocolate3")) +
      scale_y_log10(labels=function(x) axisFormat(x)) + scale_x_log10()+ annotation_logticks(sides="lb") 
    setwd(outfolder)
    ggsave(g.asis.meas, file="g.asis.meas_RAug10.jpg", width=6.5, height=4.5)
    

  #Fig A5.  Environmental conditions in the tunnel, by replicate ______________________
    #temperature
    g.tightT<- ggplot(main1, aes(x=paste(ws, " m/s, ", volume, " L", sep=""), y=temp.c, color=volume)) + 
      theme_bw() +  geom_point() + facet_wrap(~nozz, nrow=1, scales="free_x") + 
      ylab("Temperature (deg. C)") + xlab("Nominal wind speed, payload") + 
      geom_line(aes(group=paste(ws, volume))) + scale_color_manual(values=c("black","dodgerblue"))+
      theme(legend.position = "none", 
            axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5))
    #rh
    g.tightRH<- ggplot(main1, aes(x=paste(ws, " m/s, ", volume, " L", sep=""), y=rh, color=volume)) + 
      theme_bw() + geom_point() + facet_wrap(~nozz, nrow=1, scales="free_x") + 
      ylab("Relative humidity (%)") +  xlab("Nominal wind speed, payload") + 
      geom_line(aes(group=paste(ws, volume))) + scale_color_manual(values=c("black","dodgerblue"))+
      theme(legend.position = "none", 
            axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5))
    #wind speed 
    g.tightWS<- ggplot(main1, aes(x=paste(ws, " m/s, ", volume, " L", sep=""), y=ws.rel, color=volume)) + 
      theme_bw() + geom_hline(yintercept = 0, color="gray85", size=.5) +
      scale_y_continuous(labels=function(x) axisMinus(x))+
      geom_point() + facet_wrap(~nozz, nrow=1, scales="free_x") + 
      ylab("Relative wind speed (m/s)") +  xlab("Nominal wind speed, payload") + 
      geom_line(aes(group=paste(ws, volume))) + scale_color_manual(values=c("black","dodgerblue"))+
      theme(legend.position = "none", 
            axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5))
    setwd(outfolder)
    ggsave(g.tightT , file="g.tightT_RAug10.jpg" , width=6.5, height=3.3)
    ggsave(g.tightRH, file="g.tightRH_RAug10.jpg", width=6.4, height=3.3)
    ggsave(g.tightWS, file="g.tightWS_RAug10.jpg", width=6.47, height=3.3)
    

  #Fig A2.  Environmnental conditions are related to each other ______________________________
    p1<- ggplot(main1, aes(x=temp.c, y=rh)) + geom_point() + theme_bw()  + 
      ylab("Humidity (%)") + xlab("Temperature (deg. C)") + theme(text=element_text(size=9))
    p2<- ggplot(main1, aes(x=temp.c, y=ws.rel)) + geom_point() + theme_bw()  + 
      ylab("Relative wind speed (m/s)") + xlab("Temperature (deg. C)") + theme(text=element_text(size=9))+
      scale_y_continuous(labels=function(x) axisMinus(x))
    p3<- ggplot(main1, aes(x=rh, y=ws.rel)) + geom_point() + theme_bw()  + 
      xlab("Humidity (%)") + ylab("Relative wind speed (m/s)") + theme(text=element_text(size=9))+
      scale_y_continuous(labels=function(x) axisMinus(x))
    g.MetCorrs<- grid.arrange(p1, p2, p3, nrow=1)
    setwd(outfolder)
    ggsave(g.MetCorrs, file="g.MetCorrs_RAug10.jpg", width=6.5, height=2.1)
    

  #Table 2.  Summary of conditions in tunnel ______________________________________________
    temp<- melt(main1, id.vars = c("distance","volume","wsf","nozz","rep"), 
                measure.vars = c("temp.c","rh","ws.meas"))
    wtab<- temp %>%
      group_by(variable, wsf) %>%
      summarise (vmin = min(value), vmean = mean(value), vmax = max(value))
    wtab$variable<- revalue(wtab$variable, c("temp.c"="Temperature (deg. C)",
                                             "rh" = "Relative humidity (%)",
                                             "ws.meas" = "Measured wind speed (m/s)"))
    wtab

    
  #Fig A4.  Outlier ___________________________________________________________
    temp<- subset(main, distance>1.1)
    temp$nozz<- gsub("11001", "",temp$nozz)      
    g.outlier<- ggplot(temp, aes(x=distance, y=mass, color=as.factor(ws), shape=nozz)) + 
      geom_point() + theme_bw() +scale_y_log10(labels=function(x) axisFormat(x)) + scale_x_log10() + annotation_logticks(sides="lb") +
      theme(panel.grid.minor=element_blank()) + ylab(expression(Deposition~(ng/cm^2))) +
      scale_shape_manual("Nozzle",values=c(76, 84, 88)) + xlab("Distance (m)")+
      scale_color_manual("Wind speed (m/s)", values=c("black","dodgerblue","chocolate3"))
    setwd(outfolder)
    ggsave(g.outlier, file="g.outlier_RAug10.jpg", width=5.5, height=3)
    

# Table 3 and Fig 7.  Statistics on drift 2m+  ------------------------------------

  #Table 3.  Regression model
    fit<- lm(data=main1, log(mass) ~ log(distance)*wsf + nozz + log(distance)*volume +
                 r.t.w + ZBC )
    summary(fit); vif(fit, type="high-order") #

  #Table 3 cont. ANOVA to determine significance of full variables (all categories)
    anova(fit)
    
  #Residual plots
    hist(resid(fit))
    main1$predictions<- predict(fit)
    main1$resids<- resid(fit)
    
    minmin<- min(c(main1$mass, exp(main1$predictions)))
    maxmax<- max(c(main1$mass, exp(main1$predictions)))
    g.resid<- ggplot(main1, aes(x=mass, y=exp(predictions), color=wsf, shape=nozz)) + theme_bw() +
      geom_point(size=2) + geom_abline(intercept=0, slope = 1, color="gray50") + 
      xlab("Measured deposition (ng/cm2)") + ylab("Predicted deposition (ng/cm2)") +
      xlab(expression(Measured~deposition~(ng/cm^2))) + 
      ylab(expression(Predicted~deposition~(ng/cm^2))) +
      scale_shape_manual("Nozzle",values=c(76, 84, 88)) +
      scale_color_manual("Wind speed (m/s)", values=c("black","dodgerblue","chocolate3"))+
      scale_x_log10(limits=c(minmin, maxmax), labels=function(x) axisFormat(x))+ scale_y_log10(limits=c(minmin, maxmax), labels=function(x) axisFormat(x)) +
      annotation_logticks(sides="lb")
    setwd(outfolder)
    ggsave(g.resid, file="g.resid3_RAug10.jpg", width=5, height=2.8)
    

# Fig 8. Prediction graphs, part 1 -------------------------------------------------------

  #Create dataset upon which to make predictions using the regression
    main1$predictions<- predict(fit)
    new<- expand.grid(distance = unique(main1$distance),
                      nozz = unique(main1$nozz),
                      volume = unique(main1$volume),
                      wsf = unique(main1$wsf), 
                      r.t.w = c(round(quantile(main1$r.t.w, .05),3), round(mean(main1$r.t.w),3),
                                      round(quantile(main1$r.t.w, .95),3)),  
                      ZBC = c(round(quantile(main1$ZBC, .05),3), round(mean(main1$ZBC),3),
                              round(quantile(main1$ZBC, .95),3)))
    temp<- as.data.frame (exp(predict(fit, newdata=new, level=.95, interval="confidence")))
    new<- cbind(new, temp)
    new$r.t.w2<- as.factor(new$r.t.w)
    new$r.t.w2<- revalue(new$r.t.w2, c("-0.211"="Warm, dry, slow", "0.181"="Mean",
                                       "0.669"="Cool, moist, fast"))
    new$nozz2<- revalue(new$nozz, c("Lechler IDK  (coarse)"="Coarse", "TT (medium)"="Medium",
                                    "XR (fine)"="Fine" ))
    
    #color by nozzle
    g.pred.1noz<- ggplot(subset(new, wsf==3 & volume==2 & r.t.w==0.181 & ZBC==0), 
           aes(x=distance, y=fit, color=nozz2))+
      geom_ribbon(aes(ymin=lwr, ymax=upr, fill=nozz2), alpha=.2, color=NA) +
      geom_point(size=1) + geom_line() + theme_bw() + ggtitle("A. Nozzle") +
      ylab(expression(Deposition~(ng/cm^2))) +  xlab("Distance (m)") +
      scale_color_manual(NULL, values=c("chocolate4","chocolate3","tan")) +
      scale_fill_manual(NULL, values=c("chocolate4","chocolate3","tan")) +
      scale_x_log10(breaks=c(3,5,10,20)) + scale_y_log10(limits=c(29,9000),labels=function(x) axisFormat(x)) +annotation_logticks(sides="lb") +
      theme(legend.position = c(.7,.9),panel.grid.minor=element_blank(),
            text=element_text(size=8),legend.key.height = unit(.1,"cm"),
            plot.title=element_text(size=8), legend.background = element_rect(fill = NA))
    #color by wind speed
    g.pred.2ws<- ggplot(subset(new, nozz=="TT (medium)" & volume==2 & r.t.w==0.181 & ZBC==0), 
           aes(x=distance, y=fit, color=wsf)) +
      geom_ribbon(aes(ymin=lwr, ymax=upr, fill=wsf), alpha=.2, color=NA) +
      geom_point(size=1) + geom_line() + theme_bw() + ggtitle("B. Nominal wind speed (m/s)") +
      ylab(expression(Deposition~(ng/cm^2))) +  xlab("Distance (m)") +
      scale_color_manual(NULL, values = c('cyan2','cornflowerblue','navyblue')) +
      scale_fill_manual(NULL, values=c('cyan2','cornflowerblue','navyblue')) +
      scale_x_log10(breaks=c(3,5,10,20)) + scale_y_log10(limits=c(29,9000),labels=function(x) axisFormat(x)) +annotation_logticks(sides="lb") +
      theme(legend.position = c(.77,.9),panel.grid.minor=element_blank(),
            text=element_text(size=8),legend.key.height = unit(.1,"cm"),
            plot.title=element_text(size=8), legend.background = element_rect(fill = NA))
    #color by payload
    g.pred.3vol<- ggplot(subset(new, wsf==3 & nozz=="TT (medium)" & r.t.w==0.181 & ZBC==0), 
           aes(x=distance, y=fit, color=volume)) +
      geom_ribbon(aes(ymin=lwr, ymax=upr, fill=volume), alpha=.2, color=NA) +
      geom_point(size=1) + geom_line() + theme_bw() + ggtitle("C. Payload (L)") +
      ylab(expression(Deposition~(ng/cm^2))) +  xlab("Distance (m)")  +
      scale_color_manual(NULL, values=c("olivedrab3","green4")) +
      scale_fill_manual(NULL, values=c("olivedrab3","green4")) +
      scale_x_log10(breaks=c(3,5,10,20)) + scale_y_log10(limits=c(29,9000),labels=function(x) axisFormat(x)) +annotation_logticks(sides="lb") +
      theme(legend.position = c(.77,.92),panel.grid.minor=element_blank(),
            text=element_text(size=8),legend.key.height = unit(.1,"cm"),
            plot.title=element_text(size=8), legend.background = element_rect(fill = NA))
    #color by meteorological variable
    g.pred.4met<- ggplot(subset(new, nozz=="TT (medium)" & wsf==3 & volume==2 & ZBC==0), 
           aes(x=distance, y=fit, color=r.t.w2)) +
      geom_ribbon(aes(ymin=lwr, ymax=upr, fill=r.t.w2), alpha=.2, color=NA) +
      geom_point(size=1) + geom_line() + theme_bw() + ggtitle("D. Meteorology interaction variable") +
      ylab(expression(Deposition~(ng/cm^2))) +  xlab("Distance (m)") +
      scale_color_manual(NULL,values = c('gray80','gray45','black')) +
      scale_fill_manual(NULL, values=c('gray80','gray45','black')) +
      scale_x_log10(breaks=c(3,5,10,20)) + scale_y_log10(limits=c(29,9000),labels=function(x) axisFormat(x)) +annotation_logticks(sides="lb") +
      theme(legend.position = c(.68,.9),panel.grid.minor=element_blank(),
            text=element_text(size=8),legend.key.height = unit(.1,"cm"),
            plot.title=element_text(size=8), legend.background = element_rect(fill = NA))

  #CONTINUE TO NEXT SECTION FOR ZBC GRAPH.  It had to be done on a different set to allow
    #for subsetting on ZBC==0 above, but to show relevant variation within distance below.
    
  #to quantify the differences in paragraphs
    prob<- subset(new, wsf==3 & volume==2 & r.t.w==0.181 & ZBC==0 & distance==2) #nozz
    prob<- subset(new, nozz=="TT (medium)" & volume==2 & r.t.w==0.181 & ZBC==0) #wsf
    prob<- subset(new, wsf==3 & nozz=="TT (medium)" & r.t.w==0.181 & ZBC==0) #volume
    prob<- subset(new, nozz=="TT (medium)" & wsf==3 & volume==2 & ZBC==0 & distance==2) #r.t.w
    
  #highest and lowest at 20m, text in paragraph
    prob.hi<- subset(new, wsf==4.5 & nozz=="XR (fine)" & volume==10 & r.t.w==0.181 & ZBC==0 & distance==2)
    prob.hi$fit
    prob.lo<- subset(new, wsf==1.5 & nozz=="Lechler IDK  (coarse)" & volume==2 & r.t.w==0.181 & ZBC==0 & distance==2)
    prob.lo$fit
    prob.hi$fit / prob.lo$fit
    
   
# Fig 8. Prediction graphs, part 2 -------------------------------------------------------

  #Bring in ZBC, but vary percentiles by distance
    tempz<- main1 %>%
      group_by (distance) %>%
      summarise (z5th = quantile(ZBC,.05),
                 zmn = mean(ZBC),
                 z95th = quantile(ZBC,.95))
    tempzm<- reshape2::melt(tempz, id.vars = "distance", value.name = "ZBC") #error is fine
    tempzm$Zrange<- revalue (tempzm$variable, c(z5th="5th percentile", zmn="Mean",
                                               z95th="95th percentile"))
    tempzm<- subset(tempzm, select=-variable); rm(tempz)

  #Create dataset upon which to make predictions
    new<- expand.grid(distance = unique(main1$distance),
                      nozz = unique(main1$nozz),
                      volume = unique(main1$volume),
                      wsf = unique(main1$wsf), 
                      r.t.w = c(round(quantile(main1$r.t.w, .05),3), round(mean(main1$r.t.w),3),
                                      round(quantile(main1$r.t.w, .95),3)))
    
  #Merge with distance-specific ZBCs, predict, label
    new<- merge(new, tempzm, all=T)
    temp<- as.data.frame (exp(predict(fit, newdata=new, level=.95, interval="confidence")))
    new<- cbind(new, temp);   rm(tempzm, temp)
    new$r.t.w2<- as.factor(new$r.t.w)
    new$r.t.w2<- revalue(new$r.t.w2, c("-0.211"="Warm, dry, slow", "0.181"="Mean",
                                       "0.669"="Cool, moist, fast"))
    new$nozz2<- revalue(new$nozz, c("Lechler IDK  (coarse)"="Coarse", "TT (medium)"="Medium",
                                    "XR (fine)"="Fine" ))
    
    #color by ZBC
    g.pred.5z<- ggplot(subset(new, nozz=="TT (medium)" & wsf==3 & volume==2 & r.t.w==0.181), 
           aes(x=distance, y=fit, color=Zrange)) +
      geom_ribbon(aes(ymin=lwr, ymax=upr, fill=Zrange), alpha=.2, color=NA) +
      geom_point(size=1) + geom_line() + theme_bw() + ggtitle("E. 3D anemometer variable") +
      ylab(expression(Deposition~(ng/cm^2))) +  xlab("Distance (m)") +
      scale_color_manual(NULL,values = c("thistle","plum3","plum4")) + 
      scale_fill_manual(NULL, values=c("thistle","plum3","plum4")) +
      scale_x_log10(breaks=c(3,5,10,20)) + scale_y_log10(limits=c(29,13900),labels=function(x) axisFormat(x)) +annotation_logticks(sides="lb") +
      theme(legend.position = c(.68,.9),panel.grid.minor=element_blank(),
            text=element_text(size=8),legend.key.height = unit(.1,"cm"),
            plot.title=element_text(size=8), legend.background = element_rect(fill = NA))
    
  #FINAL COMPILATION to produce Fig 8.
    setwd(outfolder)
    g.arranged<- grid.arrange(g.pred.1noz, g.pred.2ws, g.pred.3vol,
                              g.pred.4met, g.pred.5z, nrow=2)
    setwd(outfolder)
    ggsave(g.arranged, file="g.pred5f_RAug10.jpg", width=6.67, height=5.2)


# Figs 10, A6.  Residual improvement ---------------------------------
  
  #Regression with and without 3D anemometer data
    fit<- lm(data=main1, log(mass) ~ log(distance)*wsf + nozz + log(distance)*volume +
                 r.t.w + ZBC )
    fitx<- lm(data=main1, log(mass) ~ log(distance)*wsf + nozz + log(distance)*volume + r.t.w)

  #Residuals
    main1$res.wo<- resid(fitx)
    main1$res.with<- resid(fit)
    
  #Structure, label for graphing
    temp<- melt(main1, id.vars = c('nozz','wsf','volume','distance'),
                measure.vars = c("res.wo","res.with"), value.name=("residuals"))
    temp$variable<- revalue(temp$variable, c("res.wo"="Without 3D variable", 
                                             "res.with"="Including 3D variable"))
    temp$lab.pay<- revalue(temp$volume, c("2"="Payload = 2 L", "10"="Payload = 10 L"))
    
  #Fig 10.
    g.residdist<- ggplot(temp, aes(x=distance, y=residuals, color=variable)) +
      facet_grid(lab.pay ~ paste(wsf, "m/s")) +
      geom_hline(yintercept = 0) + geom_point(shape=1) + geom_smooth(method="loess", se=F) +
      scale_y_continuous(labels=function(x) axisMinus(x))+
      scale_color_manual("Regression version", values=c("gray65","blue4"))+
      theme_bw() + ylab(expression(Residuals~(ln(ng/cm^2)))) + xlab("Distance (m)") +
      theme(legend.position = "bottom")
    setwd(outfolder)
    ggsave(g.residdist, file="g.residdist_RAug10.jpg", width=6.5, height=4.75)
      
    
  #Fig A6.  Residuals vs. fitted shapes _________________________
    #Without ZBC
      temp<- main1
      temp$predictions.wo<- predict(fitx)
      temp$predictions.with<- predict(fit)
      
      minmin<- min(c(temp$res.wo, temp$res.with))
      maxmax<- max(c(temp$res.wo, temp$res.with))
      p1<- ggplot(temp, aes(x=predictions.wo, y=res.wo, color=wsf, shape=nozz)) + theme_bw() +
        geom_point(size=2) + geom_hline(yintercept=0, color="gray55", linetype=2) + 
        xlab(expression(Predicted~deposition~(ln(ng/cm^2)))) + 
        ylab(expression(Residuals~(ln(ng/cm^2)))) + 
        scale_y_continuous(limits=c(minmin, maxmax),labels = function(x) axisMinus(x)) +
        scale_shape_manual("Nozzle",values=c(76, 84, 88)) + ggtitle("Without 3D anemometer variable") +
        scale_color_manual("Wind speed (m/s)", values=c("black","dodgerblue","chocolate3")) +
        geom_smooth(aes(group=1), se=F, color="forestgreen") +
        theme(legend.position = "none", text=element_text(size=9))
      
    #Full, ZBC included
      p2<- ggplot(temp, aes(x=predictions.with, y=res.with, color=wsf, shape=nozz)) + theme_bw() +
        geom_point(size=2) + geom_hline(yintercept=0, color="gray55", linetype=2) + 
        xlab(expression(Predicted~deposition~(ln(ng/cm^2)))) + 
        ylab(expression(Residuals~(ln(ng/cm^2)))) + 
        scale_y_continuous(limits=c(minmin, maxmax),labels = function(x) axisMinus(x)) +
        scale_shape_manual("Nozzle",values=c(76, 84, 88)) + ggtitle("With 3D anemometer variable") +
        scale_color_manual("Wind speed (m/s)", values=c("black","dodgerblue","chocolate3")) +
        geom_smooth(aes(group=1), se=F, color="forestgreen") +
        theme(legend.position = "none", text=element_text(size=9))

      g.resImprv_legendonly<-  ggplot(temp, aes(x=predictions.with, y=res.with, color=wsf, shape=nozz)) +
        geom_point(size=2) + scale_shape_manual("Nozzle",values=c(76, 84, 88)) + theme_bw() +
        scale_color_manual("Wind speed (m/s)", values=c("black","dodgerblue","chocolate3")) +
        theme(legend.position = "bottom", text=element_text(size=9))
      
      g.resImprv <- grid.arrange(p1, p2, nrow=1)
      setwd(outfolder)
      ggsave(g.resImprv, file="g.resImprv2_RAug10.jpg", width=6.5, height=2.6)
      ggsave(g.resImprv_legendonly, file="g.resImprv_legendonly_RAug10.jpg", width=6.5, height=1)
    
      
# Fig A7.  Examples of curve alongside points -------------------------------------------

  #Label three examples
    main1$examples<- ifelse(main1$nozz=="XR (fine)" & main1$wsf=="4.5" & main1$volume==10,
                            "High scenario: Fine nozzle,\nfast speed, full payload", NA)
    main1$examples [main1$nozz=="TT (medium)" & main1$wsf=="3" & main1$volume==10] <- 
      "Medium scenario: Medium nozzle,\nmedium speed, full payload"
    main1$examples [main1$nozz=="Lechler IDK  (coarse)" & main1$wsf=="1.5" & main1$volume==2] <- 
      "Low scenario: Coarse nozzle,\nlow speed, near-empty payload"
    main1$examples<- factor(main1$examples, 
                            levels=c("High scenario: Fine nozzle,\nfast speed, full payload",
                                     "Medium scenario: Medium nozzle,\nmedium speed, full payload",
                                     "Low scenario: Coarse nozzle,\nlow speed, near-empty payload"))
  #Subset
    exs<- subset(main1, !is.na(examples))
    
  #Fig A7.  Predictions and actual points together
    p1<- ggplot(exs, aes(x=distance, y=mass, color=rep)) + geom_point() + 
      geom_point(aes(y=exp(predictions)), shape=24) + geom_line(aes(y=exp(predictions))) +
      theme_bw()+ facet_wrap(~examples, ncol=1)+ 
      scale_y_continuous(labels=function(x) axisFormat(x))+
      scale_color_manual("Replicate", values=c("black","dodgerblue", "chocolate3")) +
      ylab(expression(Deposition~(ng/cm^2)~standard~scale)) + 
      xlab("Distance (m) standard scale") +theme(legend.position = "none")
    p2<- p1 + scale_y_log10(labels=function(x) axisFormat(x)) + scale_x_log10(breaks=c(3,5,10,20)) + annotation_logticks(sides = "lb") +
      ylab(expression(Deposition~(ng/cm^2)~log~scale)) + xlab("Distance (m) log scale") 
    p3<- p1  +theme(legend.position = "bottom")
    
    g.examples<- grid.arrange(p2, p1, nrow=1)
    setwd(outfolder)
    ggsave(g.examples, file="g.examples2_RAug10.jpg", width=6.5, height=7)
    ggsave(p3, file="g.examplesLegend_RAug10.jpg", width=6.5, height=3)
            