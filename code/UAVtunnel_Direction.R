#Accompanying: "A wind-tunnel assessment of parameters that may impact spray drift during
#UAV pesticide application" by Shanique Grant et al.
#Description: This file creates figures of wind tunnels. The calculation of the XZ hypoteneouse and conversion from meteorological plan to math plan are also done here.
#Sections beloware labeled by the figures, tables, and analyses they include.
#Last saved: 2022-07-08
#Author: Brenna Kent

# Library imports and paths ---------------------------------------------------------

library(ggplot2)
library(dplyr)
library(NISTunits)
library(reshape2)


# Should be updated to user folders
infolder<-"PATH TO DATA"
outfolder<- "PATH TO WHERE PLOTS SHOULD BE SAVED"
setwd(infolder)

#Import & prep direction -----------------------------------------------


# read in 1 min average file of directions
dir<- read.csv("3D_Anemometer_Direction.csv")

# reshaping data to the correct format
colnames(dir)<- tolower(colnames(dir))
dir<- reshape::rename(dir, c(distance_m = "distance", payload="volume", nom_ws="ws"))

#factors
dir$width<- as.factor(dir$width)
dir$width<- factor(dir$width, levels=c("0.99", "2.1",  "3.3"),
                   labels=c("L","C","R"))
dir$width<- factor(dir$width, levels=c("L","C","R"))
dir$height<- as.factor (substr(dir$location, 1,1))

# subset upwind and downwind in case we want to compare separately
dir_p<- subset(dir, distance>0, select=-c(source, location, h_m, vel_dir))
dir_n<- subset(dir, distance<=0, select=-c(source, location, h_m, vel_dir))


#Future plotting will need radians instead of degrees
dir_p$radians<- NISTdegTOradian(dir_p$direction.deg)
dir_n$radians<- NISTdegTOradian(dir_n$direction.deg)


#convert from meteorological plane to math plane
dir_p$direction.rev<-360-dir_p$direction.deg
dir_n$direction.rev<- 360-dir_n$direction.deg
      
     
#Future plotting will need radians instead of degrees  
dir_p$radians.rev<- NISTdegTOradian(dir_p$direction.rev)
dir_n$radians.rev<- NISTdegTOradian(dir_n$direction.rev)
      


# Import & prep X, Y, Z dimension speeds ----------------------------------

# reading in 1 min average 3D anemometer data for the 2L and 10L payloads
ane2<- read.csv("3D_Anemometer_2L.csv")
ane10<- read.csv("3D_Anemometer_10L.csv")
ane2$vol <- 2
ane10$vol<- 10
ane<- rbind(ane2, ane10);  rm(ane2, ane10)

# reshaping data to correct format
colnames(ane)<- tolower(colnames(ane))
ane<- reshape::rename(ane, c(distance_m = "distance", vol="volume", nom_ws="ws")) #, velocity_ms = "anemom"))

#factors
ane$width<- as.factor(ane$width)
ane$width<- factor(ane$width, levels=c("0.99", "2.1",  "3.3"),
                   labels=c("L","C","R"))
ane$width<- factor(ane$width, levels=c("L","C","R"))
ane$height<- as.factor (substr(ane$location, 1,1))
ane$vel_dir<- as.factor(ane$vel_dir)
ane_p<- subset(ane, distance>0, 
               select=-c(source, velocity_mph, vel_dir_label, location, h_m))

ane_n<- subset(ane, distance<=0, 
               select=-c(source, velocity_mph, vel_dir_label, location, h_m))



#cast speeds separately
ane_p$vel_dir<- paste0(ane_p$vel_dir,"speed")
ane_n$vel_dir<- paste0(ane_n$vel_dir,"speed")

# again formatting table
anec_p<- dcast(ane_p, height+width+distance+ws+volume ~ vel_dir, value.var="velocity_ms")
anec_n<- dcast(ane_n, height+width+distance+ws+volume ~ vel_dir, value.var="velocity_ms")
rm(ane_p, ane_n)

# Merge degrees & X, Y, Z speed; export ------------------------------------------------

both_p<- merge(dir_p, anec_p, all=T)
both_p<- subset(both_p, !is.na(Xspeed)) #NA at Top Center distance 1.
both_n<- merge(dir_n, anec_n, all=T)
both_n<- subset(both_n, !is.na(Xspeed)) #NA at Top Center distance 1.
# setwd(outfolder)
# write.csv(both, file="3D_SpeedAndDirec.csv", na="", row.names = F)


# Fig. 9A and A8A  --------------------------------------------------

#combining upwind and downwind
both<-rbind(both_n, both_p)
# calculating combined xy velocity
both$XYSpeed<-sqrt(both$Xspeed^2+both$Yspeed^2)

# getting limits for plotting
min15<-both %>% 
  filter( ws==1.5) %>% 
  select(XYSpeed) %>% 
  min()

min3<-both %>% 
  filter( ws==3.0) %>% 
  select(XYSpeed) %>% 
  min()

min45<-both %>% 
  filter( ws==4.5) %>% 
  select(XYSpeed) %>% 
  min()

max15<-both %>% 
  filter(ws==1.5) %>% 
  select(XYSpeed) %>% 
  max()

max3<-both %>% 
  filter( ws==3.0) %>% 
  select(XYSpeed) %>% 
  max()

max45<-both %>% 
  filter( ws==4.5) %>% 
  select(XYSpeed) %>% 
  max()


# Creating plots in a loop
for(wsV in c(1.5,3.0, 4.5)){
  for(vV in c(2,10)){
    
    # setting limits for plots
    if(wsV == 1.5){
      legendLimits = c(min15,max15)
    }
    if(wsV == 3.0){
      legendLimits = c(0.5,max3)
    }
    if(wsV == 4.5){
      legendLimits = c(min45,max45)
    }
    
    #subset plotting data
    plotData<- subset(both, ws==wsV & volume==vV)
    
    plot<- ggplot(plotData, aes(x=distance, y=factor(width, levels = c("L", "C", "R"), labels = c("Left", "Center", "Right")), radius=.4)) + 
      geom_tile(aes(fill=XYSpeed), color="white") +  
      scale_fill_gradient2(low="dodgerblue3", high="orange", mid="white", midpoint=wsV, limits = legendLimits) +
      geom_spoke(aes(angle=radians.rev), arrow = arrow(length= unit(.035,'inches'), type ="open"), size = 0.45) + 
      coord_equal(expand=0) +
      facet_grid(factor(height, levels = c("T", "M", "B"), labels = c("Top", "Middle", "Bottom"))~.) + ggtitle ("Directions ") +
      geom_text(aes(label=round(XYSpeed,1)), vjust = 0, nudge_y = -0.3, color="gray35", size=2.0)+
      ggtitle("A. Tunnel area at three heights", subtitle =paste0("Wind speed (m/s): ", unique(plotData$ws), "     Payload (L): ", unique(plotData$volume)))+
      labs(fill = "XY velocity\n (m/s)")+
      xlab("Distance (m)") + ylab("Width")+
      theme_bw()+
      theme(legend.position = "bottom", legend.direction = "horizontal", text = element_text(size = 8), legend.text = element_text(size=8), legend.title=element_text(size=8),
            plot.margin=grid::unit(c(0,0,0,0), "mm"),plot.subtitle = element_text(color='gray35', size=8), plot.title = element_text(size=8))
    
    wsT<-ifelse(wsV==1.5,"1_5",ifelse(wsV==3.0,"3","4_5"))
    
    #saving file
    fileName <- paste0(outfolder,'/windPlot_DV_', wsT, '_',toString(unique(plotData$volume)), ".png" )
    ggsave(fileName ,width = 4.9, height = 3.1)
    
    
  }
  
  
}


# Fig. 9B and A8B (2m Cross section plots) --------------------------------------------------

# calculating direction between z and y velocities
both$XZDegree<-ifelse(NISTradianTOdeg(atan2(both$Zspeed, both$Xspeed))<0,
                      NISTradianTOdeg(atan2(both$Zspeed, both$Xspeed))+360, NISTradianTOdeg(atan2(both$Zspeed, both$Xspeed)) )
both$XZRadian<-atan2(both$Zspeed, both$Xspeed)


#converting from meteorological plane to math plane
both$XZDegree.rev<-ifelse(both$XZDegree<=180,180-both$XZDegree, 540-both$XZDegree )

# plot needs radians not degrees
both$XZDegree.rev.radian<- NISTdegTOradian(both$XZDegree.rev)


# calculating combinded velocity in the x and z directions
both$XZSpeed<-sqrt(both$Xspeed^2+both$Zspeed^2)




# getting limits for plotting
min15<-both %>% 
  filter(distance ==2, ws==1.5) %>% 
  select(XZSpeed) %>% 
  min()

min3<-both %>% 
  filter(distance ==2, ws==3.0) %>% 
  select(XZSpeed) %>% 
  min()

min45<-both %>% 
  filter(distance ==2, ws==4.5) %>% 
  select(XZSpeed) %>% 
  min()

max15<-both %>% 
  filter(distance ==2, ws==1.5) %>% 
  select(XZSpeed) %>% 
  max()

max3<-both %>% 
  filter(distance ==2, ws==3.0) %>% 
  select(XZSpeed) %>% 
  max()

max45<-both %>% 
  filter(distance ==2, ws==4.5) %>% 
  select(XZSpeed) %>% 
  max()


#loop to plot all figures
for(wsV in c(1.5,3.0, 4.5)){
  for(vV in c(2,10)){
    
    
    # setting limits for plots
    if(wsV == 1.5){
      legendBreaks = c(0.4, 0.8, 1.2, 1.6)
      legendLimits = c(min15,max15)
    }
    if(wsV == 3.0){
      legendBreaks = c(0.5, 1.5, 2.5, 3.0)
      legendLimits = c(0.5,max3)
    }
    if(wsV == 4.5){
      legendBreaks = c(1.5, 3.0,  4.5, 6.0 )
      legendLimits = c(min45,max45)
    }
    
    # creating plot
    plot<-both %>% 
      filter (distance ==2, ws==wsV, volume==vV) %>% #subset data
      ggplot(aes(x=factor(width, levels = c("L", "C", "R"), labels = c("Left", "Center", "Right")), y=factor(height, levels = c("B", "M", "T"), labels = c("Bottom", "Middle", "Top")), radius=.2))+
      geom_tile(aes(fill=XZSpeed), color="white") +   
      scale_fill_gradient(low="white", high="forestgreen", guide=guide_colourbar(reverse ="TRUE"), breaks= legendBreaks, limits = legendLimits)+
      geom_spoke(aes(angle=XZDegree.rev.radian), arrow = arrow(length= unit(.035,'inches'), type ="open"), size = 0.7)+
      geom_text(aes(label = round(XZSpeed,1)), color="gray35", size=2.5, vjust = 0, nudge_y = -0.3,)+ 
      labs(fill = "XZ velocity (m/s)")+
      xlab("Width") + ylab("Height")+ 
      ggtitle("B. Cross-section\n     at 2 m")+
      theme_bw()+
      theme(legend.direction = "vertical", legend.justification =  "center", text = element_text(size = 8), legend.text = element_text(size=8),
            legend.position = "bottom", axis.text.y = element_text(angle = 90, hjust=0.5),legend.key.size=unit(0.4,"cm") , plot.title = element_text(size=8),
            legend.title=element_text(size=8), plot.margin=grid::unit(c(0,0,0,0), "mm"))
    
    wsT<-ifelse(wsV==1.5,"1_5",ifelse(wsV==3.0,"3","4_5"))
    
    # saving file
    fileName <- paste0(outfolder,'/WindCrossSection_', wsT, '_',toString(vV), ".png" )
    ggsave(fileName, width = 1.4, height = 3.1)
    
  }
}







