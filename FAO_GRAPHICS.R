require(ggplot2)

#########################################
#EMMISSIONS (CATTLE)
#########################################
FAO <- read.csv("E:/Dropbox/Dropbox/Paper_PhD/FAO/FAOSTAT_data_5-28-2018_SHEEP.csv",header = T,sep=",")

FAO_data <- as.data.frame(cbind(as.character(FAO$Area),
                                as.character(FAO$Year),
                                FAO$Value
                                ))
colnames(FAO_data) <- c("Region","Year","Value")

FAO_data$Year <- as.character(FAO$Year)

#FAO_data$Year <- year(as.Date(FAO_data$Year,"%Y")) #it will gives only years Hope it helps to you! :)


FAO_data$Year <- as.numeric(as.character(FAO_data$Year))
FAO_data$Value <- as.numeric(as.character(FAO_data$Value))

#class(FAO_data$Region)
#FAO_data <- FAO_data[which(FAO_data$Region=="World"),]
FAO_data <- FAO_data[which(FAO_data$Region != "Eastern Africa"),]
FAO_data <- FAO_data[which(FAO_data$Region != "Middle Africa"),]
FAO_data <- FAO_data[which(FAO_data$Region != "Northern Africa"),]
FAO_data <- FAO_data[which(FAO_data$Region != "Western Africa"),]
FAO_data <- FAO_data[which(FAO_data$Region != "Southern Africa"),]
FAO_data <- FAO_data[which(FAO_data$Region != "Northern America"),]
FAO_data <- FAO_data[which(FAO_data$Region != "Central America"),]
FAO_data <- FAO_data[which(FAO_data$Region != "Caribbean"),]
FAO_data <- FAO_data[which(FAO_data$Region != "South America"),]
FAO_data <- FAO_data[which(FAO_data$Region != "Central Asia"),]
FAO_data <- FAO_data[which(FAO_data$Region != "Eastern Asia"),]
FAO_data <- FAO_data[which(FAO_data$Region != "Southern Asia"),]
FAO_data <- FAO_data[which(FAO_data$Region != "South-Eastern Asia"),]
FAO_data <- FAO_data[which(FAO_data$Region != "Western Asia"),]
FAO_data <- FAO_data[which(FAO_data$Region != "Eastern Europe"),]
FAO_data <- FAO_data[which(FAO_data$Region != "Northern Europe"),]
FAO_data <- FAO_data[which(FAO_data$Region != "Southern Europe"),]
FAO_data <- FAO_data[which(FAO_data$Region != "Western Europe"),]
FAO_data <- FAO_data[which(FAO_data$Region != "Australia & New Zealand"),]
FAO_data <- FAO_data[which(FAO_data$Region != "Melanesia"),]
FAO_data <- FAO_data[which(FAO_data$Region != "Polynesia"),]
FAO_data <- FAO_data[which(FAO_data$Region != "Ireland"),]

p<-ggplot(FAO_data, aes(x=Year, y=Value,colour=Region, group = Region)) +
 # geom_path( position = "identity") +
  geom_line(size=4)+
#  geom_point()+
  xlab("Year")+
#  ylab("Gigagrams of CH4") +
  
  
  ylab(expression("Gigagrams of CH"[4])) +
  
  #scale_colour_brewer(type = "qual", palette = 8) +
  scale_colour_brewer(type = "qual", palette = "Set1")+
  scale_x_continuous(breaks=seq(1961,2016,1),limits = c(1961,2016))+
  scale_y_continuous(breaks=seq(0,9000,250),limits = c(0,9000))+
        theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),
        axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),
        legend.title=element_blank(),
        legend.box = "horizontal",
       # legend.text=element_text(size=rel(0.8)),
        legend.title = element_text(size=60), 
         legend.text = element_text(size=60),
       legend.key.height = unit(5, "cm"),
       legend.key.width = unit(5, "cm")
       
        )

  
  #+
  #geom_point(aes(color=Region))
#p


ggsave(paste0("E:/Dropbox/Dropbox/Paper_PhD/FAO","/","LINE_EMMISIONS_SHEEP",".pdf"),p,dpi=600,width =90,height=60,units = "cm",scale=1.2,limitsize = FALSE)
#ggsave(paste0("E:/Dropbox/Dropbox/Paper_PhD/FAO","/","LINE_PLOT",".png"),p,dpi=600,width =90,height=60,units = "cm",scale=1.2,limitsize = FALSE)

#########################################
#HEAD (CATTLE)
#########################################
FAO2 <- read.csv("E:/Dropbox/Dropbox/Paper_PhD/FAO/FAOSTAT_data_5-31-2018_HEADS.csv",header = T,sep="|")
FAO2 <- FAO2[which(FAO2$Item=="Sheep"),]
FAO_data_H <- as.data.frame(cbind(as.character(FAO2$Area),
                                  as.character(FAO2$Year),
                                  FAO2$Value
))
colnames(FAO_data_H) <- c("Region","Year","Value")

FAO_data_H$Year <- as.character(FAO2$Year)

#FAO_data_H$Year <- year(as.Date(FAO_data_H$Year,"%Y")) #it will gives only years Hope it helps to you! :)


FAO_data_H$Year <- as.numeric(as.character(FAO_data_H$Year))
FAO_data_H$Value <- as.numeric(as.character(FAO_data_H$Value))

#class(FAO_data_H$Region)
#FAO_data_H <- FAO_data_H[which(FAO_data_H$Region=="World"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "Eastern Africa"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "Middle Africa"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "Northern Africa"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "Western Africa"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "Southern Africa"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "Northern America"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "Central America"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "Caribbean"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "South America"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "Central Asia"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "Eastern Asia"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "Southern Asia"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "South-Eastern Asia"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "Western Asia"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "Eastern Europe"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "Northern Europe"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "Southern Europe"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "Western Europe"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "Australia & New Zealand"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "Melanesia"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "Polynesia"),]
FAO_data_H <- FAO_data_H[which(FAO_data_H$Region != "Ireland"),]


FAO_data_H$Value <- FAO_data_H$Value/1000000


p2<-ggplot(FAO_data_H, aes(x=Year, y=Value,colour=Region, group = Region)) +
  # geom_path( position = "identity") +
  geom_line(size=4)+
  #  geom_point()+
  xlab("Year")+
  #  ylab("Gigagrams of CH4") +
  
  
  ylab("Million of head") +
  
  
  #scale_colour_brewer(type = "qual", palette = 8) +
  scale_colour_brewer(type = "qual", palette = "Set1")+
  scale_x_continuous(breaks=seq(1961,2016,1),limits = c(1961,2016))+
  scale_y_continuous(breaks=seq(0,1300,100),limits = c(0,1300))+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),
        axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),
        legend.title=element_blank(),
        legend.box = "horizontal",
        # legend.text=element_text(size=rel(0.8)),
        legend.title = element_text(size=60), 
        legend.text = element_text(size=60),
        legend.key.height = unit(5, "cm"),
        legend.key.width = unit(5, "cm")
        
  )


#+
#geom_point(aes(color=Region))
#p


ggsave(paste0("E:/Dropbox/Dropbox/Paper_PhD/FAO","/","LINE_HEAD_SHEEP",".pdf"),p2,dpi=600,width =90,height=60,units = "cm",scale=1.2,limitsize = FALSE)
#ggsave(paste0("E:/Dropbox/Dropbox/Paper_PhD/FAO","/","LINE_PLOT",".png"),p,dpi=600,width =90,height=60,units = "cm",scale=1.2,limitsize = FALSE)


#########################################
#EMMISION PER HEAD
#########################################

FAO_MERGE <- as.data.frame(merge(FAO_data,FAO_data_H,by=c("Region","Year")))
FAO_MERGE$Em_Head <- NA
FAO_MERGE$Value.x <- FAO_MERGE$Value.x/1e-6
FAO_MERGE$Value.y <- FAO_MERGE$Value.y*1e6
FAO_MERGE$Em_Head <- FAO_MERGE$Value.x/ FAO_MERGE$Value.y
  



p3<-ggplot(FAO_MERGE, aes(x=Year, y=Em_Head, colour=Region, group = Region)) +
  # geom_path( position = "identity") +
  geom_line(size=4)+
  #  geom_point()+
  xlab("Year")+
  #  ylab("Gigagrams of CH4") +
  
  
  ylab(expression("Kg of CH"[4] / head)) +
         #labs(y=expression("Seasonal Flux (kg " *N[2]*"O-N/ha)"))
       
  #scale_colour_brewer(type = "qual", palette = 8) +
  scale_colour_brewer(type = "qual", palette = "Set1")+
  scale_x_continuous(breaks=seq(1961,2016,1),limits = c(1961,2016))+
 scale_y_continuous(breaks=seq(5,8,0.05),limits = c(5,8))+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),
        axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),
        legend.title=element_blank(),
        legend.box = "horizontal",
        # legend.text=element_text(size=rel(0.8)),
        legend.title = element_text(size=60), 
        legend.text = element_text(size=60),
        legend.key.height = unit(5, "cm"),
        legend.key.width = unit(5, "cm")
        
  )


#+
#geom_point(aes(color=Region))
#p


ggsave(paste0("E:/Dropbox/Dropbox/Paper_PhD/FAO","/","LINE_EM_HEAD_SHEEP",".pdf"),p3,dpi=600,width =90,height=60,units = "cm",scale=1.2,limitsize = FALSE)
  #ggsave(paste0("E:/Dropbox/Dropbox/Paper_PhD/FAO","/","LINE_PLOT",".png"),p,dpi=600,width =90,height=60,units = "cm",scale=1.2,limitsize = FALSE)


#########################################
#VALUE $$$ IMPORT
#########################################
FAO3 <- read.csv("E:/Dropbox/Dropbox/Paper_PhD/FAO/FAOSTAT_data_5-31-2018_TRADE.csv",header = T,sep=",")
FAO3 <- FAO3[which(FAO3$Item=="Sheep"),]
FAO3 <- FAO3[which(FAO3$Element=="Import Value"),]

FAO_data_TA <- as.data.frame(cbind(as.character(FAO3$Country),
                                   as.numeric(as.character(FAO3$Year)),
                                   as.numeric(as.character(FAO3$Value))
))
colnames(FAO_data_TA) <- c("Region","Year","Value")

FAO_data_TA$Year <- as.character(FAO3$Year)
FAO_data_TA$Year <- as.numeric(as.character(FAO_data_TA$Year))
FAO_data_TA$Value <- as.numeric(as.character(FAO_data_TA$Value))




#FAO_data_TA$Year <- year(as.Date(FAO_data_TA$Year,"%Y")) #it will gives only years Hope it helps to you! :)




#class(FAO_data_TA$Region)
#FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region=="World"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "Eastern Africa"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "Middle Africa"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "Northern Africa"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "Western Africa"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "Southern Africa"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "Northern America"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "Central America"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "Caribbean"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "South America"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "Central Asia"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "Eastern Asia"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "Southern Asia"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "South-Eastern Asia"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "Western Asia"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "Eastern Europe"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "Northern Europe"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "Southern Europe"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "Western Europe"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "Australia & New Zealand"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "Melanesia"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "Polynesia"),]
FAO_data_TA <- FAO_data_TA[which(FAO_data_TA$Region != "Ireland"),]


FAO_data_TA$Value <- FAO_data_TA$Value*1000; FAO_data_TA$Value <- FAO_data_TA$Value/1000000


p4<-ggplot(FAO_data_TA, aes(x=Year, y=Value,colour=Region, group = Region)) +
  # geom_path( position = "identity") +
  geom_line(size=4)+
  #  geom_point()+
  xlab("Year")+
  #  ylab("Gigagrams of CH4") +
  
  
  ylab("Million of USD (Imports)") +
  
  
  #scale_colour_brewer(type = "qual", palette = 8) +
  scale_colour_brewer(type = "qual", palette = "Set1")+
  scale_x_continuous(breaks=seq(1961,2016,1),limits = c(1961,2016))+
  #scale_y_continuous(breaks=seq(0,1300,100),limits = c(0,1300))+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),
        axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),
        legend.title=element_blank(),
        legend.box = "horizontal",
        # legend.text=element_text(size=rel(0.8)),
        legend.title = element_text(size=60), 
        legend.text = element_text(size=60),
        legend.key.height = unit(5, "cm"),
        legend.key.width = unit(5, "cm")
        
  )


#+
#geom_point(aes(color=Region))
#p


ggsave(paste0("E:/Dropbox/Dropbox/Paper_PhD/FAO","/","LINE_TA_I",".pdf"),p4,dpi=600,width =90,height=60,units = "cm",scale=1.2,limitsize = FALSE)
#ggsave(paste0("E:/Dropbox/Dropbox/Paper_PhD/FAO","/","LINE_PLOT",".png"),p,dpi=600,width =90,height=60,units = "cm",scale=1.2,limitsize = FALSE)


#########################################
#VALUE $$$ EXPORT
#########################################
FAO4 <- read.csv("E:/Dropbox/Dropbox/Paper_PhD/FAO/FAOSTAT_data_5-31-2018_TRADE.csv",header = T,sep=",")
FAO4 <- FAO4[which(FAO4$Item=="Sheep"),]
FAO4 <- FAO4[which(FAO4$Element=="Export Value"),]

FAO_data_TA_E <- as.data.frame(cbind(as.character(FAO4$Country),
                                     as.numeric(as.character(FAO4$Year)),
                                     as.numeric(as.character(FAO4$Value))
))
colnames(FAO_data_TA_E) <- c("Region","Year","Value")

FAO_data_TA_E$Year <- as.character(FAO4$Year)
FAO_data_TA_E$Year <- as.numeric(as.character(FAO_data_TA_E$Year))
FAO_data_TA_E$Value <- as.numeric(as.character(FAO_data_TA_E$Value))




#FAO_data_TA_E$Year <- year(as.Date(FAO_data_TA_E$Year,"%Y")) #it will gives only years Hope it helps to you! :)




#class(FAO_data_TA_E$Region)
#FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region=="World"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "Eastern Africa"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "Middle Africa"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "Northern Africa"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "Western Africa"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "Southern Africa"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "Northern America"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "Central America"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "Caribbean"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "South America"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "Central Asia"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "Eastern Asia"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "Southern Asia"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "South-Eastern Asia"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "Western Asia"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "Eastern Europe"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "Northern Europe"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "Southern Europe"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "Western Europe"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "Australia & New Zealand"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "Melanesia"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "Polynesia"),]
FAO_data_TA_E <- FAO_data_TA_E[which(FAO_data_TA_E$Region != "Ireland"),]


FAO_data_TA_E$Value <- FAO_data_TA_E$Value*1000; FAO_data_TA_E$Value <- FAO_data_TA_E$Value/1000000


p5<-ggplot(FAO_data_TA_E, aes(x=Year, y=Value,colour=Region, group = Region)) +
  # geom_path( position = "identity") +
  geom_line(size=4)+
  #  geom_point()+
  xlab("Year")+
  #  ylab("Gigagrams of CH4") +
  
  
  ylab("Million of USD (Exports)") +
  
  
  #scale_colour_brewer(type = "qual", palette = 8) +
  scale_colour_brewer(type = "qual", palette = "Set1")+
  scale_x_continuous(breaks=seq(1961,2016,1),limits = c(1961,2016))+
  #scale_y_continuous(breaks=seq(0,1300,100),limits = c(0,1300))+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),
        axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),
        legend.title=element_blank(),
        legend.box = "horizontal",
        # legend.text=element_text(size=rel(0.8)),
        legend.title = element_text(size=60), 
        legend.text = element_text(size=60),
        legend.key.height = unit(5, "cm"),
        legend.key.width = unit(5, "cm")
        
  )


#+
#geom_point(aes(color=Region))
#p


ggsave(paste0("E:/Dropbox/Dropbox/Paper_PhD/FAO","/","LINE_TA_E",".pdf"),p5,dpi=600,width =90,height=60,units = "cm",scale=1.2,limitsize = FALSE)
#ggsave(paste0("E:/Dropbox/Dropbox/Paper_PhD/FAO","/","LINE_PLOT",".png"),p,dpi=600,width =90,height=60,units = "cm",scale=1.2,limitsize = FALSE)

