##############################################################################
############## TRUSTLAB - DISCRIMINATION - US AND DEU  #######################
##############               FINAL GRAPHS              #######################
##############################################################################

# Trustlab paper: graphs using R
install.packages("gmodels")
install.packages("gridExtra")
install.packages("cowplot")
install.packages("ggpubr")

library(gmodels)
library(foreign)
library(ggplot2)
library(plyr)
library(gridExtra)
library(cowplot)
library(scales)
library(ggpubr)


# Open dataset
setwd("/Users/cetre/Dropbox/RECHERCHES/Experimental_Economics/Trust/Trustlab dataset")
graph_path <-"/Users/cetre/Dropbox/RECHERCHES/Experimental_Economics/Trust/US_Germany/Paper"
data <- read.dta("Trustlab_R.dta")


attach(data)



########################## FIGURE 1 ########################## 
# FIRST MOVER TRANSFER BY RACE OF SECOND MOVER - US AND GERMANY
# Setting up the ethnic groups
b_race <- NA
b_race[b_black == 1 | b_black_top20 == 1] <- "B Afr. Ame."
b_race[b_white == 1 | b_white_top20 == 1] <- "B White"
b_race[b_hispanic ==1 | b_hisp_top20 == 1] <- "B Hispanic"
b_race[b_native ==1 | b_native_top20 == 1] <- "B Rooted German"
b_race[b_eeur ==1 | b_eeur_top20 == 1] <- "B East. Eur."
b_race[b_turkish ==1 | b_turk_top20 == 1] <- "B Turkish"

# Adding the variable to the data frame
data$b_race <- b_race

# Removing the generalized trust from the data
subset <- subset(data,(!is.na(b_race)))

# Collapse mean and calculation of CI
means.sem <- ddply(subset, .(b_race,part), summarise,
                   mean=mean(trust), sem=sd(trust)/sqrt(length(trust)))
means.sem <- transform(means.sem, lower=mean-1.96*sem, upper=mean+1.96*sem)

top20 <-ifelse(means.sem$part=="whi20" | means.sem$part=="afa20" | means.sem$part=="his20"
               |means.sem$part=="nat20" |means.sem$part=="eeur20" |means.sem$part=="turk20","2. Second mover is also from the top 20%","1. Only ethnic group of second mover is known")
means.sem$top20 <- top20



# Selection of the subsets US and Germany
US <- subset(means.sem,(b_race=="B Afr. Ame." | b_race=="B White" | b_race=="B Hispanic"))
Germany <-subset(means.sem,(b_race=="B Rooted German" | b_race=="B Turkish" | b_race=="B East. Eur."))

# Plot on the US
p1 <- ggplot(data=US, aes(x=b_race,y=mean,fill=factor(b_race))) +
  geom_bar(position="dodge",stat="identity",width=0.7) + geom_errorbar(aes(ymax =upper, ymin=lower), width=0.2, alpha=0.6) +
  coord_flip(ylim = c(3.5, 6))  + labs(y = "Mean transfer from A to B", x="")  +
  guides(fill=FALSE) + # This removes the legend 
  scale_fill_manual(values=c("deepskyblue1", "deepskyblue3", "deepskyblue4")) + # This changes the color 
  facet_wrap(~top20, ncol=1) + ggtitle("US") + theme_gray() +
  theme(plot.title = element_text(hjust = 0.5)) +  theme(plot.margin = unit(c(1,1,1,0), "cm"))  +
  scale_x_discrete(limits= c("B White","B Hispanic","B Afr. Ame.")) # This set up the order of the labels

# Plot on Germany
p2 <- ggplot(data=Germany, aes(x=b_race,y=mean,fill=factor(b_race))) +
  geom_bar(position="dodge",stat="identity",width=0.7) + geom_errorbar(aes(ymax =upper, ymin=lower), width=0.2, alpha=0.6) +
  coord_flip(ylim = c(3.5, 6))  + labs(y = "Mean transfer from A to B", x="")  +
  guides(fill=FALSE) + # This removes the legend 
  scale_fill_manual(values=c("palegreen2", "palegreen3", "palegreen4")) + # This changes the color 
  facet_wrap(~top20, ncol=1) + ggtitle("Germany") + theme_gray() +
  theme(plot.title = element_text(hjust = 0.5)) +  theme(plot.margin = unit(c(1,1,1,-1), "cm")) +
  scale_x_discrete(limits= c("B Turkish","B Rooted German","B East. Eur.")) 



# Combine both graph and save
combined <- plot_grid(p1, p2, ncol=2, align="h")
ggsave(paste(graph_path, "US_DEU_trust_race.pdf", sep="/"),combined, width=3, height=2, units="in", scale=3)


########################## FIGURE 2 ########################## 
# Same figure but breaking down by first mover ethnic group 

a_race <- NA
a_race[a_black == 1] <- "First mover is Afr. Ame."
a_race[a_white == 1] <- "First mover is White"
a_race[a_hispanic ==1 ] <- "First mover is Hispanic"
a_race[a_native ==1] <- "First mover is a Rooted German"
a_race[a_eeur ==1] <- "First mover is from East. Eur."
a_race[a_turkish ==1] <- "First mover is of Turkish descent"
a_race[a_other_US ==1] <- "Other first mover"
a_race[data$US==1 & a_black != 1 & a_white !=1 & a_hispanic != 1] <- "Other first movers"
a_race[data$DEU==1 & a_native != 1 & a_eeur !=1 & a_turkish != 1] <- "Other first movers"

# Adding the variable to the data frame
data$a_race <- a_race

table(a_race, useNA="ifany")

# Removing the generalized trust from the data
subset <- subset(data,(!is.na(b_race)))

# Collapse mean and calculation of CI
means.sem <- ddply(subset, .(a_race,b_race,part), summarise,
                   mean=mean(trust), sem=sd(trust)/sqrt(length(trust)))
means.sem <- transform(means.sem, lower=mean-1.96*sem, upper=mean+1.96*sem)

top20 <-ifelse(means.sem$part=="whi20" | means.sem$part=="afa20" | means.sem$part=="his20"
               |means.sem$part=="nat20" |means.sem$part=="eeur20" |means.sem$part=="turk20","2. B is from top 20%","1. Only B's ethnic group is known")
means.sem$top20 <- top20

# Selection of the subsets US and Germany
US <- subset(means.sem,(b_race=="B Afr. Ame." | b_race=="B White" | b_race=="B Hispanic"))
Germany <-subset(means.sem,(b_race=="B Rooted German" | b_race=="B Turkish" | b_race=="B East. Eur."))

# US plot
p1 <-ggplot(data=US, aes(x=b_race,y=mean,fill=factor(b_race))) +
  geom_bar(position="dodge",stat="identity",width=0.7) + geom_errorbar(aes(ymax =upper, ymin=lower), width=0.2, alpha=0.6) +
  coord_flip(ylim = c(2.5, 6.5))  + labs(y = "Mean transfer from A to B", x="")  +
  guides(fill=FALSE) + # This removes the legend 
  scale_fill_manual(values=c("deepskyblue1", "deepskyblue3", "deepskyblue4")) + # This changes the color 
  facet_grid(top20 ~ a_race)  + theme_gray() + ggtitle("US") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits= c("B White","B Hispanic","B Afr. Ame.")) # This set up the order of the labels


ggsave(paste(graph_path, "trust_USA_race.pdf", sep="/"),p1, width=3.1, height=1.8, units="in", scale=3)


p2 <-ggplot(data=Germany, aes(x=b_race,y=mean,fill=factor(b_race))) +
  geom_bar(position="dodge",stat="identity",width=0.7) + geom_errorbar(aes(ymax =upper, ymin=lower), width=0.2, alpha=0.6) +
  coord_flip(ylim = c(2.5, 6.5))  + labs(y = "Mean transfer from A to B", x="")  +
  guides(fill=FALSE) + # This removes the legend 
  scale_fill_manual(values=c("palegreen2", "palegreen3", "palegreen4")) + # This changes the color 
  facet_grid(top20 ~ a_race)  + theme_gray() + ggtitle("Germany") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits= c("B Turkish","B Rooted German","B East. Eur.")) 


  ggsave(paste(graph_path, "trust_DEU_race.pdf", sep="/"),p2, width=3.1, height=1.8, units="in", scale=3)
  
  
 

########################## FIGURE 3 ##########################
# Difference in Trustworthiness across ethnic groups
data <- read.dta("Trustlab_R_short.dta")
attach(data)

a_race <- NA
a_race[a_black == 1] <- "Second mover is Afr. Ame."
a_race[a_white == 1] <- "Second mover is White"
a_race[a_hispanic ==1 ] <- "Second mover is Hispanic"
a_race[a_native ==1] <- "Second mover is a Rooted German"
a_race[a_eeur ==1] <- "Second mover is from East. Eur."
a_race[a_turkish ==1] <- "Second mover is of Turkish descent"
a_race[data$US==1 & a_black != 1 & a_white !=1 & a_hispanic != 1] <- "Other US second movers"
a_race[data$DEU==1 & a_native != 1 & a_eeur !=1 & a_turkish != 1] <- "Other German second movers"

# Adding the variable to the data frame
data$a_race <- a_race

# Collapse mean and calculation of CI
means.sem <- ddply(data, .(a_race), summarise,
                   mean=mean(trustworthiness), sem=sd(trustworthiness)/sqrt(length(trustworthiness)))
means.sem <- transform(means.sem, lower=mean-1.96*sem, upper=mean+1.96*sem)


# Selection of the subsets US and Germany
US <- subset(means.sem,(a_race=="Second mover is Afr. Ame." | a_race=="Second mover is White" 
                        | a_race=="Second mover is Hispanic" | a_race=="Other US second movers"))
Germany <-subset(means.sem,(a_race=="Second mover is a Rooted German" | a_race=="Second mover is from East. Eur." 
                            | a_race=="Second mover is of Turkish descent" | a_race=="Other German second movers"))
# US graph
p1 <-ggplot(data=US, aes(x=a_race,y=mean,fill=factor(a_race))) +
  geom_bar(position="dodge",stat="identity",width=0.7) + geom_errorbar(aes(ymax =upper, ymin=lower), width=0.2, alpha=0.6) +
  coord_flip()  + labs(y = "Mean transfer from B to A", x="")  +
  guides(fill=FALSE) + # This removes the legend 
  scale_fill_manual(values=c("dodgerblue4","deepskyblue1", "deepskyblue3", "deepskyblue4")) + # This changes the color 
   theme_gray() + ggtitle("US") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits= c("Other US second movers","Second mover is White" ,
                             "Second mover is Hispanic","Second mover is Afr. Ame." ))


p2 <-ggplot(data=Germany, aes(x=a_race,y=mean,fill=factor(a_race))) +
  geom_bar(position="dodge",stat="identity",width=0.7) + geom_errorbar(aes(ymax =upper, ymin=lower), width=0.2, alpha=0.6) +
  coord_flip()  + labs(y = "Mean transfer from B to A", x="")  +
  guides(fill=FALSE) + # This removes the legend 
  scale_fill_manual(values=c("darkgreen", "palegreen3","palegreen2", "palegreen4")) + # This changes the color 
  theme_gray() + ggtitle("Germany") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits= c("Other German second movers","Second mover is of Turkish descent",
                             "Second mover is a Rooted German","Second mover is from East. Eur."))

combined <- plot_grid(p1, p2, ncol=1, align="v")
combined
ggsave(paste(graph_path, "trustworthiness.pdf", sep="/"),combined, width=3, height=2, units="in", scale=2)


 ########################## FIGURE 4 ########################## 
  # Time to complete the survey by transfer decision
data <- read.dta("Trustlab_R_short.dta")
attach(data)

switcherp2 <- ifelse(no_change_trust_race==0,'Transfer different amounts across ethnic groups','Transfer same amounts across ethnic groups')
switcherp3 <- ifelse(no_change_trust_race_b3==0,'Transfer different amounts across ethnic groups','Transfer same amounts across ethnic groups')
switcher_all <- ifelse(no_change_trust_race_all==0,'Transfer different amounts across ethnic groups','Transfer same amounts across ethnic groups')
data$switcherp2 <-switcherp2
data$switcherp3 <-switcherp3
data$switcher_all <-switcher_all

subset <-subset(data, (time_all < 500))

# Careful the switcher variable is wrong here

block2 <- ggplot(subset, aes(x=time_all)) + geom_density(aes(group=factor(switcherp2), colour=factor(switcherp2), fill=factor(switcherp2)), alpha=0.3) +
  theme_gray() + theme(legend.title=element_blank()) + theme(legend.position ="bottom") + 
  guides(col = guide_legend(nrow = 2)) + labs(x="Time to complete the online survey (min)") + ggtitle("Germany") +
  theme(plot.title = element_text(hjust = 0.5)) 

block3 <- ggplot(subset, aes(x=time_all)) + geom_density(aes(group=factor(switcherp3), colour=factor(switcherp3), fill=factor(switcherp3)), alpha=0.3) +
  theme_gray() + theme(legend.title=element_blank()) + theme(legend.position ="bottom") + 
  guides(col = guide_legend(nrow = 2)) + labs(x="Time to complete the online survey (min)") 

# Combine both graphs
combined <- plot_grid(block2, block3, ncol=2, align="h")
combined <- ggarrange(block2, block3, ncol=2, common.legend = TRUE, legend="bottom")

# Only one graph for everyone
all <- ggplot(subset, aes(x=time_all)) + geom_density(aes(group=factor(switcher_all), colour=factor(switcher_all), fill=factor(switcher_all)), alpha=0.3) +
  theme_gray() + theme(legend.title=element_blank()) + theme(legend.position = c(0.7, 0.7)) +
  guides(col = guide_legend(nrow = 2)) + labs(x="Time to complete the online survey (min)") 

all
 
ggsave(paste(graph_path, "time_all.pdf", sep="/"),all, width=3, height=2, units="in", scale=2)

