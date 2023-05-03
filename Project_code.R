#packages used
library(tidyverse)
library(here)
install.packages('extrafont')
library(extrafont)
font_import()
loadfonts()
library(gganimate)
install.packages("gganimate")
install.packages("gifski")
install.packages("png")

#here is a link to the data - coding book is on these pages
# https://digital.nhs.uk/data-and-information/publications/statistical/autism-statistics/april-2019-to-december-2021
# https://digital.nhs.uk/data-and-information/publications/statistical/autism-statistics/july-2021-to-june-2022

#add a function for repeating lines somehow or compress but ideally need a function

# import data as data frames
Apr19_Dec21 <- read.csv(unz(here("data","Zipped_total_data.zip"),"April2019_Dec2021.csv")) 
Jul21_Jun22 <- read.csv(unz(here("data","Zipped_total_data.zip"),"July2021_June2022.csv"))

#new_df <- subset(df, b != 7 & d != 38)
# do a keep vs elim func i think and merge first

# keeping only new referral data
c1Apr19_Dec21 <- Apr19_Dec21[Apr19_Dec21$METRIC == 'ASD12',]
c1Jul21_Jun22 <- Jul21_Jun22[Jul21_Jun22$METRIC == 'ASD12',]

# keeping only age-related data
c2Apr19_Dec21 <- c1Apr19_Dec21[c1Apr19_Dec21$SECONDARY_LEVEL != 'NONE',]
c2Jul21_Jun22 <- c1Jul21_Jun22[c1Jul21_Jun22$SECONDARY_LEVEL != 'NONE',]

# keeping only CCG data
c2Apr19_Dec21 <- c2Apr19_Dec21[c2Apr19_Dec21$BREAKDOWN != 'CCG - GP Practice or Residence; Age Group',]
c2Jul21_Jun22 <- c2Jul21_Jun22[c2Jul21_Jun22$BREAKDOWN != 'CCG - GP Practice or Residence; Age Group',]

#find the last date of the first dataset and the first of the second dataset
tail(c2Apr19_Dec21, n=1)
head(c2Jul21_Jun22, n=1)

#find the last row that is a duplicate
index <- which(c2Jul21_Jun22[,2] == "31/12/2021")
lastdupind <- tail(index,1)

#create a new dataframe for the second dataset with only dates after this
c3Jul21_Jun22 <- c2Jul21_Jun22[-c(1:lastdupind),]

#making one big dataframe with no duplicates
merged_Apr19_Jun22 <- rbind(c2Apr19_Dec21,c3Jul21_Jun22)

#changing date values from character class to date format
#changing referral numbers from character to numeric
classformattedmerged <- mutate(merged_Apr19_Jun22,REPORTING_PERIOD_START = as.Date(REPORTING_PERIOD_START, format = "%d/%m/%Y")) 
classformattedmerged <- mutate(classformattedmerged,REPORTING_PERIOD_END = as.Date(REPORTING_PERIOD_END, format = "%d/%m/%Y"))
classformattedmerged <- mutate(classformattedmerged,METRIC_VALUE = as.numeric(METRIC_VALUE))

#removing any NA/unknown values
formatted_values_only <- classformattedmerged[!is.na(classformattedmerged$METRIC_VALUE),]
formatted_values_only <- classformattedmerged[classformattedmerged$SECONDARY_LEVEL != "UNKNOWN",]

#making the legend label order in age order
formatted_values_only$SECONDARY_LEVEL[formatted_values_only$SECONDARY_LEVEL == 'Under 18'] <- '0 - 17'

#Sum by age and date
ref_age_date <- aggregate(METRIC_VALUE ~ REPORTING_PERIOD_START + SECONDARY_LEVEL, data = formatted_values_only, FUN = "sum")
ref_age_date <- data.frame(ref_age_date)


#initial basic plot
initialplot <- ggplot(ref_age_date, aes(x = REPORTING_PERIOD_START, y = METRIC_VALUE, color = SECONDARY_LEVEL)) +
  geom_point(size = 1.5) +
  labs(title = "New autism referrals each month by age from 2019-mid2022", x = "date", y = "Number of New Referrals") +
  guides(color=guide_legend("Age Group"))+
  scale_color_manual(values=c("#FF6A6A", "#FFA54F", "#FFEC8B", "#90EE90", "#7EC0EE","#7A67EE","#8B8386")) + 
  geom_point(shape = 1,size = 1.5,colour = "black")

#viewing initial plot
initialplot

# x values for annotations on barplot
lockdown1 <- head(which(ref_age_date[,1] == "2020-03-01"),1)
lockdown_text_place <- head(which(ref_age_date[,1] == "2020-02-01"),1)

#plotting a bar chart
barplot <- ggplot(ref_age_date, aes(x=REPORTING_PERIOD_START,y=METRIC_VALUE)) +
  geom_bar(aes(fill=SECONDARY_LEVEL), stat='identity', colour='black', width=20)+ 
  scale_y_continuous(breaks=seq(0,10000,500),expand = c(0,0), limits = c(0, 10000))+
  scale_x_date(date_breaks = '1 month', date_labels = "%b-%Y", expand = expansion(add = 2))+
  theme(axis.text.x=element_text(angle=60, hjust=1),
        plot.title=element_text(family='Times New Roman', face="bold", size=20,hjust = 0.5, vjust = 0.5),
        axis.text = element_text(family="Times New Roman", colour = "black"),
        legend.text = element_text(family="Times New Roman", colour = "black", face = "bold"), 
        legend.background = element_rect(fill = "#C1CDCD",linetype = 1, colour = "black", linewidth = 1),
        legend.title = element_text(family="Times New Roman", colour = "black", face = "bold", size = 12),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 2),)+
  labs(title="New autism referrals each month by age from April 2019 - June 2022", x="Date", y="Number of Referrals Across England", fill = "Age Groups:")+
  scale_fill_manual(values=c("#FF6A6A", "#FFA54F", "#FFEC8B", "#90EE90", "#7EC0EE","#7A67EE","#FFF0F5"))+
  geom_vline(xintercept = ref_age_date[lockdown1,1])+
  annotate("text", x= ref_age_date[lockdown_text_place,1], y=7600, label="Start of 1st Lockdown in England", angle=90)

#viewing the bar chart
barplot


## animating the plot
anim2 <- barplot + 
  transition_states(REPORTING_PERIOD_START,transition_length = 2,state_length = 1) + 
  shadow_mark() 

#viewing the animated plot
anim2

#saving the animated plot as a gif
barplot_anim_gif <- animate(anim2,height = 8, width = 13, units = "in", res = 250, renderer =gifski_renderer("barplot_gif", loop=FALSE))
anim_save("barplot_anim_gif")




# Needed only on Windows - run once per R session
# Adjust the path to match your installation of Ghostscript
#Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.05/bin/gswin32c.exe")
#embed_fonts("font_plot.pdf", outfile="font_plot_embed.pdf")
#embed_fonts("font_ggplot.pdf", outfile="font_ggplot_embed.pdf")
# If outfile is not specified, it will overwrite the original file