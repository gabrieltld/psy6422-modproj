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

#loading the data
Apr19_Dec21 <- read.csv(unz(here("data","Zipped_total_data.zip"),"April2019_Dec2021.csv")) 
Jul21_Jun22 <- read.csv(unz(here("data","Zipped_total_data.zip"),"July2021_June2022.csv"))

#making a function
fval_tail <- function(df,col_num,value) {
  #find a value in the data fram that matches input, find the element number of the last instance of this
  index <- which(df[,col_num] == value)
  output <- tail(index,1)
}


#create a new dataframe for the second dataset with only dates after this
lastdupind <- fval_tail(Jul21_Jun22,1,tail(Apr19_Dec21$REPORTING_PERIOD_START, n=1))
nondup_Jun21_Jul22 <- Jul21_Jun22[-c(1:lastdupind),]

#merge
merged <- rbind(Apr19_Dec21,nondup_Jun21_Jul22)
merged_subset <- subset(merged,merged$METRIC == 'ASD12' & 
                          merged$SECONDARY_LEVEL != 'NONE' & 
                          merged$BREAKDOWN != 'CCG - GP Practice or Residence; Age Group')

#format the data classes
classform_ms <- mutate(merged_subset,REPORTING_PERIOD_START = as.Date(REPORTING_PERIOD_START, format = "%d/%m/%Y")) 
classform_ms <- mutate(classform_ms,REPORTING_PERIOD_END = as.Date(REPORTING_PERIOD_END, format = "%d/%m/%Y"))
classform_ms <- mutate(classform_ms,METRIC_VALUE = as.numeric(METRIC_VALUE))

#removing any NA/unknown values
valonly_full <- classform_ms[!is.na(classform_ms$METRIC_VALUE),]
valonly_full <- classform_ms[classform_ms$SECONDARY_LEVEL != "UNKNOWN",]

#making the legend label order in age order
valonly_full$SECONDARY_LEVEL[valonly_full$SECONDARY_LEVEL == 'Under 18'] <- '0 - 17'

#Sum by age and date
ref_age_date <- aggregate(METRIC_VALUE ~ REPORTING_PERIOD_START + SECONDARY_LEVEL, data = valonly_full, FUN = "sum")
ref_age_date <- data.frame(ref_age_date)


#initial basic plot
initialplot <- ggplot(ref_age_date, aes(x = REPORTING_PERIOD_START, y = METRIC_VALUE, color = SECONDARY_LEVEL)) +
  geom_point(size = 1.5) +
  labs(title = "New autism referrals each month by age from 2019-mid2022", x = "date", y = "Number of New Referrals") +
  guides(color=guide_legend("Age Group"))+
  scale_color_manual(values=c("#FF6A6A", "#FFA54F", "#FFEC8B", "#90EE90", "#7EC0EE","#7A67EE","#8B8386")) + 
  geom_point(shape = 1,size = 1.5,colour = "black")

#viewing and saving initial plot
ggsave(here("visualisations","initialplot.png"),height = 8, width = 13)
initialplot

# x values for annotations on barplot
lockdown1 <- fval_tail(ref_age_date,1,"2020-03-01")
lockdown_text_placement <- fval_tail(ref_age_date,1,"2020-02-01")

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
  annotate("text", x= ref_age_date[lockdown_text_placement,1], y=7600, label="Start of 1st Lockdown in England", angle=90)

#viewing and saving the bar chart
ggsave(here("visualisations","viz220225546.png"),height = 8, width = 13)
barplot


#animating the plot 
anim <- barplot + 
  transition_states(REPORTING_PERIOD_START,transition_length = 2,state_length = 1) + 
  shadow_mark() 

#viewing the animated plot
anim

#saving the animated plot as a gif
barplot_anim_gif <- animate(anim,height = 8, width = 13, units = "in", res = 250, renderer =gifski_renderer("barplot_gif", loop=TRUE))
anim_save(here("visualisations","barplot_anim_gif"))
