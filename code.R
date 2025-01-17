
#load libraries
library(tidyverse)
library(scales)

#pulled from here
#https://www.bls.gov/cew/revisions/qcew-revisions.csv
#get revisions to the QCEW
qcew_revisions <- read_csv("qcew-revisions.csv")

#read in the CES benchmark revisions preliminary and final
ces_revisions <- read_csv("ces_revisions.csv")

#add the date and the difference between the prelim and final
ces_revisions <- ces_revisions %>% 
                  mutate(diff=(Final-Prelim)*1000,
                         date = as.Date(paste0(March,"-3-1"),
                          format="%Y-%m-%d") ) 

#fix the names of the dataset so it's easier to use
names(qcew_revisions) <- tolower(make.names(names(qcew_revisions))) 
                         
#I am only considering US revisions right now
qcew_revisions <- qcew_revisions %>% 
                    #Subset data to national employment and drop establishment counts
                    filter(area=="United States",                
                           str_detect(field,"Employment")) %>%
                    #convert 'field' into a month name and number field
                    mutate(monthn = trimws(str_remove(field," Employment")),
                           month = map_int(monthn,\(x){ which(x==month.name)})) %>% 
                    #CES only keep the initial QCEW values and its two revisions
                    select( year, quarter, initial.value,monthn, month,
                            first.revised.value) %>% 
                    #rename the values so that they're easier to use
                    rename( cut1 = initial.value, 
                            cut2 = first.revised.value) %>% 
                    #convert columns to numeric
                    #also create a date variable
                    mutate(across(matches("cut"),\(x){as.numeric(x)}),
                           date= as.Date(paste0(year,"-",month,"-1"),
                                         format="%Y-%m-%d"))

###cut1 cut2 plot
qcew_revisions %>% 
  pivot_longer(matches("cut")) %>% 
  ggplot(aes(x=date,y=value,col=name)) + 
  geom_line() +
  scale_y_continuous( labels= scales::comma_format(),
                      n.breaks = 8 ) + 
  labs(title="Initial QCEW Employment (Cut 1) vs First Revised (Cut 2)",
       col="Cut",
       y= "Employment",
       x= "Date")


###cut1 cut2 plot
qcew_revisions %>% 
  pivot_longer(matches("cut")) %>% 
  ggplot(aes(x=date,y=value,col=name)) + 
  geom_line() +
    scale_y_continuous( labels= scales::comma_format(),
                      n.breaks = 8 ) + 
  labs(title="Initial QCEW Employment (Cut 1) vs First Revised (Cut 2)",
       col="Cut",
       y= "Employment",
       x= "Date")


#plot revisions BUT only highlight march
qcew_revisions %>%  
  mutate(rev=cut2-cut1) %>% 
  ggplot(aes(x=date,y=rev)) + 
  geom_line(alpha=.5) + 
  geom_point(aes(x=date,y=rev,shape=if_else(month==3,15,NA) ))+
  scale_shape_identity()+
  geom_hline(yintercept = 0,col=alpha("black",alpha = .5),linetype = 2)+
  scale_y_continuous( labels= scales::comma_format()) + 
  labs(title="QCEW Revisions Cut 1 to Cut 2",
       y= "Revision",
       x= "Date")



#plot revisions BUT only highlight march
qcew_revisions %>%  
  mutate(rev=cut2-cut1) %>% 
  ggplot(aes(x=date,y=rev)) + 
  geom_line(alpha=.5) + 
  geom_point(aes(x=date,y=rev,shape=if_else(month==3,15,NA) ))+
  scale_shape_identity()+
  geom_hline(yintercept = 0,col=alpha("black",alpha = .5),linetype = 2)+
  scale_y_continuous( labels= scales::comma_format()) + 
  labs(title="QCEW Revisions Cut 1 to Cut 2",
       y= "Revision",
       x= "Date") + 
  geom_point(aes(x=date,y=diff),col="red",size=2,
             data=ces_revisions %>% filter(year(date)>=2017))

