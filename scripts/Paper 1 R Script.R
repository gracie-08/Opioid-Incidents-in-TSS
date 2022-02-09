## SET UP WORKSPACE ##
install.packages("opendatatoronto")
install.packages("lubridate")
install.packages("knitr")
install.packages("janitor")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("dplyr")

library(knitr)
library(janitor)
library(lubridate)
library(opendatatoronto)
library(tidyverse)
library(tidyr)
library(dplyr)

## GET RESOURCES AND WRITE TABLE ## 

opioid_overdoses <- # what i want to name my data frame
  list_package_resources("0d1fb545-d1b2-4e0a-b87f-d8a1835e5d85") |> 
  # the green code within the quotation is the ID for my data frame, can be found under "For Developers" section on OpenData webpage 
  filter(row_number()==1) |> 
  get_resource()

write_csv(
  x = opioid_overdoses, # name of the data frame chosen above (line 19)
  file = "opioid_overdoses.csv"
)

## CHECK THE TABLE ##
head(opioid_overdoses)

## CLEAN UP DATA ##

opioid_overdoses_clean <- # renamed data frame with the desired columns
  clean_names(opioid_overdoses) |> 
  select(id,year, year_stage, suspected_non_fatal_overdoses_incidents, fatal_overdoses_incident)
# I chose these 5 headings/columns

## CHECK CLEANED DATA ## 
head(opioid_overdoses_clean)

## WRITE TABLE FOR CLEANED DATA ##
write_csv(
  x = opioid_overdoses_clean, 
  file = "opioid_overdoses_clean.csv"
)

opioid_overdoses_clean <- 
  read_csv(
    "opioid_overdoses_clean.csv",
    show_col_types = FALSE
  )

## CHECK CLEANEAD DATA AGAIN##
head(opioid_overdoses_clean)

##--------------------------------------------------##

## SET UP THE GRAPH 1##

opioid_overdoses_clean |> # this is the name of my cleaned dataframe
  ggplot(mapping = aes(x = year, y = suspected_non_fatal_overdoses_incidents, fill = year_stage)) +
  
  # the above identifies how I want my graph will be: aes means aesthetic of the graph
  # state x and y variables by recalling their name from the csv table 
  # fill means the variable that will determine the colour
  # in this case, different bars will have different colours depending on year_stage
  
  geom_bar(stat="identity", position = "dodge") +
  
  # this means that I am creating a bar graph
  # dodge means the bars won't be stacked, they will be side by side
  
  labs(title = "Opioid Overdoses in Toronto's Shelter System", 
       x = "Year", 
       y = "Suspected Non Fatal Overdose Incidents",
       fill = "Year Stage") +
  # labs means labels!! 
  
  theme_classic() +
  scale_fill_brewer(palette = "Set1")
# theme is the background and scale brewer are used for bar colours!!

## GRAPH WITHOUT THE COMMENTS ##

opioid_overdoses_clean |> 
  ggplot(mapping = aes(x = year, y = suspected_non_fatal_overdoses_incidents, fill = year_stage, label = suspected_non_fatal_overdoses_incidents)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Suspected Non-Fatal Opioid Overdoses in Toronto's Shelter System", 
       x = "Year", 
       y = "No. of Incidents",
       fill = "Year Stage") +
  theme_classic() +
  scale_fill_brewer(palette = "Set1")

## ATTEMPT FOR DATA LABELS ##
opioid_overdoses_clean |> 
  ggplot(mapping = aes(x = year, y = suspected_non_fatal_overdoses_incidents, fill = year_stage, label = suspected_non_fatal_overdoses_incidents)) +
  geom_bar(stat="identity", position = position_dodge(0.9)) +
  geom_text(aes(label=suspected_non_fatal_overdoses_incidents), position=position_dodge(width=0.85), vjust=-0.3, size = 2.9)+
  labs(title = "Suspected Non-Fatal Opioid Overdose Incidents in Toronto", 
       x = "Year", 
       y = "No. of Incidents",
       fill = "Year Stage") +
  theme_classic() +
  scale_fill_brewer(palette = "Set1")

## -----------------------------------------------------##

## SETUP TIDY DATA (TABLE 1)

tibble(stage = c("Q1", "Q2", "Q3", "Total"),
       fatal = c(15,12,19,26),
       nonfatal = c(297, 313, 427, 1037)
) |>
  knitr::kable(
    caption = "Suspected Opioid Overdose Incidents in Toronto 2021",
    col.names = c("Stage", "Fatal Incidents", "Non Fatal Incidents"),
    digits = 1,
    booktabs = TRUE,
    linesep = ""
  )

## ----------------------------------------------##

comp_opioidoverdoses <- 
  clean_names(opioid_overdoses) |> 
  select(id, year, suspected_non_fatal_overdoses_incidents, fatal_overdoses_incident) |>
  filter(fatal_overdoses_incident != "< 5", 
         fatal_overdoses_incident != "<5") |>
  drop_na(fatal_overdoses_incident)

## CHECK CLEANED DATA ## 
head(comp_opioidoverdoses)

## WRITE TABLE FOR CLEANED DATA ##
write_csv(
  x = comp_opioidoverdoses, 
  file = "comp_opioid_overdoses.csv"
)

comp_opioidoverdoses <- 
  read_csv(
    "comp_opioid_overdoses.csv",
    show_col_types = FALSE
  )

## CHECK CLEANEAD DATA AGAIN##
head(comp_opioidoverdoses)


comp_opioidoverdoses |> 
  ggplot(mapping = aes(x = year, y = suspected_non_fatal_overdoses_incidents, fill = year_stage, label = suspected_non_fatal_overdoses_incidents)) +
  geom_bar(stat="identity", position = position_dodge(0.9)) +
  geom_text(aes(label=suspected_non_fatal_overdoses_incidents), position=position_dodge(width=0.85), vjust=-0.3, size = 2.9)+
  labs(title = "Suspected Non-Fatal Opioid Overdose Incidents in Toronto", 
       x = "Year", 
       y = "No. of Incidents",
       fill = "Year Stage") +
  theme_classic() +
  scale_fill_brewer(palette = "Set1")





## ATTEMPT FOR FILTERING VALUE FOR TABLE 2 WITH RAW DATA## 

raw_opioid_overdoses <-
  read.csv("suspected-opioid-overdoses-in-shelters.csv")

write.csv(
  x = raw_opioid_overdoses, 
  file = "raw_opioid_overdoses.csv"
)

raw_opioid_overdoses_clean <-
  clean_names(raw_opioid_overdoses) |> 
  select(location_name, year, year_stage, suspected_non_fatal_overdoses)  |>
  arrange(desc(suspected_non_fatal_overdoses)) |>
  filter(year == "2021", year_stage == "Q3", suspected_non_fatal_overdoses != "0",
         suspected_non_fatal_overdoses != "< 5", 
         suspected_non_fatal_overdoses != "<5",
         suspected_non_fatal_overdoses != "9",
         suspected_non_fatal_overdoses != "7",
         suspected_non_fatal_overdoses != "6",
         suspected_non_fatal_overdoses != "5") |>
  drop_na(suspected_non_fatal_overdoses)

write_csv(
  x = raw_opioid_overdoses_clean, 
  file = "raw_opioid_overdoses_clean.csv"
)

raw_opioid_overdoses_clean <- 
  read_csv(
    "raw_opioid_overdoses_clean.csv",
    show_col_types = FALSE
  )

head(raw_opioid_overdoses_clean)


tibble(shelter = c("HFS 45 The Esplanade","HFS 545 Lake Shore Blvd W","SSHA Seaton House",
                   "Dixon Hall Bond Place Hotel Program","St. Felix Centre 69 Fraser Ave Respite",
                   "HFS Willowdale Centre","HFS 195 Princes' Blvd","Youth Agencies Scarborough Hotel Program",
                   "HFS - Lawrence East Shelter","COSTI/SSHA Etobicoke Hotel Program",
                   "SSHA Scarborough Hotel Program 2"),
       nonfatal = c(46, 42, 42, 36,34,28,24,17,13,10,10)
) |>
  knitr::kable(
    caption = "Suspected Opioid Overdose Incidents in Toronto 2021",
    col.names = c("Shelter", "Non Fatal Incidents"),
    digits = 1,
    booktabs = TRUE,
    linesep = ""
  )



