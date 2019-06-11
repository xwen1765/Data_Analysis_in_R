#---------------------------------
# Packages
#---------------------------------
library(tidyverse)
library(magrittr)

#---------------------------------
# Load data
#---------------------------------

data = read_delim("data_in_R_format.txt", delim = "\t")

#---------------------------------
# Prepare data
#---------------------------------

data %<>%
  select(-index) %>%
  mutate(
    Correctness = ifelse(is.na(ReactionTime), NA, Correctness),
    ReactionTime = ifelse(is.na(Correctness), NA, ReactionTime)
  ) %>%
  filter(CueDirection %in% c(-1,1)) %>%
  mutate(
    CueValidity = case_when(
      CueValidity == 0 ~ "invalid",
      CueValidity == 1 ~ "valid",
      CueValidity == 2 ~ "neutral",
      T ~ NA_character_
    ),
    CueDirection = case_when(
      CueDirection == -1 ~ "bottom left",
      CueDirection == 1 ~ "top right",
      T ~ NA_character_
    )
  )

data %<>%
  mutate(
    TargetLocation = case_when(
      CueValidity == "valid" & CueDirection == "bottom left" ~ "Left",
      CueValidity == "invalid" & CueDirection == "top right" ~ "Left",
      CueValidity == "neutral" & CueDirection == "bottom left" ~ "Left",
      
      CueValidity == "valid" & CueDirection == "top right" ~ "Right",
      CueValidity == "invalid" & CueDirection == "bottom left" ~"Right",
      CueValidity == "neutral" & CueDirection == "top right" ~ "Right",
      
      TRUE ~ NA_character_
    ),
    MS_Include = ifelse(dplyr::between(MS_Start, 0, 3000), TRUE, FALSE),
    MS_DeviationFromTarget = case_when(
      MS_Include == F ~ NA_real_,        # Exclude all MS outside of relevant time window from this variable
      TargetLocation == "Left" ~ abs(atan(9/16) + pi - MS_Angle), 
      TargetLocation == "Right" ~ abs(atan(9/16) - MS_Angle), 
      TRUE ~ NA_real_
    )
  )


data.acc = data %>%
  group_by(subject, session, trials, CueDirection, CueValidity) %>%
  summarise_all(
    .funs = mean, na.rm = T
  ) %>%
  mutate(
    MS_Include = ifelse(is.na(MS_Include) | MS_Include == 0, -1, 1)
  )
nrow(d.acc)

#---------------------------------
# View data
#---------------------------------
# view(data.acc)

#---------------------------------
# Collect data
#---------------------------------

##Accuracy comparasion with free MS
data.acc %>%
  mutate(
    MS_Include = case_when(
      MS_Include == -1 ~ "Not include MS",
      MS_Include == 1 ~ "include MS",
      TRUE ~ NA_character_
    )
  )%>%
  ggplot(aes(x = MS_Include, y = Correctness)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange") +
  scale_y_continuous("Accuracy") + 
  facet_wrap(~ subject)+
  ggtitle("Accuracy with MS free trails")


##Accuracy comparasion with free MS (seperated cue validity)
data.acc %>%
  mutate(
    MS_Include = case_when(
      MS_Include == -1 ~ "Not include MS",
      MS_Include == 1 ~ "include MS",
      TRUE ~ NA_character_
    )
  )%>%
  ggplot(aes(x = MS_Include, color  = CueValidity, y = Correctness)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange") +
  scale_y_continuous("Accuracy") + 
  facet_wrap(~ subject)
summary(data.acc)
