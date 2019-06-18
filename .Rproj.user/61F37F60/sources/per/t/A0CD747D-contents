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
      TargetLocation == "Left" ~ ifelse(MS_Angle < atan(9/16), abs(pi - atan(9/16) + MS_Angle), abs(atan(9/16) + pi - MS_Angle)), 
      TargetLocation == "Right" ~ ifelse(MS_Angle < atan(9/16) + pi, abs(atan(9/16) - MS_Angle), abs(2*pi + atan(9/16) - MS_Angle)), 
      TRUE ~ NA_real_
    )
  )

#---------------------------------
# View data
#---------------------------------
# view(data.acc)

#-----------------------------------
# Accuracy Conparasion with free MS
#-----------------------------------


data.acc = data %>%
  group_by(subject, session, trials, CueDirection, CueValidity) %>%
  summarise_all(
    .funs = mean, na.rm = T
  ) %>%
  mutate(
    MS_Include = ifelse(is.na(MS_Include) | MS_Include == 0, -1, 1)
  )
nrow(d.acc)

theme_update(plot.title = element_text(hjust = 0.5))
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


##Accuracy comparasion with free MS (seperated by cue validity)
data.acc$CueValidity <- factor(data.acc$CueValidity,  c("valid","neutral","invalid"))

data.acc %>%
  mutate(
    MS_Include = case_when(
      MS_Include == -1 ~ "Not include MS",
      MS_Include == 1 ~ "include MS",
      TRUE ~ NA_character_
    )
  )%>%
  ggplot(aes(x = MS_Include, color  = CueValidity, y = Correctness)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange", position = position_dodge(.3)) +
  scale_y_continuous("Accuracy") + 
  facet_wrap(~ subject)+
  ggtitle("Accuracy comparasion with free MS (seperated by cue validity)")


#-----------------------------------
# Microsaccades Deviation
#-----------------------------------


data.dev =  filter(data, !is.na(MS_DeviationFromTarget))

data.dev %<>%
  mutate(
    MS_InSameDirection = case_when(
      MS_DeviationFromTarget < pi/2 ~ 1,
      MS_DeviationFromTarget >= pi/2 ~ 0,
      TRUE ~ NA_real_
    )
  )

data.dev$CueValidity <- factor(data.dev$CueValidity,  c("valid","neutral","invalid"))
data.dev%>%
  ggplot(aes(x = CueValidity, y  = MS_InSameDirection)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange") +
  scale_y_continuous("Percentage") + 
  facet_wrap(~ subject)+
  geom_hline(yintercept = 0.5, color = "gray60")+
  ggtitle("Percentage of MS in Direction of Target")+
  scale_x_discrete(limit = c("valid","neutral","invalid"))

data.dev %>%
  mutate(
    MS_InSameDirection = case_when(
      MS_InSameDirection == 1 ~ "Same direction",
      MS_InSameDirection == 0 ~ "Not same direction",
      TRUE ~ NA_character_
    )
  )%>%
  ggplot(aes(x = MS_InSameDirection, y  = Correctness,  color = CueValidity)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange", position = position_dodge(.2)) +
  scale_y_continuous("Accuracy") + 
  facet_wrap(~ subject)+
  ggtitle("Accuracy for Different Cue Type and MS Deviation")

#---------------------------------------------------------
# Relation between Presense of MS and Accuracy
#---------------------------------------------------------

data.rlt =  data

data.rlt%<>%
  mutate(
    MS_InSameDirection = case_when(
      MS_DeviationFromTarget < pi/2 ~ 1,
      MS_DeviationFromTarget >= pi/2 ~ 0,
      TRUE ~ NA_real_
    ),
    MS_Include = ifelse(is.na(MS_Include),"Not Include MS","Include MS")
  )

#reorder 
data.rlt$CueValidity <- factor(data.rlt$CueValidity,  c("valid","neutral","invalid"))
data.rlt%>%
  mutate(
    MS_InSameDirection = case_when(
      MS_InSameDirection == 1 ~ "Same direction",
      MS_InSameDirection == 0 ~ "Not same direction",
      TRUE ~ NA_character_
    )
  )%>%
  ggplot(aes(x = CueDirection, color = MS_Include ,y = Correctness)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange", position = position_dodge(.3)) +
  scale_y_continuous("Accuracy")+
  facet_wrap(~subject)+
  ggtitle("Relation between Persense of MS, Target location and Accuracy")

