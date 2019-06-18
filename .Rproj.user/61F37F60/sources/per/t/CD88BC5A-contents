## ----------------------------------------------------------------------
# Packages
## ----------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(lme4)
library(lmerTest)
library(mgcv)
library(Hmisc)


## ----------------------------------------------------------------------
# Load data
## ----------------------------------------------------------------------
d = read_delim("data_in_R_format.txt", delim = "\t")


## ----------------------------------------------------------------------
# Prepare data
## ----------------------------------------------------------------------

d %<>%
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
    # Correctness = case_when(
    #   Correctness == 0 ~ "incorrect",
    #   Correctness == 1 ~ "correct",
    #   T ~ NA_character_
    #   )
  ) %>%
  mutate_at(
    c("CueDirection", "CueValidity", "subject"),
    .funs = factor
  ) %>%
  droplevels()

contrasts(d$CueDirection) = 
  cbind(
    "TR vs. BL" = c(-1, 1))
contrasts(d$CueValidity) = 
  cbind(
    "Valid vs. neutral" = c(0, -1, 1),
    "Invalid vs. neutral" = c(1, -1, 0))
summary(d)


## ----------------------------------------------------------------------
# Extractring information about microsaccades (MS) for the RT and accuracy 
# analyses. For this purpose we are only considering MS between 505ms and
# 1141 ms from trial onset.
## ----------------------------------------------------------------------
d %<>%
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
    MS_Include = ifelse(dplyr::between(MS_Start, 505, 1141), TRUE, FALSE),
    MS_DeviationFromTarget = case_when(
      MS_Include == F ~ NA_real_,        # Exclude all MS outside of relevant time window from this variable
      TargetLocation == "Left" ~ abs(atan(9/16) + pi - MS_Angle), 
      TargetLocation == "Right" ~ abs(atan(9/16) - MS_Angle), 
      TRUE ~ NA_real_
    )
  )

## ----------------------------------------------------------------------
# Reaction time analyses and plots
## ----------------------------------------------------------------------
d.rt = d %>%
  group_by(subject, session, trials, CueDirection, CueValidity) %>%
  summarise_all(
    .funs = mean, na.rm = T
  ) %>%
  mutate(
    MS_Include = ifelse(is.na(MS_Include) | MS_Include == 0, -1, 1)
  )
nrow(d.rt)

l.rt = lm(ReactionTime ~ 1 + CueDirection + CueValidity + CueDirection:CueValidity,
   data = d.rt)
summary(l.rt)

# l.rt.bryce = lm(ReactionTime ~ 1 + CueDirection + CueValidity + CueDirection:CueValidity,
#           data = d.rt,
#           subset = subject == "Bryce")
# summary(l.rt.bryce)

d.rt %>%
  ggplot(aes(x = CueDirection, color = CueValidity, y = ReactionTime)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange", position = position_dodge(.1)) +
  ggtitle("Relationship between cue direction, \ncue validity and reaction time \n(Combined data from 4 subjects)")+
  scale_y_log10("Reaction times (log-scale)")

tiff("Relationship between cue direction,cue validity and reaction time(4 subjects).tiff", units="in", width=5, height=5, res=300);
d.rt %>%
  ggplot(aes(x = CueDirection, color = CueValidity, y = ReactionTime)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange", position = position_dodge(.1)) +
  ggtitle("Relationship between cue direction, \ncue validity and reaction time \n(4 subjects)")+
  scale_y_log10("Reaction times (log-scale)") +
  facet_wrap(~ subject)
dev.off()
# ll.rt = lmList(ReactionTime ~ 1 + CueDirection + CueValidity + CueDirection:CueValidity | subject,
#           data = subset(d.rt, !is.na(ReactionTime))$ReactionTime)
# summary(ll.rt)

d.rt$MS_DeviationFromTarget = ifelse(is.na(d.rt$MS_DeviationFromTarget), max(d.rt$MS_DeviationFromTarget, na.rm=T), d.rt$MS_DeviationFromTarget)
ml.rt = lmer(ReactionTime ~ 1 + CueDirection + CueValidity + CueDirection:CueValidity + 
               # MS_Include +
               MS_DeviationFromTarget +
            (1 + CueDirection + CueValidity + CueDirection:CueValidity | subject),
          data = d.rt)
summary(ml.rt)
ranef(ml.rt)

d.rt %>%
  group_by(subject, CueDirection, CueValidity) %>%
  dplyr::summarise(
    ReactionTime = mean(ReactionTime, na.rm = T)
  ) %>%
  ggplot(aes(x = CueDirection, color = CueValidity, y = ReactionTime)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange", position = position_dodge(.1)) +
  scale_y_log10("Reaction times (log-scale)")


ml.rt.acc.trial = lmer(ReactionTime ~ 1 + CueDirection * CueValidity + 
                   Correctness +
                   trials +
               (1 + CueDirection + CueValidity + CueDirection:CueValidity | subject),
             data = d.rt)
summary(ml.rt.acc.trial)

## ----------------------------------------------------------------------
# Accuracy analyses and plots
## ----------------------------------------------------------------------
ml.acc = glmer(Correctness ~ 1 + CueDirection + CueValidity + CueDirection:CueValidity +
            (1 + CueDirection + CueValidity + CueDirection:CueValidity | subject),
          data = d,
          family = "binomial")
summary(ml.acc)
ranef(ml.acc)

tiff("Relationship between cue direction,cue validity and Accuracy(4 subjects).tiff", units="in", width=5, height=5, res=300);
d %>%
  ggplot(aes(x = CueDirection, color = CueValidity, y = Correctness)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange", position = position_dodge(.1))  +
  facet_wrap(~ subject)
dev.off()
  
  
d %>%
  group_by(subject, CueDirection, CueValidity) %>%
  dplyr::summarise(
    Correctness = mean(Correctness, na.rm = T)
  ) %>%
  ggplot(aes(x = CueDirection, color = CueValidity, y = Correctness)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange", position = position_dodge(.1)) +
  scale_y_continuous("Accuracy")


ml.acc.rt = glmer(Correctness ~ 1 + CueDirection + CueValidity + CueDirection:CueValidity + 
                    log(ReactionTime) +
                 (1 + CueDirection + CueValidity + CueDirection:CueValidity | subject) + trials,
               data = d,
               family = "binomial")
summary(ml.acc.rt)




## ----------------------------------------------------------------------
# Other stuff
## ----------------------------------------------------------------------
library(brms)
#binary regression model
bl.acc.rt = brm(Correctness ~ 1 + CueDirection + CueValidity + CueDirection:CueValidity + 
                    log(ReactionTime) +
                    (1 + CueDirection + CueValidity + CueDirection:CueValidity | subject),
                  data = d,
                  family = "bernoulli")
summary(bl.acc.rt)

#generative additive model
b = bam(log(ReactionTime) ~ 
          CueDirection * CueValidity +  
          ti(trials) +
          ti(trials, by = CueDirection) + 
          ti(trials, by = CueValidity) + 
          s(subject, CueDirection, CueValidity, bs = "re") +
          s(subject, trials, bs = "re"),
        family = gaussian(),
        data = d,
        subset = !is.na(ReactionTime))
summary(b)




