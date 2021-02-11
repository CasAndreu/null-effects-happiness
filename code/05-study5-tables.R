#===============================================================================
# 05-study5-tables.R
# Purpose: To replicate Study 5 tables in the appendix of the paper, where we report the descriptives 
#          and details of the regressions 
# Article: "The (null) effects of happiness on affective polarization, conspiracy 
#           endorsement, and deep fake recognition : Evidence from five survey 
#           experiments in three countries"
# Journal: Political Behavior
# Year: 2020
# Authors: Xudong Yu, Magdalena Wojcieszak, Seungsu Lee, Andreu Casas, Rachid Azrout, Tomasz Gackowski
#===============================================================================


library(dplyr)
library(broom)
library(haven)
library(psych)
library(scales)
library(agricolae)
library(tibble)
options(scipen=999)
options(digits = 10)

# load the data
setwd("~/emotion_pb_clean/data")
dat = read.csv("study5.csv")

# exclude speeders and those who failed the att check
dat = dat %>%
  filter(duration >= .48*median(duration) & attention_check == 2)

# making sure all numeric variables are numeric and all factor variables are factor
dat_numvars <- names(dat)[
  which(!(names(dat) %in% 
            c("condition", "attitude_government","gender", "town_size", "edu")))]
for (v in dat_numvars) {
  dat[,v] = as.numeric(as.character(dat[,v]))
}

dat_facvars = c("condition", "attitude_government","gender", "town_size", "edu")
for (v in dat_facvars) {
  dat[,v] = as.factor(as.character(dat[,v]))
}
dat$condition=relevel(dat$condition, ref = "ControlW")

# create scales
dat = dat %>%
  mutate(
    pids = (pids_1 + pids_2 + pids_3 + pids_4)/4,
    
    
    ft_outgov = ifelse(attitude_government == "supporter", ft_govern_opp, ft_govern_sup),
    ft_outideo = ifelse(ideology > 5, ft_liberal, 
                        ifelse(ideology < 5, ft_conservative, NA)),
    ft_ingov = ifelse(attitude_government == "supporter", ft_govern_sup, ft_govern_opp),
    ft_inideo = ifelse(ideology > 5, ft_conservative, 
                       ifelse(ideology < 5, ft_liberal, NA)),
    
    ft_party_supporter_df = ft_inparty_supporter - ft_outparty_supporter,
    ft_party_df = ft_inparty - ft_outparty,
    ft_gov_df = ft_ingov - ft_outgov,
    ft_ideo_df = ft_inideo - ft_outideo,
    
    outgroup_trust = (outgroup_trust_1 + outgroup_trust_2 + outgroup_trust_3 + outgroup_trust_4)/4,
    social_distance = (social_distance_1 + social_distance_2 + social_distance_3)/3,
    trait_ratings = (trait_1 + trait_2 + (8 - trait_3R) + (8 - trait_4R) + (8 - trait_5R) + trait_6)/6,
    conspiracy = (conspiracy_1 + conspiracy_2 + conspiracy_3 + conspiracy_4 + conspiracy_5 + conspiracy_6)/6
  ) %>%
  dplyr::select(
    -pids_1, -pids_2, -pids_3, -pids_4,
    -ft_govern_sup, -ft_govern_opp, -ft_liberal, -ft_conservative, 
    -outgroup_trust_1, -outgroup_trust_2, -outgroup_trust_3, -outgroup_trust_4,
    -social_distance_1, -social_distance_2, -social_distance_3,
    -trait_1, -trait_2, -trait_3R, -trait_4R, -trait_5R, -trait_6,
    -conspiracy_1, -conspiracy_2, -conspiracy_3, -conspiracy_4, -conspiracy_5, -conspiracy_6
  )

# rescale variables and create an aggregate index of AP
dat = dat %>%
  mutate_at(.funs = scales::rescale, .vars = vars(-duration, -gender, -town_size, -edu, -attention_check, -condition, -attitude_government))

dat = dat %>%
  mutate(ap_index = rowMeans(dat[,c('ft_outparty_supporter', 'ft_outparty', 'ft_outideo', 'ft_outgov', 'outgroup_trust', 'social_distance', 'trait_ratings')], na.rm = T))

# create a intended emotion scores
dat = dat %>%
  mutate(emotion_strength = as.numeric(as.character(ifelse(condition == "ControlW", '', 
                                   ifelse(condition == "ANGW", Angry, 
                                          ifelse(condition == "ANXW", Anxious, Happy))))
  ))
dat$emotion_strength_2 = dat$emotion_strength

moderators = c("pids", "interest", "attitude_government","ideology", "emotion_strength", "emotion_strength_2")

dv_list = c( "ap_index", "ft_immigrant","ft_feminist", "ft_neonazi", "ft_nationalist", "conspiracy", "video",
             "ft_outparty", "ft_outparty_supporter", "ft_outgov", "ft_outideo", "social_distance", "trait_ratings", "outgroup_trust",
             "ft_party_df", "ft_party_supporter_df", "ft_gov_df", "ft_ideo_df")


#### main effects (including additional models presented in the appendix) ####
out_dd = data.frame(term = c("Intercept", "Angry Writing", "Anxious Writing", "Happy Writing","N","R-squared"))
for (dv in dv_list) {
  simple_formula <- formula(paste0(
    dv, " ~ ", "condition" 
  ))
  
  model_fit <- lm(simple_formula, data = dat)
  
  summary_table = tidy(model_fit) %>%
    as.data.frame()
  
  summary_table$estimate = format(round(summary_table$estimate,3), nsmall = 3)
  summary_table$std.error = format(round(summary_table$std.error,3), nsmall = 3)
  
  summary_table = summary_table %>%
    dplyr::select(-statistic, -term)
  summary_table$p.value = format(round(summary_table$p.value,3), nsmall = 3)
  
  Num_o = c(stats::nobs(model_fit), NA,NA) 
  summary_table = rbind(summary_table, Num_o)
  r2 = c(format(round(glance(model_fit)$r.squared,3),nsmall = 3), NA,NA)
  summary_table = rbind(summary_table, r2)
  
  names(summary_table)[names(summary_table) == "estimate"] <- dv
  out_dd <- cbind(out_dd, summary_table)
}


#### moderations ####
out_dd_pids = data.frame(term = c("Intercept", "Angry Writing", "Anxious Writing", "Happy Writing","Pids", 
                                  "Angry Writing × Pids", "Anxious Writing × Pids" , "Happy Writing × Pids" , 
                                  "N","R-squared"))
out_dd_interest = data.frame(term = c("Intercept", "Angry Writing", "Anxious Writing", "Happy Writing","Interest", 
                                      "Angry Writing × Interest", "Anxious Writing × Interest" , "Happy Writing × Interest" ,  
                                      "N","R-squared"))
out_dd_attgov = data.frame(term = c("Intercept", "Angry Writing", "Anxious Writing", "Happy Writing","Att. Gov.(Supporter)", 
                                          "Angry Writing × Att. Gov.(Supporter)", "Anxious Writing × Att. Gov.(Supporter)" , "Happy Writing × Att. Gov.(Supporter)" ,  
                                          "N","R-squared"))
out_dd_ideology = data.frame(term = c("Intercept", "Angry Writing", "Anxious Writing", "Happy Writing","Ideology", 
                                      "Angry Writing × Ideology", "Anxious Writing × Ideology" , "Happy Writing × Ideology" ,  
                                      "N","R-squared"))
out_dd_emotion_h = data.frame(term = c("Intercept", "Angry Writing", "Anxious Writing", "Emotion Strength",
                                     "Angry Writing × Emotion Strength", "Anxious Writing × Emotion Strength" , 
                                     "N","R-squared"))
out_dd_emotion_ang = data.frame(term = c("Intercept", "Anxious Writing", "Happy Writing", "Emotion Strength",
                                         "Anxious Writing × Emotion Strength", "Happy Writing × Emotion Strength" , 
                                         "N","R-squared"))

for (moderator in moderators) {
  
  moderator_formulas <- c()
  if (moderator != "emotion_strength" & moderator != "emotion_strength_2"){
    for (dv in dv_list) {
      mod_formula <- formula(paste0(
        dv, " ~ ", "condition * ", moderator
      ))
      moderator_formulas <- c(moderator_formulas, mod_formula)
    }
  } else if (moderator == "emotion_strength"){
    for (dv in dv_list) {
      mod_formula <- formula(paste0(
        dv, " ~ ", "relevel(condition, ref = 'HappyW') * ", moderator
      ))
      moderator_formulas <- c(moderator_formulas, mod_formula)
    }
  } else if (moderator == "emotion_strength_2"){
    for (dv in dv_list) {
      mod_formula <- formula(paste0(
        dv, " ~ ", "relevel(condition, ref = 'ANGW') * ", moderator
      ))
      moderator_formulas <- c(moderator_formulas, mod_formula)
    }
  }
  
  for (i in 1:length(moderator_formulas)) {
    mod_formula <- moderator_formulas[[i]]
    
    model_fit <- lm(mod_formula, data = dat)
    
    summary_table = tidy(model_fit) %>%
      as.data.frame()
    
    summary_table$estimate = format(round(summary_table$estimate,3), nsmall = 3)
    summary_table$std.error = format(round(summary_table$std.error,3), nsmall = 3)
    
    summary_table = summary_table %>%
      dplyr::select(-statistic, -term)
    summary_table$p.value = format(round(summary_table$p.value,3), nsmall = 3)
    
    Num_o = c(stats::nobs(model_fit), NA,NA) 
    summary_table = rbind(summary_table, Num_o)
    r2 = c(format(round(glance(model_fit)$r.squared,3),nsmall = 3), NA,NA)
    summary_table = rbind(summary_table, r2)

    
    names(summary_table)[names(summary_table) == "estimate"] <- dv_list[i]
    
    if (moderator == "pids"){
      out_dd_pids <- cbind(out_dd_pids, summary_table)
    } else if (moderator == "interest") {
      out_dd_interest <- cbind(out_dd_interest, summary_table)
    } else if (moderator == "attitude_government") {
      out_dd_attgov <- cbind(out_dd_attgov, summary_table)
    } else if (moderator == "ideology") {
      out_dd_ideology <- cbind(out_dd_ideology, summary_table)
    } else if (moderator == "emotion_strength") {
      out_dd_emotion_h <- cbind(out_dd_emotion_h, summary_table)
    } else if (moderator == "emotion_strength_2") {
      out_dd_emotion_ang <- cbind(out_dd_emotion_ang, summary_table)
    }
    
  }
  
}

#### fdr adjustment ####
out_dd_all = list(out_dd, out_dd_pids, out_dd_interest, out_dd_attgov, out_dd_ideology, out_dd_emotion_h, out_dd_emotion_ang)
pvalue_all = NULL
for (i in (1:7)){
  for (j in (seq(4, 55, 3))){
    pvalue_all = rbind(pvalue_all,out_dd_all[[i]][j])
  }
}

pvalue_all = pvalue_all %>%
  mutate(p.fdr = p.adjust(p.value, method = 'BH'),
         p.sig = ifelse(p.fdr < .05,"*",""))


for (i in (1:18)){
  out_dd = out_dd %>%
    add_column(p.fdr = pvalue_all$p.fdr[c((6*i-5):(i*6))], 
               .after = 4*i,
               .name_repair = "minimal") %>%
    mutate(!!dv_list[i] := ifelse(is.na(out_dd[ ,3]), out_dd[ ,(4*i-2)], paste0(out_dd[ ,(4*i-2)], replace(pvalue_all$p.sig[c((6*i-5):(i*6))], is.na(pvalue_all$p.sig[c((6*i-5):(i*6))]),""), " (", out_dd[ ,(4*i-1)], ")"))
    )
  
  out_dd_pids = out_dd_pids %>%
    add_column(p.fdr = pvalue_all$p.fdr[c((99+10*i):(108+10*i))], 
               .after = 4*i,
               .name_repair = "minimal") %>%
    mutate(!!dv_list[i] := ifelse(is.na(out_dd_pids[ ,3]), out_dd_pids[ ,(4*i-2)], paste0(out_dd_pids[ ,(4*i-2)], replace(pvalue_all$p.sig[c((99+10*i):(108+10*i))], is.na(pvalue_all$p.sig[c((99+10*i):(108+10*i))]),""), " (", out_dd_pids[ ,(4*i-1)], ")"))
    )
  
  out_dd_interest = out_dd_interest %>%
    add_column(p.fdr = pvalue_all$p.fdr[c((279+10*i):(288+10*i))], 
               .after = 4*i,
               .name_repair = "minimal") %>%
    mutate(!!dv_list[i] := ifelse(is.na(out_dd_interest[ ,3]), out_dd_interest[ ,(4*i-2)], paste0(out_dd_interest[ ,(4*i-2)], replace(pvalue_all$p.sig[c((279+10*i):(288+10*i))], is.na(pvalue_all$p.sig[c((279+10*i):(288+10*i))]),""), " (", out_dd_interest[ ,(4*i-1)], ")"))
    )
  
  out_dd_attgov = out_dd_attgov %>%
    add_column(p.fdr = pvalue_all$p.fdr[c((459+10*i):(468+10*i))], 
               .after = 4*i,
               .name_repair = "minimal") %>%
    mutate(!!dv_list[i] := ifelse(is.na(out_dd_attgov[ ,3]), out_dd_attgov[ ,(4*i-2)], paste0(out_dd_attgov[ ,(4*i-2)], replace(pvalue_all$p.sig[c((459+10*i):(468+10*i))], is.na(pvalue_all$p.sig[c((459+10*i):(468+10*i))]),""), " (", out_dd_attgov[ ,(4*i-1)], ")"))
    )
  
  out_dd_ideology = out_dd_ideology %>%
    add_column(p.fdr = pvalue_all$p.fdr[c((639+10*i):(648+10*i))], 
               .after = 4*i,
               .name_repair = "minimal") %>%
    mutate(!!dv_list[i] := ifelse(is.na(out_dd_ideology[ ,3]), out_dd_ideology[ ,(4*i-2)], paste0(out_dd_ideology[ ,(4*i-2)], replace(pvalue_all$p.sig[c((639+10*i):(648+10*i))], is.na(pvalue_all$p.sig[c((639+10*i):(648+10*i))]),""), " (", out_dd_ideology[ ,(4*i-1)], ")"))
    )
  
  out_dd_emotion_h = out_dd_emotion_h %>%
    add_column(p.fdr = pvalue_all$p.fdr[c((821+8*i):(828+8*i))], 
               .after = 4*i,
               .name_repair = "minimal") %>%
    mutate(!!dv_list[i] := ifelse(is.na(out_dd_emotion_h[ ,3]), out_dd_emotion_h[ ,(4*i-2)], paste0(out_dd_emotion_h[ ,(4*i-2)], replace(pvalue_all$p.sig[c((821+8*i):(828+8*i))], is.na(pvalue_all$p.sig[c((821+8*i):(828+8*i))]),""), " (", out_dd_emotion_h[ ,(4*i-1)], ")"))
    )
  
  out_dd_emotion_ang = out_dd_emotion_ang %>%
    add_column(p.fdr = pvalue_all$p.fdr[c((965+8*i):(972+8*i))], 
               .after = 4*i,
               .name_repair = "minimal") %>%
    mutate(!!dv_list[i] := ifelse(is.na(out_dd_emotion_ang[ ,3]), out_dd_emotion_ang[ ,(4*i-2)], paste0(out_dd_emotion_ang[ ,(4*i-2)], replace(pvalue_all$p.sig[c((965+8*i):(972+8*i))], is.na(pvalue_all$p.sig[c((965+8*i):(972+8*i))]),""), " (", out_dd_emotion_ang[ ,(4*i-1)], ")"))
    )
}

# create the tables
out_dd_final = out_dd[ , c(1, seq(2, 70, 4))] %>%
  rename(
    ` ` = term,
    `Affective polarization` = ap_index,
    `FT immigrants` = ft_immigrant,
    `FT feminists` = ft_feminist,
    `FT neo-Nazis` = ft_neonazi,
    `FT nationalists` = ft_nationalist,
    Conspiracy = conspiracy,
    `Deep fake` = video,
    `FT outparty` = ft_outparty, 
    `FT outparty supporters` = ft_outparty_supporter, 
    `FT out-government` = ft_outgov,
    `FT out-ideologues` = ft_outideo,
    `Social distance` = social_distance, 
    `Trait ratings` = trait_ratings, 
    `Outgroup trust` = outgroup_trust,
    `FT party (subtractive)` = ft_party_df, 
    `FT party supporters (subtractive)` = ft_party_supporter_df, 
    `FT government (subtractive)` = ft_gov_df,
    `FT ideology (subtractive)` = ft_ideo_df
  )


out_dd_pids_final = out_dd_pids[ , c(1, seq(2, 70, 4))] %>%
  rename(
    ` ` = term,
    `Affective polarization` = ap_index,
    `FT immigrants` = ft_immigrant,
    `FT feminists` = ft_feminist,
    `FT neo-Nazis` = ft_neonazi,
    `FT nationalists` = ft_nationalist,
    Conspiracy = conspiracy,
    `Deep fake` = video,
    `FT outparty` = ft_outparty, 
    `FT outparty supporters` = ft_outparty_supporter, 
    `FT out-government` = ft_outgov,
    `FT out-ideologues` = ft_outideo,
    `Social distance` = social_distance, 
    `Trait ratings` = trait_ratings, 
    `Outgroup trust` = outgroup_trust,
    `FT party (subtractive)` = ft_party_df, 
    `FT party supporters (subtractive)` = ft_party_supporter_df, 
    `FT government (subtractive)` = ft_gov_df,
    `FT ideology (subtractive)` = ft_ideo_df
  )

out_dd_interest_final = out_dd_interest[ , c(1, seq(2, 70, 4))] %>%
  rename(
    ` ` = term,
    `Affective polarization` = ap_index,
    `FT immigrants` = ft_immigrant,
    `FT feminists` = ft_feminist,
    `FT neo-Nazis` = ft_neonazi,
    `FT nationalists` = ft_nationalist,
    Conspiracy = conspiracy,
    `Deep fake` = video,
    `FT outparty` = ft_outparty, 
    `FT outparty supporters` = ft_outparty_supporter, 
    `FT out-government` = ft_outgov,
    `FT out-ideologues` = ft_outideo,
    `Social distance` = social_distance, 
    `Trait ratings` = trait_ratings, 
    `Outgroup trust` = outgroup_trust,
    `FT party (subtractive)` = ft_party_df, 
    `FT party supporters (subtractive)` = ft_party_supporter_df, 
    `FT government (subtractive)` = ft_gov_df,
    `FT ideology (subtractive)` = ft_ideo_df
  )

out_dd_attgov_final = out_dd_attgov[ , c(1, seq(2, 70, 4))] %>%
  rename(
    ` ` = term,
    `Affective polarization` = ap_index,
    `FT immigrants` = ft_immigrant,
    `FT feminists` = ft_feminist,
    `FT neo-Nazis` = ft_neonazi,
    `FT nationalists` = ft_nationalist,
    Conspiracy = conspiracy,
    `Deep fake` = video,
    `FT outparty` = ft_outparty, 
    `FT outparty supporters` = ft_outparty_supporter, 
    `FT out-government` = ft_outgov,
    `FT out-ideologues` = ft_outideo,
    `Social distance` = social_distance, 
    `Trait ratings` = trait_ratings, 
    `Outgroup trust` = outgroup_trust,
    `FT party (subtractive)` = ft_party_df, 
    `FT party supporters (subtractive)` = ft_party_supporter_df, 
    `FT government (subtractive)` = ft_gov_df,
    `FT ideology (subtractive)` = ft_ideo_df
  )

out_dd_ideology_final = out_dd_ideology[ , c(1, seq(2, 70, 4))] %>%
  rename(
    ` ` = term,
    `Affective polarization` = ap_index,
    `FT immigrants` = ft_immigrant,
    `FT feminists` = ft_feminist,
    `FT neo-Nazis` = ft_neonazi,
    `FT nationalists` = ft_nationalist,
    Conspiracy = conspiracy,
    `Deep fake` = video,
    `FT outparty` = ft_outparty, 
    `FT outparty supporters` = ft_outparty_supporter, 
    `FT out-government` = ft_outgov,
    `FT out-ideologues` = ft_outideo,
    `Social distance` = social_distance, 
    `Trait ratings` = trait_ratings, 
    `Outgroup trust` = outgroup_trust,
    `FT party (subtractive)` = ft_party_df, 
    `FT party supporters (subtractive)` = ft_party_supporter_df, 
    `FT government (subtractive)` = ft_gov_df,
    `FT ideology (subtractive)` = ft_ideo_df
  )

out_dd_emotion_h_final = out_dd_emotion_h[ , c(1, seq(2, 70, 4))] %>%
  rename(
    ` ` = term,
    `Affective polarization` = ap_index,
    `FT immigrants` = ft_immigrant,
    `FT feminists` = ft_feminist,
    `FT neo-Nazis` = ft_neonazi,
    `FT nationalists` = ft_nationalist,
    Conspiracy = conspiracy,
    `Deep fake` = video,
    `FT outparty` = ft_outparty, 
    `FT outparty supporters` = ft_outparty_supporter, 
    `FT out-government` = ft_outgov,
    `FT out-ideologues` = ft_outideo,
    `Social distance` = social_distance, 
    `Trait ratings` = trait_ratings, 
    `Outgroup trust` = outgroup_trust,
    `FT party (subtractive)` = ft_party_df, 
    `FT party supporters (subtractive)` = ft_party_supporter_df, 
    `FT government (subtractive)` = ft_gov_df,
    `FT ideology (subtractive)` = ft_ideo_df
  )

out_dd_emotion_ang_final = out_dd_emotion_ang[ , c(1, seq(2, 70, 4))] %>%
  rename(
    ` ` = term,
    `Affective polarization` = ap_index,
    `FT immigrants` = ft_immigrant,
    `FT feminists` = ft_feminist,
    `FT neo-Nazis` = ft_neonazi,
    `FT nationalists` = ft_nationalist,
    Conspiracy = conspiracy,
    `Deep fake` = video,
    `FT outparty` = ft_outparty, 
    `FT outparty supporters` = ft_outparty_supporter, 
    `FT out-government` = ft_outgov,
    `FT out-ideologues` = ft_outideo,
    `Social distance` = social_distance, 
    `Trait ratings` = trait_ratings, 
    `Outgroup trust` = outgroup_trust,
    `FT party (subtractive)` = ft_party_df, 
    `FT party supporters (subtractive)` = ft_party_supporter_df, 
    `FT government (subtractive)` = ft_gov_df,
    `FT ideology (subtractive)` = ft_ideo_df
  )

View(out_dd_final)
out_dd_final_main = out_dd_final[ ,c(1:8)]
out_dd_final_indi_diff = out_dd_final[ ,c(1,9:19)]

View(out_dd_pids_final)
out_dd_pids_final_main = out_dd_pids_final[ ,c(1:8)]
out_dd_pids_final_indi_diff = out_dd_pids_final[ ,c(1,9:19)]

View(out_dd_interest_final)
out_dd_interest_final_main = out_dd_interest_final[ ,c(1:8)]
out_dd_interest_final_indi_diff = out_dd_interest_final[ ,c(1,9:19)]

View(out_dd_attgov_final)
out_dd_attgov_final_main = out_dd_attgov_final[ ,c(1:8)]
out_dd_attgov_final_indi_diff = out_dd_attgov_final[ ,c(1,9:19)]

View(out_dd_ideology_final)
out_dd_ideology_final_main = out_dd_ideology_final[ ,c(1:8)]
out_dd_ideology_final_indi_diff = out_dd_ideology_final[ ,c(1,9:19)]

View(out_dd_emotion_h_final)
out_dd_emotion_h_final_main = out_dd_emotion_h_final[ ,c(1:8)]
out_dd_emotion_h_final_indi_diff = out_dd_emotion_h_final[ ,c(1,9:19)]

View(out_dd_emotion_ang_final)
out_dd_emotion_ang_final_main = out_dd_emotion_ang_final[ ,c(1:8)]
out_dd_emotion_ang_final_indi_diff = out_dd_emotion_ang_final[ ,c(1,9:19)]

write.table(out_dd_final_main, file = "study5_maineffecits_main.txt", sep = ",", quote = FALSE, row.names = F, na = "")
write.table(out_dd_final_indi_diff, file = "study5_maineffecits_individual_and_difference.txt", sep = ",", quote = FALSE, row.names = F, na = "")

write.table(out_dd_pids_final_main, file = "study5_moderation_pids_main.txt", sep = ",", quote = FALSE, row.names = F, na = "")
write.table(out_dd_pids_final_indi_diff, file = "study5_moderation_pids_individual_and_difference.txt", sep = ",", quote = FALSE, row.names = F, na = "")

write.table(out_dd_interest_final_main, file = "study5_moderation_interest_main.txt", sep = ",", quote = FALSE, row.names = F, na = "")
write.table(out_dd_interest_final_indi_diff, file = "study5_moderation_interest_individual_and_difference.txt", sep = ",", quote = FALSE, row.names = F, na = "")

write.table(out_dd_attgov_final_main, file = "study5_moderation_attgov_main.txt", sep = ",", quote = FALSE, row.names = F, na = "")
write.table(out_dd_attgov_final_indi_diff, file = "study5_moderation_attgov_individual_and_difference.txt", sep = ",", quote = FALSE, row.names = F, na = "")

write.table(out_dd_ideology_final_main, file = "study5_moderation_ideology_main.txt", sep = ",", quote = FALSE, row.names = F, na = "")
write.table(out_dd_ideology_final_indi_diff, file = "study5_moderation_ideology_individual_and_difference.txt", sep = ",", quote = FALSE, row.names = F, na = "")

write.table(out_dd_emotion_h_final_main, file = "study5_moderation_emotion_happiness_as_ref_main.txt", sep = ",", quote = FALSE, row.names = F, na = "")
write.table(out_dd_emotion_h_final_indi_diff, file = "study5_moderation_emotion_happiness_as_ref_individual_and_difference.txt", sep = ",", quote = FALSE, row.names = F, na = "")

write.table(out_dd_emotion_ang_final_main, file = "study5_moderation_emotion_angry_as_ref_main.txt", sep = ",", quote = FALSE, row.names = F, na = "")
write.table(out_dd_emotion_ang_final_indi_diff, file = "study5_moderation_emotion_angry_as_ref_individual_and_difference.txt", sep = ",", quote = FALSE, row.names = F, na = "")

####descriptives 

des = dat %>%
  group_by(condition) %>%
  summarise(`Affective polarization` = mean(ap_index),`Affective polarization_sd` = sd(ap_index), 
            `FT immigrants` = mean(ft_immigrant), `FT immigrants_sd` = sd(ft_immigrant),
            `FT feminists` = mean(ft_feminist), `FT feminists_sd` = sd(ft_feminist),
            `FT neo-Nazis` = mean(ft_neonazi), `FT neo-Nazis_sd` = sd(ft_neonazi),
            `FT nationalists` = mean(ft_nationalist), `FT nationalists_sd` = sd(ft_nationalist),
            Conspiracy = mean(conspiracy), Conspiracy_sd = sd(conspiracy),
            `Deep fake` = mean(video),  `Deep fake_sd` = sd(video),
            `FT outparty` = mean(ft_outparty), `FT outparty_sd` = sd(ft_outparty), 
            `FT outparty supporters` = mean(ft_outparty_supporter), `FT outparty supporters_sd` = sd(ft_outparty_supporter), 
            `FT out-government` = mean(ft_outgov),  `FT out-government_sd` = sd(ft_outgov),
            `FT out-ideologues` = mean(ft_outideo, na.rm = T), `FT out-ideologues_sd` = sd(ft_outideo, na.rm = T),
            `Social distance` = mean(social_distance),  `Social distance_sd` = sd(social_distance), 
            `Trait ratings` = mean(trait_ratings),  `Trait ratings_sd` = sd(trait_ratings), 
            `Outgroup trust` = mean(outgroup_trust), `Outgroup trust_sd` = sd(outgroup_trust),
            `FT party (subtractive)` = mean(ft_party_df), `FT party (subtractive)_sd` = sd(ft_party_df), 
            `FT party supporters (subtractive)` = mean(ft_party_supporter_df),  `FT party supporters (subtractive)_sd` = sd(ft_party_supporter_df), 
            `FT government (subtractive)` = mean(ft_gov_df), `FT government (subtractive)_sd` = sd(ft_gov_df), 
            `FT ideology (subtractive)` = mean(ft_ideo_df, na.rm = T), `FT ideology (subtractive)_sd` = sd(ft_ideo_df, na.rm = T)
  )
des = as.data.frame(des)


for (i in seq(2, 36, 2)){
  des[ ,i] = paste0(format(round(des[ ,i], 3), nsmall = 3), " (",format(round(des[ , i+1], 3), nsmall = 3), ")")
}

des = des[ ,c(1, seq(2, 36, 2))]

des = des %>%
  rename(Condition = condition) %>%
  mutate(Condition = recode(Condition,
                            ControlW = "Control Writing",
                            ANGW = "Angry Writing",
                            ANXW = "Anxious Writing",
                            HappyW = "Happy Writing"
  ))

des_main = des[ ,c(1:8)]
des_indi_diff = des[ ,c(1,9:19)]

write.table(des_main, file = "study5_descriptives_main_by_condition.txt", sep = ",", quote = FALSE, row.names = F, na = "")
write.table(des_indi_diff, file = "study5_descriptives_individual_and_difference_by_condition.txt", sep = ",", quote = FALSE, row.names = F, na = "")


