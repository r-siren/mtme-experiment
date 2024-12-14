#### Replication code for: Measurement Error When Surveying Issue Positions: A MultiTrait MultiError Approach ####

# Admin -----------------------------

library(tidyverse)
library(lavaan)
library(haven)
library(ggcorrplot)
library(viridis)
library(blavaan)
library(gtsummary)
library(cardx)

# Table 3 -----------------------------

# import
data_raw <- read_sav("2023-08-25mtme_clean.sav")

data_raw %>% 
  count(MTME)


# Cleaning -----------------------------

# Rename and make id
data <- data_raw %>% 
  rename_all(~str_to_lower(.)) %>% 
  mutate(id = row_number())

# Bring together t1 and t2 data

# Make all variable names
var_names <- expand.grid(1:10, c("f1", "f2", "f3", "f4")) %>% 
  mutate(nm = str_c(Var2, "_", Var1))

# Make function to bring them together
var_merge <- function(var_name) {
  nm_t1 <- str_c(var_name, "_t1")
  nm_t2 <- str_c(var_name, "_t2")
  var <- data[[nm_t1]]
  var <- ifelse(is.na(var), data[[nm_t2]], var)
  var
}

# Create data
merged_data <- map(var_names$nm, var_merge) %>% 
  reduce(cbind) %>%
  as.data.frame() %>% 
  setNames(var_names$nm) %>% 
  mutate(id = row_number())

# Merge with original data
data <- full_join(data, merged_data)

# Reverse code
data2 <- data %>% 
  mutate_at(vars(str_c("f1_", 1:10)),
            ~ 6 - .) %>% 
  mutate_at(vars(str_c("f3_", 1:10)),
            ~ 12 - .)   

# Make new variables for mtme
data2 <- data2 %>%
  mutate(polint = ifelse(polinterest == 1, 1, 0),
         inteff = ifelse(int_eff_sum <= 6, 0, 1),
         time_days_c = time_days - mean(time_days)) 


# Fix variable names
var_names2 <- expand.grid(c("f1", "f2", "f3", "f4"), 1:10) %>% 
  mutate(nm = str_c(Var1, "_", Var2)) %>% 
  .[["nm"]]

q_labs <- c("Tax", "Corporations", "Rural", "Prison", "Refugees",
            "Gay", "Enviornment", "EU", "Digitalization", "Beef")


# Figure 2 -----------------------------

normalize <- function(x, na.rm = TRUE) {
  return((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)))
}

data2 %>% 
  select(var_names2) %>% 
  mutate_all(~normalize(.)) %>% 
  summarise_all(.funs = 
                  list(mean = ~mean(., na.rm = T),
                       se = ~sd(., na.rm = T)/sqrt(n()))) %>%
  gather(value, key) %>%  
  mutate(topic = case_when(
    str_detect(value, "_1_") ~ "Tax",
    str_detect(value, "_2") ~ "Corporations",
    str_detect(value, "_3") ~ "Rural",
    str_detect(value, "_4") ~ "Prison",
    str_detect(value, "_5") ~ "Refugees",
    str_detect(value, "_6") ~ "Gay",
    str_detect(value, "_7") ~ "Enviornment",
    str_detect(value, "_8") ~ "EU",
    str_detect(value, "_9") ~ "Digitalization",
    str_detect(value, "_10") ~ "Beef"),
    form = str_extract(value, "f."),
    form2 = case_when(form == "f1" ~ "5 point - Postive (F1)",
                      form == "f2" ~ "5 point - Negative (F2)",
                      form == "f3" ~ "11 point - Postive (F3)",
                      form == "f4" ~ "11 point - Negative (F4)"),
    scale = ifelse(form %in% c("f1", "f2"), "5 point scale", "11 point scale"),
    acq = ifelse(form %in% c("f1", "f3"), "Positive first", "Positive last"),
    value = str_remove(value, "f.+_[0-9]+_")) %>% 
  pivot_wider(names_from = value, values_from = key) %>% 
  mutate(lci = mean - (1.96 * se),
         uci = mean + (1.96 * se),
         form2 = fct_rev(form2),
         topic_fct = as.factor(topic),
         topic_fct = fct_relevel(topic_fct, q_labs) %>% fct_rev()) %>% 
  ggplot(aes(mean, topic_fct, color = form2)) +
  geom_pointrange(aes(xmin = lci, xmax = uci),
                  position = position_dodge(-0.5), 
                  line_width = 1, size = 0.15) +
  labs(x = "Average (rescaled negative to postive 0-1)",
       y = "Topic",
       color = "Form") +
  theme_bw() +
  viridis::scale_color_viridis(discrete = T)


# Figure 3 -----------------------------

data2 %>% 
  select(var_names2[length(var_names2):1]) %>% 
  rename_all(~str_replace(., "f1", "F1_")) %>% 
  rename_all(~str_replace(., "f2", "F2_")) %>% 
  rename_all(~str_replace(., "f3", "F3_")) %>% 
  rename_all(~str_replace(., "f4", "F4_")) %>% 
  rename_all(~str_replace(., "_1$", "Tax")) %>% 
  rename_all(~str_replace(., "_2", "Corporations")) %>% 
  rename_all(~str_replace(., "_3", "Rural")) %>% 
  rename_all(~str_replace(., "_4", "Prison")) %>% 
  rename_all(~str_replace(., "_5", "Refugees")) %>% 
  rename_all(~str_replace(., "_6", "Gay")) %>% 
  rename_all(~str_replace(., "_7", "Enviornment")) %>% 
  rename_all(~str_replace(., "_8", "EU")) %>% 
  rename_all(~str_replace(., "_9", "Digitalization")) %>% 
  rename_all(~str_replace(., "_10", "Beef")) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  ggcorrplot::ggcorrplot(tl.cex = 7) +
  viridis::scale_fill_viridis() +
  labs(fill = "Correlation")


# Running the MTME model -----------------------------

mtme1 <- "T1 =~ 1*f1_1 + f2_1 + f3_1 + f4_1
        T2 =~ 1*f1_2 + f2_2 + f3_2 + f4_2
        T3 =~ 1*f1_3 + f2_3 + f3_3 + f4_3
        T4 =~ 1*f1_4 + f2_4 + f3_4 + f4_4
        T5 =~ 1*f1_5 + f2_5 + f3_5 + f4_5
        T6 =~ 1*f1_6 + f2_6 + f3_6 + f4_6
        T7 =~ 1*f1_7 + f2_7 + f3_7 + f4_7
        T8 =~ 1*f1_8 + f2_8 + f3_8 + f4_8
        T9 =~ 1*f1_9 + f2_9 + f3_9 + f4_9
        T10 =~ 1*f1_10 + f2_10 + f3_10 + f4_10
        
        M1 =~ 1*f1_1 + 1*f1_2 + 1*f1_3 + 1*f1_4 + 1*f1_5 + 1*f1_6 + 1*f1_7 + 1*f1_8 + 1*f1_9 + 1*f1_10 + 1*f2_1 + 1*f2_2 + 1*f2_3 + 1*f2_4 + 1*f2_5 + 1*f2_6 + 1*f2_7 + 1*f2_8 + 1*f2_9 + 1*f2_10
        M2 =~ 1*f3_1 + 1*f3_2 + 1*f3_3 + 1*f3_4 + 1*f3_5 + 1*f3_6 + 1*f3_7 + 1*f3_8 + 1*f3_9 + 1*f3_10 + 1*f4_1 + 1*f4_2 + 1*f4_3 + 1*f4_4 + 1*f4_5 + 1*f4_6 + 1*f4_7 + 1*f4_8 + 1*f4_9 + 1*f4_10
        
        A =~ 1*f1_1 + 1*f1_2 + 1*f1_3 + 1*f1_4 + 1*f1_5 + 1*f1_6 + 1*f1_7 + 1*f1_8 + 1*f1_9 + 1*f1_10 + 1*f3_1 + 1*f3_2 + 1*f3_3 + 1*f3_4 + 1*f3_5 + 1*f3_6 + 1*f3_7 + 1*f3_8 + 1*f3_9 + 1*f3_10
        
        # estimate variances of latent variables

        M1 ~~ NA*M1
        M2 ~~ NA*M2
        A ~~ NA*A
        
        
        T1 ~~ NA*T1
        T2 ~~ NA*T2
        T3 ~~ NA*T3
        T4 ~~ NA*T4
        T5 ~~ NA*T5
        T6 ~~ NA*T6
        T7 ~~ NA*T7
        T8 ~~ NA*T8
        T9 ~~ NA*T9
        T10 ~~ NA*T10
        
        # fix intercepts and estimate means
        f1_1 + f2_1 + f3_1 + f4_1 ~ 0*0
        f1_2 + f2_2 + f3_2 + f4_2 ~ 0*0
        f1_3 + f2_3 + f3_3 + f4_3 ~ 0*0
        f1_4 + f2_4 + f3_4 + f4_4 ~ 0*0
        f1_5 + f2_5 + f3_5 + f4_5 ~ 0*0
        f1_6 + f2_6 + f3_6 + f4_6 ~ 0*0
        f1_7 + f2_7 + f3_7 + f4_7 ~ 0*0
        f1_8 + f2_8 + f3_8 + f4_8 ~ 0*0
        f1_9 + f2_9 + f3_9 + f4_9 ~ 0*0
        f1_10 + f2_10 + f3_10 + f4_10 ~ 0*0
        
        T1 + T2 + T3 + T4 + T5 + T6 + T7 + T8 + T9 + T10 + M1 + M2 + A ~ NA*0
        
        # no correlations between methods
        M1 ~~ 0*M2
        M1 ~~ 0*A
        M2 ~~ 0*A
        
        # no correlations between methods and traits
        M1 ~~ 0*T1
        M1 ~~ 0*T2
        M1 ~~ 0*T3
        M1 ~~ 0*T4
        M1 ~~ 0*T5
        M1 ~~ 0*T6
        M1 ~~ 0*T7
        M1 ~~ 0*T8
        M1 ~~ 0*T9
        M1 ~~ 0*T10

        M2 ~~ 0*T1
        M2 ~~ 0*T2
        M2 ~~ 0*T3
        M2 ~~ 0*T4
        M2 ~~ 0*T5
        M2 ~~ 0*T6
        M2 ~~ 0*T7
        M2 ~~ 0*T8
        M2 ~~ 0*T9
        M2 ~~ 0*T10
        
        
        A ~~ 0*T1
        A ~~ 0*T2
        A ~~ 0*T3
        A ~~ 0*T4
        A ~~ 0*T5
        A ~~ 0*T6
        A ~~ 0*T7
        A ~~ 0*T8
        A ~~ 0*T9
        A ~~ 0*T10
"

fit_mtme1 <- cfa(mtme1, data = data2, std.lv = TRUE, missing="ml",
                 auto.fix.first = FALSE, auto.var = TRUE, 
                 em.h1.iter.max = 10000) 

summary(fit_mtme1, standardized = TRUE)

lavaan::fitmeasures(fit_mtme1)

# look at predicted scores
mtme_fit_est <- lavaan::parameterestimates(fit_mtme1, standardized = T)

mtme_est_qual <- mtme_fit_est %>%
  filter(op == "=~" | op == "~~") %>%
  filter(!(op == "~~" & str_detect(lhs, "T|M|A"))) %>%
  mutate(Trait = case_when(
    str_detect(rhs, "_1$") ~ "Tax",
    str_detect(rhs, "_2") ~ "Corporations",
    str_detect(rhs, "_3") ~ "Rural",
    str_detect(rhs, "_4") ~ "Prison",
    str_detect(rhs, "_5") ~ "Refugees",
    str_detect(rhs, "_6") ~ "Gay",
    str_detect(rhs, "_7") ~ "Enviornment",
    str_detect(rhs, "_8") ~ "EU",
    str_detect(rhs, "_9") ~ "Digitalization",
    str_detect(rhs, "_10") ~ "Beef",
  ),
  group = case_when(
    str_detect(rhs, "f1|f2") ~ "5 point scale",
    str_detect(rhs, "f3|f4") ~ "11 point scale",),
  source = case_when(str_detect(lhs, "T") ~ "Trait",
                     str_detect(lhs, "M") ~ "Method",
                     str_detect(lhs, "A") ~ "Acquiescence",
                     TRUE ~ "Random error"),
  qual = ifelse(source != "Random error", std.all^2, std.all)) %>%
  select(-op, -est, -std.lv)

# summarise quality
mtme_est_qual %>% 
  group_by(source) %>% 
  summarise(qual = mean(qual))

mtme_est_qual %>% 
  group_by(source, group) %>% 
  summarise(qual = mean(qual))

mtme_est_qual %>% 
  group_by(source, Trait) %>% 
  summarise(qual = mean(qual)) %>% 
  filter(source == "Trait") %>% 
  arrange(qual)

mtme_est_qual %>% 
  group_by(Trait, source, group) %>% 
  summarise(qual = mean(qual))


# Figure 4 -----------------------------

mtme_est_qual %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  group_by(group, source) %>%
  summarise(group_qual = sum(qual)/20) %>%
  ggplot(aes(group, group_qual, fill = source)) +
  geom_bar(stat = "identity") +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Measurement type",
       fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5,
                                   hjust = 0.1))

mtme_est_qual %>%
  mutate(source = fct_relevel(source, "Trait"),
         Trait = fct_rev(Trait)) %>%
  group_by(Trait, source) %>%
  summarise(group_qual = sum(qual)/4) %>%
  mutate(Trait_fct = as.factor(Trait),
         Trait_fct = fct_relevel(Trait_fct, q_labs)) %>% 
  ggplot(aes(Trait_fct, group_qual, fill = source)) +
  geom_bar(stat = "identity") +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Topic",
       fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5,
                                   hjust = 0.1))


# Figure 5 -----------------------------

mtme_est_qual %>%
  mutate(source = fct_relevel(source, "Trait"),
         Trait = fct_rev(Trait)) %>%
  group_by(Trait, source, group) %>% 
  summarise(qual = sum(qual)/2) %>%
  
  mutate(Trait_fct = as.factor(Trait),
         Trait_fct = fct_relevel(Trait_fct, q_labs)) %>% 
  ggplot(aes(group, qual, fill = source)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Trait_fct, nrow = 1) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Response scale",
       fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5,
                                   hjust = 0.1))


# Groupwise MTME -----------------------------

mtme2 <- "T1 =~ 1*f1_1 + f2_1 + f3_1 + f4_1
        T2 =~ 1*f1_2 + f2_2 + f3_2 + f4_2
        T3 =~ 1*f1_3 + f2_3 + f3_3 + f4_3
        T4 =~ 1*f1_4 + f2_4 + f3_4 + f4_4
        T5 =~ 1*f1_5 + f2_5 + f3_5 + f4_5
        T6 =~ 1*f1_6 + f2_6 + f3_6 + f4_6
        T7 =~ 1*f1_7 + f2_7 + f3_7 + f4_7
        T8 =~ 1*f1_8 + f2_8 + f3_8 + f4_8
        T9 =~ 1*f1_9 + f2_9 + f3_9 + f4_9
        T10 =~ 1*f1_10 + f2_10 + f3_10 + f4_10
        
        M1 =~ 1*f1_1 + 1*f1_2 + 1*f1_3 + 1*f1_4 + 1*f1_5 + 1*f1_6 + 1*f1_7 + 1*f1_8 + 1*f1_9 + 1*f1_10 + 1*f2_1 + 1*f2_2 + 1*f2_3 + 1*f2_4 + 1*f2_5 + 1*f2_6 + 1*f2_7 + 1*f2_8 + 1*f2_9 + 1*f2_10
       
        
        A =~ 1*f1_1 + 1*f1_2 + 1*f1_3 + 1*f1_4 + 1*f1_5 + 1*f1_6 + 1*f1_7 + 1*f1_8 + 1*f1_9 + 1*f1_10 + 1*f3_1 + 1*f3_2 + 1*f3_3 + 1*f3_4 + 1*f3_5 + 1*f3_6 + 1*f3_7 + 1*f3_8 + 1*f3_9 + 1*f3_10
        
        # estimate variances of latent variables

        M1 ~~ NA*M1
        A ~~ NA*A
        
        
        T1 ~~ NA*T1
        T2 ~~ NA*T2
        T3 ~~ NA*T3
        T4 ~~ NA*T4
        T5 ~~ NA*T5
        T6 ~~ NA*T6
        T7 ~~ NA*T7
        T8 ~~ NA*T8
        T9 ~~ NA*T9
        T10 ~~ NA*T10
        
        # fix intercepts and estimate means
        f1_1 + f2_1 + f3_1 + f4_1 ~ 0*0
        f1_2 + f2_2 + f3_2 + f4_2 ~ 0*0
        f1_3 + f2_3 + f3_3 + f4_3 ~ 0*0
        f1_4 + f2_4 + f3_4 + f4_4 ~ 0*0
        f1_5 + f2_5 + f3_5 + f4_5 ~ 0*0
        f1_6 + f2_6 + f3_6 + f4_6 ~ 0*0
        f1_7 + f2_7 + f3_7 + f4_7 ~ 0*0
        f1_8 + f2_8 + f3_8 + f4_8 ~ 0*0
        f1_9 + f2_9 + f3_9 + f4_9 ~ 0*0
        f1_10 + f2_10 + f3_10 + f4_10 ~ 0*0
        
        T1 + T2 + T3 + T4 + T5 + T6 + T7 + T8 + T9 + T10 + M1 + A ~ NA*0
        
        # no correlations between methods
        M1 ~~ 0*A
        
        # no correlations between methods and traits
        M1 ~~ 0*T1
        M1 ~~ 0*T2
        M1 ~~ 0*T3
        M1 ~~ 0*T4
        M1 ~~ 0*T5
        M1 ~~ 0*T6
        M1 ~~ 0*T7
        M1 ~~ 0*T8
        M1 ~~ 0*T9
        M1 ~~ 0*T10
        
        A ~~ 0*T1
        A ~~ 0*T2
        A ~~ 0*T3
        A ~~ 0*T4
        A ~~ 0*T5
        A ~~ 0*T6
        A ~~ 0*T7
        A ~~ 0*T8
        A ~~ 0*T9
        A ~~ 0*T10
"


# Political interest MTME -------------

fit_mtme2_polint <- cfa(mtme2,
                        data = data2,
                        std.lv = TRUE,
                        missing="ml",
                        auto.fix.first = FALSE,
                        auto.var = TRUE,
                        em.h1.iter.max = 10000,
                        group = "polint")

summary(fit_mtme2_polint, standardized = TRUE)

lavaan::fitmeasures(fit_mtme2_polint)

mtme_fit_est_polint <- parameterestimates(fit_mtme2_polint, 
                                          standardized = T)

mtme_est_qual_polint <- mtme_fit_est_polint %>%
  filter(op == "=~" | op == "~~") %>%
  filter(!(op == "~~" & str_detect(lhs, "T|M|A"))) %>%
  rename(polint = group) %>% 
  mutate(
    polint = factor(polint, 
                    labels = c("High interest", "Low interest")),
    Trait = case_when(
      str_detect(rhs, "_1$") ~ "Tax",
      str_detect(rhs, "_2") ~ "Corporations",
      str_detect(rhs, "_3") ~ "Rural",
      str_detect(rhs, "_4") ~ "Prison",
      str_detect(rhs, "_5") ~ "Refugees",
      str_detect(rhs, "_6") ~ "Gay",
      str_detect(rhs, "_7") ~ "Enviornment",
      str_detect(rhs, "_8") ~ "EU",
      str_detect(rhs, "_9") ~ "Digitalization",
      str_detect(rhs, "_10") ~ "Beef",
    ),
    group = case_when(
      str_detect(rhs, "f1|f2") ~ "5 point scale",
      str_detect(rhs, "f3|f4") ~ "11 point scale",),
    source = case_when(str_detect(lhs, "T") ~ "Trait",
                       str_detect(lhs, "M") ~ "Method",
                       str_detect(lhs, "A") ~ "Acquiescence",
                       TRUE ~ "Random error"),
    qual = ifelse(source != "Random error", std.all^2, std.all)) %>%
  select(-op, -est, -std.lv)

mtme_est_qual_polint %>% 
  group_by(source, polint) %>% 
  summarise(qual = mean(qual))

mtme_est_qual_polint %>% 
  group_by(polint, group, source) %>% 
  summarise(qual = mean(qual))

mtme_est_qual_polint %>% 
  group_by(source, Trait, polint) %>% 
  summarise(qual = mean(qual)) %>% 
  filter(source == "Trait") %>% 
  group_by(Trait) %>% 
  mutate(dif = qual - lag(qual))

mtme_est_qual_polint %>% 
  group_by(polint, Trait, source) %>% 
  summarise(qual = mean(qual))


# Figure 6 -----------------------------

mtme_est_qual_polint %>%
  mutate(source = fct_relevel(source, "Trait"),
         Trait = fct_rev(Trait)) %>%
  group_by(Trait, polint, source) %>%
  summarise(group_qual = sum(qual)/4)  %>%
  mutate(Trait_fct = as.factor(Trait),
         Trait_fct = fct_relevel(Trait_fct, q_labs)) %>%
  ggplot(aes(polint, group_qual, fill = source)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Trait_fct, nrow = 1) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Political interest",
       fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5,
                                   hjust = 0.1))


# Internal efficacy MTME -----------------------------

fit_mtme2_inteff <- cfa(mtme2,
                        data = data2,
                        std.lv = TRUE,
                        missing="ml",
                        auto.fix.first = FALSE,
                        auto.var = TRUE,
                        em.h1.iter.max = 10000,
                        group = "inteff")

summary(fit_mtme2_inteff, standardized = TRUE)

lavaan::fitmeasures(fit_mtme2_inteff)

mtme_fit_est_inteff <- parameterestimates(fit_mtme2_inteff, 
                                          standardized = T)

mtme_est_qual_inteff <- mtme_fit_est_inteff %>%
  filter(op == "=~" | op == "~~") %>%
  filter(!(op == "~~" & str_detect(lhs, "T|M|A"))) %>%
  rename(inteff = group) %>% 
  mutate(
    inteff = factor(inteff, 
                    labels = c("High internal efficacy", 
                               "Low internal efficacy")),
    Trait = case_when(
      str_detect(rhs, "_1$") ~ "Tax",
      str_detect(rhs, "_2") ~ "Corporations",
      str_detect(rhs, "_3") ~ "Rural",
      str_detect(rhs, "_4") ~ "Prison",
      str_detect(rhs, "_5") ~ "Refugees",
      str_detect(rhs, "_6") ~ "Gay",
      str_detect(rhs, "_7") ~ "Enviornment",
      str_detect(rhs, "_8") ~ "EU",
      str_detect(rhs, "_9") ~ "Digitalization",
      str_detect(rhs, "_10") ~ "Beef",
    ),
    group = case_when(
      str_detect(rhs, "f1|f2") ~ "5 point scale",
      str_detect(rhs, "f3|f4") ~ "11 point scale",),
    source = case_when(str_detect(lhs, "T") ~ "Trait",
                       str_detect(lhs, "M") ~ "Method",
                       str_detect(lhs, "A") ~ "Acquiescence",
                       TRUE ~ "Random error"),
    qual = ifelse(source != "Random error", std.all^2, std.all)) %>%
  select(-op, -est, -std.lv)

mtme_est_qual_inteff %>% 
  group_by(source, inteff) %>% 
  summarise(qual = mean(qual))

mtme_est_qual_inteff %>% 
  group_by(inteff, group, source) %>% 
  summarise(qual = mean(qual))

mtme_est_qual_inteff %>% 
  group_by(source, Trait, inteff) %>% 
  summarise(qual = mean(qual)) %>% 
  filter(source == "Trait") %>% 
  group_by(Trait) %>% 
  mutate(dif = qual - lag(qual)) %>% 
  arrange(dif)

mtme_est_qual_inteff %>% 
  group_by(inteff, Trait, source) %>% 
  summarise(qual = mean(qual))


# Figure 7 -----------------------------

mtme_est_qual_inteff %>%
  mutate(source = fct_relevel(source, "Trait"),
         Trait = fct_rev(Trait)) %>%
  group_by(Trait, inteff, source) %>%
  summarise(group_qual = sum(qual)/4) %>%
  mutate(Trait_fct = as.factor(Trait),
         Trait_fct = fct_relevel(Trait_fct, q_labs)) %>%
  ggplot(aes(inteff, group_qual, fill = source)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Trait_fct, nrow = 1) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Internal efficacy",
       fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5,
                                   hjust = 0.1))


# Degree MTME -----------------------------

fit_mtme2_degree <- cfa(mtme2,
                        data = filter(data2, !is.na(degree)) %>%
                          mutate(degree = as.numeric(degree)),
                        std.lv = TRUE,
                        missing="ml",
                        auto.fix.first = FALSE,
                        auto.var = TRUE,
                        em.h1.iter.max = 10000,
                        group = "degree")

summary(fit_mtme2_degree, standardized = TRUE)

lavaan::fitmeasures(fit_mtme2_degree)

mtme_fit_est_degree <- parameterestimates(fit_mtme2_degree, 
                                          standardized = T)

mtme_est_qual_degree <- mtme_fit_est_degree %>%
  filter(op == "=~" | op == "~~") %>%
  filter(!(op == "~~" & str_detect(lhs, "T|M|A"))) %>%
  rename(degree = group) %>% 
  mutate(
    degree = factor(degree, 
                    labels = c("Degree", "No degree")),
    Trait = case_when(
      str_detect(rhs, "_1$") ~ "Tax",
      str_detect(rhs, "_2") ~ "Corporations",
      str_detect(rhs, "_3") ~ "Rural",
      str_detect(rhs, "_4") ~ "Prison",
      str_detect(rhs, "_5") ~ "Refugees",
      str_detect(rhs, "_6") ~ "Gay",
      str_detect(rhs, "_7") ~ "Enviornment",
      str_detect(rhs, "_8") ~ "EU",
      str_detect(rhs, "_9") ~ "Digitalization",
      str_detect(rhs, "_10") ~ "Beef",
    ),
    group = case_when(
      str_detect(rhs, "f1|f2") ~ "5 point scale",
      str_detect(rhs, "f3|f4") ~ "11 point scale",),
    source = case_when(str_detect(lhs, "T") ~ "Trait",
                       str_detect(lhs, "M") ~ "Method",
                       str_detect(lhs, "A") ~ "Acquiescence",
                       TRUE ~ "Random error"),
    qual = ifelse(source != "Random error", std.all^2, std.all)) %>%
  select(-op, -est, -std.lv)

mtme_est_qual_degree %>% 
  group_by(source, degree) %>% 
  summarise(qual = mean(qual))

mtme_est_qual_degree %>% 
  group_by(degree, group, source) %>% 
  summarise(qual = mean(qual))

mtme_est_qual_degree %>% 
  group_by(source, Trait, degree) %>% 
  summarise(qual = mean(qual)) %>% 
  filter(source == "Trait") %>% 
  group_by(Trait) %>% 
  mutate(dif = qual - lag(qual))

mtme_est_qual_degree %>% 
  group_by(degree, Trait, source) %>% 
  summarise(qual = mean(qual))


# Figure 8 -----------------------------

mtme_est_qual_degree %>%
  mutate(source = fct_relevel(source, "Trait"),
         Trait = fct_rev(Trait)) %>%
  group_by(Trait, degree, source) %>%
  summarise(group_qual = sum(qual)/4) %>%
  mutate(Trait_fct = as.factor(Trait),
         Trait_fct = fct_relevel(Trait_fct, q_labs)) %>% 
  ggplot(aes(degree, group_qual, fill = source)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Trait_fct, nrow = 1) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Topic",
       fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5,
                                   hjust = 0.1))


# Table A1 -----------------------------

tbl1 <- data %>% 
  mutate(across(where(is.labelled), ~as_factor(.x))) %>% 
  mutate(agegroup4 = fct_collapse(agegroup,
                                  "18-34" = c("18-24", "25-29", "30-34"),
                                  "35-49" = c("35-39", "40-44", "45-49"),
                                  "50-64" = c("50-54","55-59", "60-64"),
                                  "65-" = c("65-69", "70-74", "75-79", "80-"))) %>% 
  select(gender, agegroup4, edu3, mtme)

tbl_summary(
  tbl1,
  by = mtme,
  label = list(
    gender = "Gender",
    agegroup4 = "Age",
    edu3 = "Education"),
  missing = "no") %>%
  add_n() %>%
  add_p() %>% 
  modify_header(label = "Variable") %>%
  bold_labels()


# Table A2 -----------------------------

tbl2 <- data %>% 
  mutate(across(where(is.labelled), ~as_factor(.x))) %>% 
  select(polinterest, int_eff_1, int_eff_2, int_eff_3, degree, mtme) %>% 
  mutate(int_eff_1 = factor(int_eff_1, levels = 0:3, 
                            labels = c("Strongly agree", "Agree to some extent", 
                                       "Disagree to some extent", "Strongly disagree")),
         int_eff_2 = factor(int_eff_2, levels = 3:0, 
                            labels = c("Strongly agree", "Agree to some extent", 
                                       "Disagree to some extent", "Strongly disagree")),
         int_eff_3 = factor(int_eff_3, levels = 3:0, 
                            labels = c("Strongly agree", "Agree to some extent", 
                                       "Disagree to some extent", "Strongly disagree")))

tbl_summary(
  tbl2,
  by = mtme,
  label = list(
    polinterest = "How interested are you in politics?",
    int_eff_1 = "Sometimes politics seems so complicated that I cannot quite understand what is going on",
    int_eff_2 = "I trust in my ability to participate in politics",
    int_eff_3 = "I know what I think on important social issues",
    degree = "University degree or not"),
  missing = "no") %>%
  add_n() %>%
  modify_header(label = "Variable") %>%
  bold_labels()


# Figure A1 -----------------------------

# Read in the raw Finnish responses from the two surveys to calculate don't knows and NAs
t1 <- read_sav("t1_nonresp.sav") %>% 
  mutate(across(where(is.labelled), ~as_factor(.x)))

t2 <- read_sav("t2_nonresp.sav") %>% 
  mutate(across(where(is.labelled), ~as_factor(.x)))

# Combine the two time-points according to form and treatment group
F1_t1 <- t1 %>% 
  filter(MTME == "1" | MTME == "2" | MTME == "3") %>% 
  select(F1_1:F1_10)

F2_t1 <- t1 %>% 
  filter(MTME == "4" | MTME == "5" | MTME == "7") %>% 
  select(F2_1:F2_10)

F3_t1 <- t1 %>% 
  filter(MTME == "6" | MTME == "8" | MTME == "10") %>% 
  select(F3_1:F3_10)

F4_t1 <- t1 %>% 
  filter(MTME == "9" | MTME == "11" | MTME == "12") %>% 
  select(F4_1:F4_10)

F1_t2 <- t2 %>% 
  filter(MTME == "7" | MTME == "8" | MTME == "9") %>% 
  select(F1_1:F1_10)

F2_t2 <- t2 %>% 
  filter(MTME == "1" | MTME == "10" | MTME == "11") %>% 
  select(F2_1:F2_10)

F3_t2 <- t2 %>% 
  filter(MTME == "2" | MTME == "4" | MTME == "12") %>% 
  select(F3_1:F3_10)

F4_t2 <- t2 %>% 
  filter(MTME == "3" | MTME == "5" | MTME == "6") %>% 
  select(F4_1:F4_10)

F1 <- bind_rows(F1_t1, F1_t2)
F2 <- bind_rows(F2_t1, F2_t2)
F3 <- bind_rows(F3_t1, F3_t2)
F4 <- bind_rows(F4_t1, F4_t2)

# Counting don't knows and NAs
# Define "Don't know" (i.e. En osaa sanoa) and NA as 1, calculate number per column
F1_dk <- F1 %>%
  mutate_all(~ ifelse(. %in% "En osaa sanoa" | is.na(.), 1, 0)) %>%
  summarise_all(sum)

F2_dk <- F2 %>%
  mutate_all(~ ifelse(. %in% "En osaa sanoa" | is.na(.), 1, 0)) %>%
  summarise_all(sum)

F3_dk <- F3 %>%
  mutate_all(~ ifelse(. %in% "En osaa sanoa" | is.na(.), 1, 0)) %>%
  summarise_all(sum)

F4_dk <- F4 %>%
  mutate_all(~ ifelse(. %in% "En osaa sanoa" | is.na(.), 1, 0)) %>%
  summarise_all(sum)

# Combine data frames and add a "Form" column
F1_dk$Form <- "F1"
F2_dk$Form <- "F2"
F3_dk$Form <- "F3"
F4_dk$Form <- "F4"

## Fixing variable names
F1_dk <- rename_all(F1_dk, ~str_remove(., "F1_"))
F2_dk <- rename_all(F2_dk, ~str_remove(., "F2_"))
F3_dk <- rename_all(F3_dk, ~str_remove(., "F3_"))
F4_dk <- rename_all(F4_dk, ~str_remove(., "F4_"))

# Combine data frames into a single data frame
combined_dk <- bind_rows(F1_dk, F2_dk, F3_dk, F4_dk)

# Rename the columns
combined_dk <- combined_dk %>% 
  rename("Tax_nonresp" = "1",
         "Corporations_nonresp" = "2",
         "Rural_nonresp" = "3",
         "Prison_nonresp" = "4",
         "Refugees_nonresp" = "5",
         "Gay_nonresp" = "6",
         "Environment_nonresp" = "7",
         "EU_nonresp" = "8",
         "Digitalization_nonresp" = "9",
         "Beef_nonresp" = "10")

# Counting actual answers
# Define "Don't know" (i.e. En osaa sanoa) or item nonresponse (NA) as 0, everything else as response (1)
F1_resp <- F1 %>%
  mutate_all(~ ifelse(. %in% "En osaa sanoa" | is.na(.), 0, 1)) %>%
  summarise_all(sum)

F2_resp <- F2 %>%
  mutate_all(~ ifelse(. %in% "En osaa sanoa" | is.na(.), 0, 1)) %>%
  summarise_all(sum)

F3_resp <- F3 %>%
  mutate_all(~ ifelse(. %in% "En osaa sanoa" | is.na(.), 0, 1)) %>%
  summarise_all(sum)

F4_resp <- F4 %>%
  mutate_all(~ ifelse(. %in% "En osaa sanoa" | is.na(.), 0, 1)) %>%
  summarise_all(sum)

# Combine data frames and add a "Form" column
F1_resp$Form <- "F1"
F2_resp$Form <- "F2"
F3_resp$Form <- "F3"
F4_resp$Form <- "F4"

# Fixing variable names
F1_resp <- rename_all(F1_resp, ~str_remove(., "F1_"))
F2_resp <- rename_all(F2_resp, ~str_remove(., "F2_"))
F3_resp <- rename_all(F3_resp, ~str_remove(., "F3_"))
F4_resp <- rename_all(F4_resp, ~str_remove(., "F4_"))

# Combine data frames into a single data frame
combined_resp <- bind_rows(F1_resp, F2_resp, F3_resp, F4_resp)

# Rename the columns
combined_resp <- combined_resp %>% 
  rename("Tax" = "1",
         "Corporations" = "2",
         "Rural" = "3",
         "Prison" = "4",
         "Refugees" = "5",
         "Gay" = "6",
         "Environment" = "7",
         "EU" = "8",
         "Digitalization" = "9",
         "Beef" = "10")

# Combine and calculate item nonresponse proportions
combined <- left_join(combined_resp, combined_dk, by = "Form")

question_names <- c("Tax", "Corporations", "Rural", "Prison", "Refugees", 
                    "Gay", "Environment", "EU", "Digitalization", "Beef")

# Create an empty data frame to store the results
results_df <- data.frame(Form = character(0), Question = character(0), Item_Nonresponse_Pct = numeric(0))

# Loop through each question and calculate item nonresponse
for (question in question_names) {
  result <- combined %>%
    group_by(Form) %>%
    summarize(
      Item_Nonresponse_Pct = sum(get(paste0(question, "_nonresp"))) / sum(get(paste0(question, "_nonresp")) + get(question))
    ) %>%
    mutate(Question = question)
  
  results_df <- bind_rows(results_df, result)
}

results_df$Question <- factor(results_df$Question, levels = question_names)

ggplot(results_df, aes(x = Question, y = Item_Nonresponse_Pct)) +
  geom_bar(stat = "identity", position = "dodge", fill = "gray", color = "black") +
  geom_text(aes(label = scales::percent(Item_Nonresponse_Pct, accuracy = 0.1)), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(x = NULL, y = "Item Nonresponse Proportion") +
  facet_wrap(~ Form, ncol = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  ylim(0, 0.15)


# Memory effects -------------------------------

mtme1_c <- "T1 =~ 1*f1_1 + f2_1 + f3_1 + f4_1
        T2 =~ 1*f1_2 + f2_2 + f3_2 + f4_2
        T3 =~ 1*f1_3 + f2_3 + f3_3 + f4_3
        T4 =~ 1*f1_4 + f2_4 + f3_4 + f4_4
        T5 =~ 1*f1_5 + f2_5 + f3_5 + f4_5
        T6 =~ 1*f1_6 + f2_6 + f3_6 + f4_6
        T7 =~ 1*f1_7 + f2_7 + f3_7 + f4_7
        T8 =~ 1*f1_8 + f2_8 + f3_8 + f4_8
        T9 =~ 1*f1_9 + f2_9 + f3_9 + f4_9
        T10 =~ 1*f1_10 + f2_10 + f3_10 + f4_10
        
        M1 =~ 1*f1_1 + 1*f1_2 + 1*f1_3 + 1*f1_4 + 1*f1_5 + 1*f1_6 + 1*f1_7 + 1*f1_8 + 1*f1_9 + 1*f1_10 + 1*f2_1 + 1*f2_2 + 1*f2_3 + 1*f2_4 + 1*f2_5 + 1*f2_6 + 1*f2_7 + 1*f2_8 + 1*f2_9 + 1*f2_10
        M2 =~ 1*f3_1 + 1*f3_2 + 1*f3_3 + 1*f3_4 + 1*f3_5 + 1*f3_6 + 1*f3_7 + 1*f3_8 + 1*f3_9 + 1*f3_10 + 1*f4_1 + 1*f4_2 + 1*f4_3 + 1*f4_4 + 1*f4_5 + 1*f4_6 + 1*f4_7 + 1*f4_8 + 1*f4_9 + 1*f4_10
        
        A =~ 1*f1_1 + 1*f1_2 + 1*f1_3 + 1*f1_4 + 1*f1_5 + 1*f1_6 + 1*f1_7 + 1*f1_8 + 1*f1_9 + 1*f1_10 + 1*f3_1 + 1*f3_2 + 1*f3_3 + 1*f3_4 + 1*f3_5 + 1*f3_6 + 1*f3_7 + 1*f3_8 + 1*f3_9 + 1*f3_10
        
        # estimate variances of latent variables

        M1 ~~ NA*M1
        M2 ~~ NA*M2
        A ~~ NA*A
        
        
        T1 ~~ NA*T1
        T2 ~~ NA*T2
        T3 ~~ NA*T3
        T4 ~~ NA*T4
        T5 ~~ NA*T5
        T6 ~~ NA*T6
        T7 ~~ NA*T7
        T8 ~~ NA*T8
        T9 ~~ NA*T9
        T10 ~~ NA*T10
        
        # fix intercepts and estimate means
        f1_1 + f2_1 + f3_1 + f4_1 ~ 0*0
        f1_2 + f2_2 + f3_2 + f4_2 ~ 0*0
        f1_3 + f2_3 + f3_3 + f4_3 ~ 0*0
        f1_4 + f2_4 + f3_4 + f4_4 ~ 0*0
        f1_5 + f2_5 + f3_5 + f4_5 ~ 0*0
        f1_6 + f2_6 + f3_6 + f4_6 ~ 0*0
        f1_7 + f2_7 + f3_7 + f4_7 ~ 0*0
        f1_8 + f2_8 + f3_8 + f4_8 ~ 0*0
        f1_9 + f2_9 + f3_9 + f4_9 ~ 0*0
        f1_10 + f2_10 + f3_10 + f4_10 ~ 0*0
        
        T1 + T2 + T3 + T4 + T5 + T6 + T7 + T8 + T9 + T10 + M1 + M2 + A ~ NA*0
        
        # no correlations between methods
        M1 ~~ 0*M2
        M1 ~~ 0*A
        M2 ~~ 0*A
        
        # no correlations between methods and traits
        M1 ~~ 0*T1
        M1 ~~ 0*T2
        M1 ~~ 0*T3
        M1 ~~ 0*T4
        M1 ~~ 0*T5
        M1 ~~ 0*T6
        M1 ~~ 0*T7
        M1 ~~ 0*T8
        M1 ~~ 0*T9
        M1 ~~ 0*T10

        M2 ~~ 0*T1
        M2 ~~ 0*T2
        M2 ~~ 0*T3
        M2 ~~ 0*T4
        M2 ~~ 0*T5
        M2 ~~ 0*T6
        M2 ~~ 0*T7
        M2 ~~ 0*T8
        M2 ~~ 0*T9
        M2 ~~ 0*T10
        
        
        A ~~ 0*T1
        A ~~ 0*T2
        A ~~ 0*T3
        A ~~ 0*T4
        A ~~ 0*T5
        A ~~ 0*T6
        A ~~ 0*T7
        A ~~ 0*T8
        A ~~ 0*T9
        A ~~ 0*T10
        
        f1_1 + f2_1 + f3_1 + f4_1 + f1_2 + f2_2 + f3_2 + f4_2 + f1_3 + f2_3 + f3_3 + f4_3 + f1_4 + f2_4 + f3_4 + f4_4 + f1_5 + f2_5 + f3_5 + f4_5 + f1_6 + f2_6 + f3_6 + f4_6 + f1_7 + f2_7 + f3_7 + f4_7 + f1_8 + f2_8 + f3_8 + f4_8 + f1_9 + f2_9 + f3_9 + f4_9 + f1_10 + f2_10 + f3_10 + f4_10 ~ time_days_c
"

fit_mtme1_c <- cfa(mtme1_c, data = data2, std.lv = TRUE, missing="ml",
                   auto.fix.first = FALSE, auto.var = TRUE, 
                   em.h1.iter.max = 10000) 

summary(fit_mtme1_c, standardized = TRUE)

lavaan::fitmeasures(fit_mtme1_c)

mtme_fit_est_c <- lavaan::parameterestimates(fit_mtme1_c, 
                                             standardized = T)

mtme_est_qual_c <- mtme_fit_est_c %>%
  filter(op == "=~" | op == "~~") %>%
  filter(!(op == "~~" & str_detect(lhs, "T|M|A"))) %>%
  filter(!lhs == "time_days_c") %>% 
  mutate(Trait = case_when(
    str_detect(rhs, "_1$") ~ "Tax",
    str_detect(rhs, "_2") ~ "Corporations",
    str_detect(rhs, "_3") ~ "Rural",
    str_detect(rhs, "_4") ~ "Prison",
    str_detect(rhs, "_5") ~ "Refugees",
    str_detect(rhs, "_6") ~ "Gay",
    str_detect(rhs, "_7") ~ "Enviornment",
    str_detect(rhs, "_8") ~ "EU",
    str_detect(rhs, "_9") ~ "Digitalization",
    str_detect(rhs, "_10") ~ "Beef",
  ),
  group = case_when(
    str_detect(rhs, "f1|f2") ~ "5 point scale",
    str_detect(rhs, "f3|f4") ~ "11 point scale",),
  source = case_when(str_detect(lhs, "T") ~ "Trait",
                     str_detect(lhs, "M") ~ "Method",
                     str_detect(lhs, "A") ~ "Acquiescence",
                     TRUE ~ "Random error"),
  qual = ifelse(source != "Random error", std.all^2, std.all)) %>%
  select(-op, -est, -std.lv)

mtme_est_qual_c %>% 
  group_by(source) %>% 
  summarise(qual = mean(qual))

mtme_est_qual %>% 
  group_by(source) %>% 
  summarise(qual = mean(qual))

mtme_est_qual_c %>% 
  group_by(group, source) %>% 
  summarise(qual = mean(qual))

mtme_est_qual %>% 
  group_by(group, source) %>% 
  summarise(qual = mean(qual))


mtme_est_qual_c %>% 
  group_by(group, source) %>% 
  summarise(qual = mean(qual))

mtme_est_qual_c %>% 
  group_by(Trait, source, group) %>% 
  summarise(qual = mean(qual)) %>% print(n = 100)

mtme_est_qual %>% 
  group_by(Trait, source, group) %>% 
  summarise(qual = mean(qual))  %>% print(n = 100)

mtme_sum_control <- mtme_est_qual_c %>%
  mutate(source = fct_relevel(source, "Trait"),
         Trait = fct_rev(Trait)) %>%
  group_by(Trait, source) %>%
  summarise(group_qual = sum(qual)/4) %>% 
  mutate(group = "Controling for time")

mtme_sum <- mtme_est_qual %>%
  mutate(source = fct_relevel(source, "Trait"),
         Trait = fct_rev(Trait)) %>%
  group_by(Trait, source) %>%
  summarise(group_qual = sum(qual)/4) %>% 
  mutate(group = "Not controling for time")


# Figure A2 -------------------------------

rbind(mtme_sum_control, mtme_sum) %>% 
  mutate(Trait_fct = as.factor(Trait),
         Trait_fct = fct_relevel(Trait_fct, q_labs)) %>% 
  ggplot(aes(group, group_qual, fill = source)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Trait_fct, nrow = 1) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Model",
       fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5,
                                   hjust = 0.1))


# Prob vs non-prob MTME -------------------------------

fit_mtme1_source <- cfa(mtme1,
                        data = data2 %>%
                          mutate(source = as.numeric(source)),
                        std.lv = TRUE,
                        missing="ml",
                        auto.fix.first = FALSE,
                        auto.var = TRUE,
                        em.h1.iter.max = 10000,
                        group = "source")

summary(fit_mtme1_source, standardized = TRUE)

lavaan::fitmeasures(fit_mtme1_source)

mtme_fit_est_source <- parameterestimates(fit_mtme1_source, 
                                          standardized = T)

mtme_est_qual_source <- mtme_fit_est_source %>%
  filter(op == "=~" | op == "~~") %>%
  filter(!(op == "~~" & str_detect(lhs, "T|M|A"))) %>%
  rename(data_source = group) %>% 
  mutate(
    data_source = factor(data_source, 
                         labels = c("Non-probability", "Probability")),
    Trait = case_when(
      str_detect(rhs, "_1$") ~ "Tax",
      str_detect(rhs, "_2") ~ "Corporations",
      str_detect(rhs, "_3") ~ "Rural",
      str_detect(rhs, "_4") ~ "Prison",
      str_detect(rhs, "_5") ~ "Refugees",
      str_detect(rhs, "_6") ~ "Gay",
      str_detect(rhs, "_7") ~ "Enviornment",
      str_detect(rhs, "_8") ~ "EU",
      str_detect(rhs, "_9") ~ "Digitalization",
      str_detect(rhs, "_10") ~ "Beef",
    ),
    group = case_when(
      str_detect(rhs, "f1|f2") ~ "5 point scale",
      str_detect(rhs, "f3|f4") ~ "11 point scale",),
    source = case_when(str_detect(lhs, "T") ~ "Trait",
                       str_detect(lhs, "M") ~ "Method",
                       str_detect(lhs, "A") ~ "Acquiescence",
                       TRUE ~ "Random error"),
    qual = ifelse(source != "Random error", std.all^2, std.all)) %>%
  select(-op, -est, -std.lv)

mtme_est_qual_source %>% 
  group_by(source, data_source) %>% 
  summarise(qual = mean(qual))

mtme_est_qual_source %>% 
  group_by(data_source, group, source) %>% 
  summarise(qual = mean(qual))

mtme_est_qual_source %>% 
  group_by(Trait, source, data_source) %>% 
  summarise(qual = mean(qual)) %>% 
  filter(source == "Trait") %>% 
  group_by(Trait) %>% 
  mutate(dif = qual - lag(qual)) %>% 
  arrange(dif)

mtme_est_qual_source %>% 
  group_by(data_source, Trait, source) %>% 
  summarise(qual = mean(qual))


# Figure A3 -------------------------------

mtme_est_qual_source %>%
  mutate(source = fct_relevel(source, "Trait"),
         Trait = fct_rev(Trait)) %>%
  group_by(Trait, data_source, source) %>%
  summarise(group_qual = sum(qual)/4) %>%
  mutate(Trait_fct = as.factor(Trait),
         Trait_fct = fct_relevel(Trait_fct, q_labs)) %>%
  ggplot(aes(data_source, group_qual, fill = source)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Trait_fct, nrow = 1) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Sample source",
       fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5,
                                   hjust = 0.1))




