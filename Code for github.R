######Load data########
load("eurobonds_data.Rdata")
load("eurobonds_long.Rdata")

######Load libraries######
library(fixest)
library(dplyr)
library(naniar)
library(ggplot2)


#Run main analysis
policy_differences <- feglm(support  ~ policy_factor | year + country + studyID,
                            weights = df_long_eurobonds$weight.new,
                            family = "binomial",
                            data = df_long_eurobonds, 
                            se = "threeway")

summary(policy_differences)

#Create main plot
tiff("policy_differences.tiff", res = 300, height = 2000, width = 2000)
plot.new()
coefplot(policy_differences, main = "Relative support for policy integration \n (reference policy: digital single market)",
         dict = c("policy_factoremu_supp_recoded" = "Economic and \n monetary union",
                  "policy_factoreu_def_supp_recoded" = "Common security and \n defence policies",
                  "policy_factoreu_energy_pol" = "Common \n energy policies",
                  "policy_factoreu_foreign_pol" = "Common \n foreign policies",
                  "policy_factoreu_migration_pol" = "Common \n migration policies",
                  "policy_factoreurobonds_supp_dummy" = "Eurobonds",
                  "policy_factorfree_movement" = "Free movement of \n persons"), lab.fit = "tilted")
dev.off()

#Run regional models
#######Subset different regions#######
df_long_eurobonds$southern_europe <- ifelse(df_long_eurobonds$country=="GR"|
                                              df_long_eurobonds$country=="ES"|
                                              df_long_eurobonds$country=="MT"|
                                              df_long_eurobonds$country=="IT"|
                                              df_long_eurobonds$country=="CY"|
                                              df_long_eurobonds$country=="CY-TCC"|
                                              df_long_eurobonds$country=="PT", 1, 0)

#Central and Eastern Europe
df_long_eurobonds$ce_europe <- ifelse(df_long_eurobonds$country=="LV"|
                                        df_long_eurobonds$country=="LT"|
                                        df_long_eurobonds$country=="EE"|
                                        df_long_eurobonds$country=="PL"|
                                        df_long_eurobonds$country=="CZ"|
                                        df_long_eurobonds$country=="SK"|
                                        df_long_eurobonds$country=="HU"|
                                        df_long_eurobonds$country=="RO"|
                                        df_long_eurobonds$country=="BG"|
                                        df_long_eurobonds$country=="SL", 1, 0)

#Nordic countries
df_long_eurobonds$nordics <- ifelse(df_long_eurobonds$country=="FI"|
                                      df_long_eurobonds$country=="SE"|
                                      df_long_eurobonds$country=="DK", 1, 0)

#Western European members
df_long_eurobonds$we_members <- ifelse(df_long_eurobonds$country=="DE"|
                                         df_long_eurobonds$country=="BE"|
                                         df_long_eurobonds$country=="NL"|
                                         df_long_eurobonds$country=="LU"|
                                         df_long_eurobonds$country=="FR"| 
                                         df_long_eurobonds$country=="GB"|
                                         df_long_eurobonds$country=="IE"|
                                         df_long_eurobonds$country=="AT", 1, 0)

######Subset the different regions#######
df_long_eurobonds_southern <- df_long_eurobonds %>%
  dplyr::filter(southern_europe == 1)

df_long_eurobonds_cee <- df_long_eurobonds %>%
  dplyr::filter(ce_europe == 1)

df_long_eurobonds_nordics <- df_long_eurobonds %>%
  dplyr::filter(nordics == 1)

df_long_eurobonds_we_members <- df_long_eurobonds %>%
  dplyr::filter(we_members == 1)

#####Run regional comparisons#######
#Southern Europe
policy_differences_southern <- feglm(support  ~ policy_factor | year + country + studyID,
                                     weights = df_long_eurobonds_southern$weight.new,
                                     family = "binomial",
                                     data = df_long_eurobonds_southern, 
                                     se = "threeway")

summary(policy_differences_southern)

#Western Europe
policy_differences_western <- feglm(support  ~ policy_factor | year + country + studyID,
                                    weights = df_long_eurobonds_we_members$weight.new,
                                    family = "binomial",
                                    data = df_long_eurobonds_we_members, 
                                    se = "threeway")

summary(policy_differences_western)

#Central and Eastern Europe
policy_differences_cee <- feglm(support  ~ policy_factor | year + country + studyID,
                                weights = df_long_eurobonds_cee$weight.new,
                                family = "binomial",
                                data = df_long_eurobonds_cee, 
                                se = "threeway")

summary(policy_differences_cee)

#Nordics
policy_differences_nordics <- feglm(support  ~ policy_factor | year + country + studyID,
                                    weights = df_long_eurobonds_nordics$weight.new,
                                    family = "binomial",
                                    data = df_long_eurobonds_nordics, 
                                    se = "threeway")

summary(policy_differences_nordics)

########Create regional plot##########
tiff("regional_policyplot.tiff", res = 300, height = 2000, width = 2000)
plot.new()
coefplot_policydiffs <- coefplot(list(policy_differences_southern, policy_differences_western, policy_differences_nordics, policy_differences_cee),
                                 main = "Changes in support for policy integration \n (reference policy: digital single market)",
                                 dict = c("policy_factoremu_supp_recoded" = "Economic and \n monetary union",
                                          "policy_factoreu_def_supp_recoded" = "Common security and \n defence policies",
                                          "policy_factoreu_energy_pol" = "Common \n energy policies",
                                          "policy_factoreu_foreign_pol" = "Common \n foreign policies",
                                          "policy_factoreu_migration_pol" = "Common \n migration policies",
                                          "policy_factoreurobonds_supp_dummy" = "Eurobonds",
                                          "policy_factorfree_movement" = "Free movement \n of persons"), 
                                 lab.fit = "tilted", col = "black", pt.pch = 1:4)
legend("bottomleft", pch = 1:4, legend = c("Southern Europe", "Western Europe", "Nordics", "Central and Eastern Europe"),
       cex = 0.75)

dev.off()

#######Run Eurobonds model######
named_vector_emueuro <- c("white_collar" = "White collar", "manual_worker" = "Manual worker", "economy_index" = "Perception of economy",
                          "institutions_trust" = "Trust in EU institutions", "trust_gov_recoded" = "Trust in nat.government", 
                          "eu_trust_recoded" = "Trust in the EU",
                          "left_right" = "Ideology", "gender" = "Gender", "age_education_recoded" = "Education", "age_exact" = "Age", 
                          "excl_identity" = "Exclusive identity")


glm_eurobonds <- feglm(eurobonds_supp_dummy  ~ manual_worker +
                         economy_index  + trust_gov_recoded +
                         eu_trust_recoded + left_right + gender + age_education_recoded + age_exact +
                         excl_identity | year + country, 
                       family = "binomial",
                       weights = eurobonds_data$weight.new,
                       se = "twoway",
                       data = eurobonds_data)

summary(glm_eurobonds)

#Test stepwise modelling
#Base model
glm_eurobonds_base <- feglm(eurobonds_supp_dummy  ~ manual_worker +
                              economy_index + excl_identity + eu_trust_recoded | year + country, 
                            family = "binomial",
                            weights = eurobonds_data$weight.new,
                            se = "twoway",
                            data = eurobonds_data)

summary(glm_eurobonds_base)

#EU variables
glm_eurobonds_euvar <- feglm(eurobonds_supp_dummy  ~ manual_worker +
                               economy_index + eu_trust_recoded +  
                               excl_identity | year + country, 
                             family = "binomial",
                             weights = eurobonds_data$weight.new,
                             se = "twoway",
                             data = eurobonds_data)

summary(glm_eurobonds_euvar)


#Create coefplot
tiff("eurobonds_robustness.tiff", res = 300, height = 2000, width = 2000)
plot.new()
coefplot(list(glm_eurobonds_base, glm_eurobonds), 
         dict = named_vector_emueuro, main = "Support for Eurobonds", 
         col = 1, pt.pch = 1:2, lab.fit = "tilted")
legend("topright", pch = 1:2, title = "Model", legend = c("Base model", "Full model"))
dev.off()


#Run EMUsupp
glm_emusupp_newdat <- feglm(emu_supp_recoded  ~ manual_worker +
                              economy_index  + trust_gov_recoded +
                              eu_trust_recoded + left_right + gender + age_education_recoded + age_exact +
                              excl_identity | year + country, 
                            family = "binomial",
                            weights = eurobonds_data$weight.new,
                            se = "twoway",
                            data = eurobonds_data)

summary(glm_emusupp_newdat)

#Test stepwise modelling
#Base model
glm_emusupp_base <- feglm(emu_supp_recoded  ~ manual_worker +
                            economy_index + excl_identity + eu_trust_recoded | year + country, 
                          family = "binomial",
                          weights = eurobonds_data$weight.new,
                          se = "twoway",
                          data = eurobonds_data)

summary(glm_emusupp_base)


#Coefplot model
tiff("emu_eurobonds.tiff", res = 300, height = 2000, width = 2000)
plot.new()
coefplot(list(glm_eurobonds, glm_emusupp_newdat), col = 1, 
         pt.pch = c(1,2), main = "Support for EMU and Eurobonds", dict = c("manual_worker" = "Manual worker", "economy_index" = "Perception of economy",
                                                                           "trust_gov_recoded" = "Trust in government", "eu_trust_recoded" = "Trust in EU",
                                                                           "left_right" = "Ideology", "gender" = "Gender", "age_education_recoded" = "Education",
                                                                           "age_exact" = "Age", "excl_identity" = "Exclusively national identity"), lab.fit = "tilted")
legend("bottomleft", cex = 0.75, pch = c(1,2),
       legend = c("Eurobonds", "EMU"), title = "Model")
dev.off()


#Comparative coefplot
tiff("emusupp_robustness.tiff", res = 300, height = 2000, width = 2000)
plot.new()
coefplot(list(glm_emusupp_base, glm_emusupp_newdat), 
         dict = named_vector_emueuro, main = "Support for EMU", 
         col = 1, pt.pch = 1:2, lab.fit = "tilted")
legend("topright", pch = 1:2, title = "Models", legend = c("Base model", "Full model"))
dev.off()

#####Appendix missingness
#Subset model variables#
emu_model_variables <-              eurobonds_data %>%
  dplyr::select(emu_supp_recoded, manual_worker,
                economy_index, income_scale, eurobonds_supp_dummy,
                trust_gov_recoded, gender,
                eu_trust_recoded, left_right, age_education_recoded, age_exact,
                excl_identity)

miss_var_summary(emu_model_variables)

#Missingness ideology
ggplot_breaks2 <- c("trust_gov_recoded", "manual_worker", "income_scale",
                    "gender", "excl_identity", "eurobonds_supp_dummy","eu_trust_recoded",
                    "emu_supp_recoded", "economy_index",
                    "age_exact", "age_education_recoded")

names_yaxis2 <- c("Trust in government", "Manual worker",
                  "Income", "Gender", "Exclusively national identity",
                  "Support for eurobonds", "Trust in EU",
                  "Support for EMU", "Perception of economy",
                  "Age","Education")

gg_miss_ideology <- gg_miss_fct(emu_model_variables, left_right)

gg_miss_ideology + labs(x = "Ideology", y = "Coefficients") +
  scale_y_discrete(breaks = ggplot_breaks2, 
                   labels = names_yaxis2)

ggsave("missingness_ideology.tiff", dpi = 300)

#Missingness income
ggplot_breaks <- c("trust_gov_recoded", "manual_worker", "left_right",
                   "gender", "excl_identity", "eurobonds_supp_dummy","eu_trust_recoded",
                   "emu_supp_recoded", "economy_index",
                   "age_exact", "age_education_recoded")

names_yaxis <- c("Trust in government", "Manual worker",
                 "Ideology", "Gender", "Exclusively national identity",
                 "Support for eurobonds", "Trust in EU",
                 "Support for EMU", "Perception of economy",
                 "Age","Education")

gg_miss_income <- gg_miss_fct(emu_model_variables, income_scale)

gg_miss_income + labs(y = "Coefficients", x = "Income") + 
  scale_y_discrete(breaks = ggplot_breaks,
                   labels = names_yaxis) + scale_color_grey() 


