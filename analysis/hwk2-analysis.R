#question1

reports_per_year <- final.hcris%>% 
  group_by(provider_number, fyear) %>%
  summarise(n = n())
multiple_reports_per_year <- reports_per_year %>%
  filter(n>1)

hospitals <- multiple_reports_per_year%>% group_by(fyear)%>% 
  summarize(count = n())
sum(graph_1$count)


graph1<-ggplot(hospitals, aes(fyear, count))+
  geom_line() +
  labs(title = "Hospitals with 2+ Filed Reports in One Year", x = "Year", y = "Number of Hospitals")+
  theme_bw()

graph1

save.image("Hwk2_workspace.Rdata")

#question2

unique_provider_numbers <- n_distinct(final.hcris$provider_number)
unique_provider_numbers

#question3
graph2 <- ggplot(final.hcris.data, aes(x=as.factor(year), y=tot_charges)) + 
  geom_violin(trim=FALSE)+
  labs(title = "Distribution of Total Charges in Each Year", x = "Year", y = "Total Charges")+
  theme_bw()

graph2
save.image("Hwk2_workspace.Rdata")

#question4

final.hcris.data$discount_factor = 1-final.hcris.data$tot_discounts/final.hcris.data$tot_charges
final.hcris.data$price_num = (final.hcris.data$ip_charges + final.hcris.data$icu_charges + final.hcris.data$ancillary_charges)*final.hcris.data$discount_factor - final.hcris.data$tot_mcare_payment
final.hcris.data$price_denom = final.hcris.data$tot_discharges - final.hcris.data$mcare_discharges
final.hcris.data$price = final.hcris.data$price_num/final.hcris.data$price_denom

graph3 <- ggplot(final.hcris.data, aes(x=as.factor(year), y=price)) + 
  geom_violin(trim=FALSE)+
  labs(title = "Distribution of Estimated Prices in Each Year", x = "Year", y = "Price")+
  theme_bw()

graph3
save.image("Hwk2_workspace.Rdata")

#question5
obs_2012 <- final.hcris.data%>% filter(year == 2012)

obs_2012$penalty <- ifelse(obs_2012$hvbp_payment + obs_2012$hrrp_payment < 0, 1,0)

obs_2012["hvbp_payment"][is.na(obs_2012["hvbp_payment"])] <- 0
obs_2012["hrrp_payment"][is.na(obs_2012["hrrp_payment"])] <- 0

obs_2012 <- obs_2012 %>% 
  filter(!is.na(penalty))%>% 
  filter(!is.na(price)) %>% 
  filter(price_num >0 && price < 100000) 
  
table_5 <- obs_2012 %>% filter(!is.na(penalty)) %>%
  group_by(penalty)%>% 
  summarize(price = mean(price, na.rm = TRUE))

table_5

save.image("Hwk2_workspace.Rdata")

#question6

obs_2012$quartile <- ntile(obs_2012$beds, 4) 

obs_2012$quartile_1 <- ifelse(obs_2012$quartile == 1, 1,0)
obs_2012$quartile_2 <- ifelse(obs_2012$quartile == 2, 1,0)
obs_2012$quartile_3 <- ifelse(obs_2012$quartile == 3, 1,0)
obs_2012$quartile_4 <- ifelse(obs_2012$quartile == 4, 1,0)

table_6 <- obs_2012 %>% group_by(penalty, quartile)%>% summarize(avg_price = mean(price, na.rm = TRUE))

table_6

save.image("Hwk2_workspace.Rdata")

#question7
#part1
inv_var <- Matching::Match(Y=obs_2012$price,
                           Tr=obs_2012$penalty, 
                           X=obs_2012$quartile,
                           M=1,
                           Weight=1,
                           estimand="ATE")
summary(inv_var)
save.image("Hwk2_workspace.Rdata")



#part2
maha <- Matching::Match(Y=obs_2012$price,
                        Tr=obs_2012$penalty,
                        X=obs_2012$quartile,
                        M=1,
                        Weight=2,
                        estimand="ATE")
summary(maha)
save.image("Hwk2_workspace.Rdata")

#part3
logit.model <- glm(penalty ~ quartile_1 + quartile_2 + quartile_3 + quartile_4,
                 data = obs_2012, family = binomial)

obs_2012 <- add_predictions(obs_2012, logit.model, 'ps', type = "response") %>%
  filter(ps>0 & ps<1 )
 
obs_2012 <- obs_2012 %>%
  mutate(ipw = case_when(
    penalty == 1 ~ 1/ps,
    penalty == 0 ~ 1/(1-ps),
    TRUE~NA_real_ ))

mean.t1 <- obs_2012 %>% 
  filter(penalty==1) %>% 
  dplyr::select(price, ipw) %>%
  summarize(mean_y=weighted.mean(price, w=ipw))
mean.t0 <- obs_2012 %>% 
  filter(penalty==0) %>% 
  dplyr::select(price, ipw) %>%
  summarize(mean_y=weighted.mean(price, w=ipw))

mean.t1$mean_y - mean.t0$mean_y
reg.ipw <- lm(price ~ penalty, data=obs_2012, weights=ipw)

reg.ipw
save.image("Hwk2_workspace.Rdata")

#part4

reg1.dat <- obs_2012 %>% filter(penalty==1)
reg1 <- lm(price ~ quartile_1+ quartile_2+ quartile_3 + quartile_4, data=reg1.dat)
reg0.dat <- obs_2012 %>% filter(penalty==0)
reg0 <- lm(price ~ quartile_1+ quartile_2+ quartile_3 + quartile_4, data=reg0.dat)
pred1_alt <- predict(reg1,new=obs_2012)
pred0_alt <- predict(reg0,new=obs_2012)
mean(pred1_alt-pred0_alt)
linreg <- mean(pred1_alt-pred0_alt)

linreg
save.image("Hwk2_workspace.Rdata")


#altogethernow

finaltable <- data.frame("Inverse Variance" = inv_var$est,
                           "Mahalanobis"=maha$est, 
                           "Inverse Propensity Weight"=reg.ipw$coefficients['penalty'],
                         "Simple Regression Result"= linreg)
finaltable
save.image("Hwk2_workspace.Rdata")





  

  
  
    






