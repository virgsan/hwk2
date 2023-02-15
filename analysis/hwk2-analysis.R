

#question1

reports_per_year <- final.hcris.data%>% 
  group_by(street, year) %>%
  summarise(n = n())
multiple_reports_per_year <- reports_per_year %>%
  filter(n>1)
n_multiple_reports_per_year <- n_distinct(multiple_reports_per_year$street)
n_multiple_reports_per_year


q1 <- multiple_reports_per_year%>% group_by(year)%>% 
  summarize(count = n())
  sum(a$count)
  
graph1 <- ggplot(a, aes(year, count))+
  geom_line()+
  labs( title = "Hospitals with 2+ Filed Reports in One Year", x = 'Year', y = 'Number of Hospitals')+
  theme_minimal()

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

obs_2012 <- obs_2012 %>% 
  filter(!is.na(penalty))%>% 
  filter(!is.na(price))

obs_2012 <- final.hcris.data%>% filter(year == 2012)

obs_2012$penalty <- ifelse(obs_2012$hvbp_payment + obs_2012$hrrp_payment < 0, 1, 0 )
  
table5<- obs_2012%>% filter(! is.na(penalty))%>%
  group_by(penalty)%>%
  summarize(price = mean(price, na.rm= TRUE))

table5

save.image("Hwk2_workspace.Rdata")

#question6

obs_2012 <- obs_2012 %>% 
  filter(!is.na(penalty))%>% 
  filter(!is.na(price))

obs_2012$quartile <- ntile(obs_2012$beds, 4) 

obs_2012$quartile_1 <- ifelse(obs_2012$quartile == 1, 1,0)
obs_2012$quartile_2 <- ifelse(obs_2012$quartile == 2, 1,0)
obs_2012$quartile_3 <- ifelse(obs_2012$quartile == 3, 1,0)
obs_2012$quartile_4 <- ifelse(obs_2012$quartile == 4, 1,0)

table_6 <- obs_2012 %>% group_by(quartile, penalty)%>% summarize(avg_price = mean(price, na.rm = TRUE))

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

#lm(formula = y ~ d, data = select.dat)

#part2
maha <- Matching::Match(Y=obs_2012$price,
                        Tr=obs_2012$penalty,
                        X=obs_2012$quartile,
                        M=1,
                        Weight=2,
                        estimand="ATE")
summary(maha)

#part3
logit.model <- glm(penalty ~ beds,
                   family=binomial,
                   data=obs_2012_f)

ps <- fitted(logit.model)


obs_2012 <- obs_2012 %>%
  mutate(ipw = case_when(
    penalty== 1 ~ 1/ps,
    penalty== 0 ~ 1/(1-ps),
    TRUE ~ NA_real_
  ))

logit.reg <- glm(penalty ~ obs_2012_f$quartile_1 + obs_2012_f$quartile_2 + 
                   obs_2012_f$quartile_3 + obs_2012_f$quartile_4,
                 data = final.hcris.data, family = binomial)
final.hcris.data <- final.hcris.data %>%
  mutate(ps = predict(logit.reg, type = 'response')) %>%
  filter(ps>0 & ps<1)

# Create IPW weights
final.hcris.data <- final.hcris.data %>%
  mutate(ipw = case_when(
    penalty == 1 ~ 1/ps,
    penalty == 0 ~ 1/(1-ps),
    TRUE~NA_real_
  ))
view(final.hcris.data)

mean.t1 <- final.hcris.data %>% 
  filter(penalty==1) %>% 
  dplyr::select(price, ipw) %>%
  summarize(mean_y=weighted.mean(price, w=ipw))
mean.t0 <- final.hcris.data %>% 
  filter(penalty==0) %>% 
  dplyr::select(price, ipw) %>%
  summarize(mean_y=weighted.mean(price, w=ipw))
mean.t1$mean_y - mean.t0$mean_y
reg.ipw <- lm(price ~ penalty, data=final.hcris.data, weights=ipw)
reg.ipw

#part4

obs_2012 <- obs_2012%>% filter(penalty==1)
reg1 <- lm(price ~ beds+ mcaid_discharges + ip_charges + mcare_discharges +
             tot_mcare_payment, data=reg1.dat)
reg0.dat <- lp.vars %>% filter(penalty==0
reg0 <- lm(price ~ beds + mcaid_discharges + ip_charges + mcare_discharges +
             tot_mcare_payment, data=reg0.dat)
pred1 <- predict(reg1,new=lp.vars)
pred0 <- predict(reg0,new=lp.vars)
mean(pred1-pred0)














#ian q5
yr_2012 <- final.hcris.data %>% filter(year==2012) %>% ungroup() %>%
  mutate(beds_q1 = quantile(beds, probs=0.25, na.rm=TRUE),
         beds_q2 = quantile(beds, probs=0.50, na.rm=TRUE),
         beds_q3 = quantile(beds, probs= 0.75, na.rm= TRUE), 
         beds_q4 = max(beds, na.rm = TRUE)) %>%
  mutate(bed_size1 = ifelse(beds<beds_q1, 1,0 ),
         bed_size2 = ifelse(beds>= beds_q1 & beds<beds_q2, 1,0),
         bed_size3 = ifelse(beds>= beds_q2 & beds<beds_q3, 1,0),
         bed_size4 = ifelse(beds>  beds_q3 & beds<beds_q4, 1,0))
         

  

  
  
    






