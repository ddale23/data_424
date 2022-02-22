#packages we need
library(tidyverse)
library(leaps)
library(corrplot)
library(MASS)

#pull in data from github
base_data = read.csv('https://raw.githubusercontent.com/ddale23/data_424/main/startup_data.csv')
#base_data = read.csv(file.choose())


#adjust data names
base_data =
base_data %>%
  rename(rd_spend = R.D.Spend,
         admin = Administration,
         marketing = Marketing.Spend,
         state = State,
         profit = Profit)

#view base data
base_data


#create dummy variables
base_data =
base_data %>%
  mutate(flor_dum = case_when(
    state == "Florida" ~ 1,
    state!= "Florida" ~ 0
  ),
  cal_dum = case_when(
    state == "California" ~ 1,
    state!= "California" ~ 0
  ))


#reordering for ease of use
re_ordered =
  base_data %>%
  mutate(rd_spend = as.numeric(rd_spend),
         admin = as.numeric(admin),
         marketing = as.numeric(marketing),
         profit = as.numeric(profit)) %>%
  select(profit, rd_spend, admin, marketing, flor_dum, cal_dum)


#setting aside data into a train and test set
n = nrow(re_ordered)
set.seed(236)

sample_train = sample(1:n, size=n/5)

data_test = re_ordered[sample_train, ]

data_train = re_ordered[-sample_train, ]


#view the test and train sets
view(data_train)
view(data_test)


#view relationships between variables
pairs(data_train[,1:4], lower.panel = NULL)


#correlation plot
cor = cor(data_train[,1:4])
corrplot(cor, method = 'number')



#build initial model
initial_model = lm(profit ~ rd_spend + admin + marketing, data = data_train)

#summary of initial_model
summary(initial_model)

#view the normality plots of the initial_model
plot(initial_model)



#transformations of quantitative data --> take a look at each one slr
ggplot(data_train, aes(rd_spend, profit)) +
  geom_point() +
  theme_bw()


#boxcox transformation on original model
bc_all = boxcox(profit ~ rd_spend + admin + marketing, data = data_train)
adj_all = bc$x[which.max(bc$y)]
adj_all 

#just rd_spend
bc_profit = boxcox(profit ~ rd_spend, data = data_train)
adj_profit = bc_profit$x[which.max(bc_profit$y)]
adj_profit

#just admin
bc_admin = boxcox(profit ~ admin, data = data_train)
adj_admin = bc_admin$x[which.max(bc_admin$y)]
adj_admin

#just marketing
bc_marketing = boxcox(profit ~ marketing, data = data_train)
adj_marketing = bc_marketing$x[which.max(bc_marketing$y)]
adj_marketing

#putting them all together into one frame
box_cox_adj = c(adj_all, adj_profit, adj_admin, adj_marketing)
names =c("all", "profit", "admin", "marketing")

names
box_cox_adj

# data frame with type and box cox value
final_box_values = data.frame(names, box_cox_adj)
final_box_values
# --> all close to 1 so that indicates we would not perform any transformation



##now to step-wise regression model

#formula we will use
step_one = lm(profit ~ ., data = data_train)

step_for = lm(profit ~ 1, data = data_train)

full_formula = formula(profit ~ ., data = data_train)

for_formula = formula(profit ~ 1, data = data_train)

forward_comp = step(step_for, scope = full_formula, direction="forward", test="F")

fstep_comp = step(step_one, scope = full_formula, direction="backward", test="F")

reg_subset = regsubsets( profit ~ . , data = data_train)




#formula is profit ~ rd_spend + intercept
leap_mod_sum = summary(reg_subset)

leap_mod_sum

fstep_comp$coefficients

plot(fstep_comp)


summary(fstep_comp)














#mod_sum
data.frame(
  Adj.R2 = which.max(leap_mod_sum$adjr2),
  CP = which.min(leap_mod_sum$cp),
  BIC = which.min(leap_mod_sum$bic)
)




plot(reg_subset, scale="adjr2")

plot(reg_subset, scale="bic")



 

View(df)






#this is the model

final_step = step(step_one, scope = full_formula, direction="both", test="F")
summary(step(step_one, scope = full_formula, direction="both", test="F"))

plot(step(step_one, scope = full_formula, direction="both", test="F"))

final_step$coefficients




full_formula = formula(lm(profit ~ ., data = data_train))

fit_int = lm(profit~ 1, data = data_train)

step(fit_int, scope = full_formula, direction = 'backward', test = 'F')




# formula 4.714735e+04 + 8.768002e-01(rd_spend)
data_test = 
data_test %>%
  mutate(pred_rd_ = 8.768002e-01 * rd_spend)

data_test =
data_test %>%
  mutate(prof_pred = pred_rd_ + 4.714735e+04) 

data_test %>%
  ggplot(aes(profit, prof_pred)) + geom_point()+
  theme_bw() +
  geom_smooth(se = F, method = lm, color = "red") +
  labs(x = "Actual Profit",
       y = "Predicted Profit",
       title = "Actual vs. Predicted Profit",
       subtitle = "using the step wise regression model") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

#y's
data_test = 
data_test %>%
  mutate(y_i = (profit - prof_pred)*(profit - prof_pred))


final_yi = sum(data_test$y_i)
final_yi /38

view(data_test)



2.863*(10*10*10)



full_formula_profit = formula(profit~.)

fit_full_wage = lm(full_formula_profit, data = data_train)

fit_int_wage = lm(profit ~ 1, data = data_train)

step(fit_int_wage, scope = full_formula_profit, direction = "both", test = "f")



#test data












#MSPE
MSPE <- sum( (data_test$profit-pred_val))/nrow(data_test) 
MSPE

#Actual vs Predicted Graph --> Test Set
data_test %>%
  ggplot(aes(profit, prof_pred)) + geom_point()+
  theme_bw() +
  geom_smooth(se = F, method = lm, color = "red") +
  labs(x = "Actual Profit",
       y = "Predicted Profit",
       title = "Actual vs. Predicted Profit",
       subtitle = "using the step wise regression model") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 12))










formula(profit ~ admin +  +
          HospitalBeds + PercPoverty + 
          IncomePerCapita + IncomePersonal +
          factor(Region))




nrow(test_data)


pred_val = predict(final_step, newdata = data_test)

pred_val


MSPE <- sum( (data_test$profit-pred_val))/nrow(data_test) 

MSPE

FitVal <- lm(full_formula, data=pred_val)
CIsel <- confint(FitSelected)
CIval <- confint(FitVal)


view(data_train)

summary(final_step)

