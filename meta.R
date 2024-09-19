library(readxl)
library(metacont)
library(meta)
library(sjPlot)
install.packages("meta")
install.packages("metacont")
install.packages(c("meta", "metasens"))
install.packages("sjPlot")


tab_df(responders,
       title = "Literature review - Responders' proportion studies", footnote = "STN = subthalamic
 nucleus, CM = central medial thalamic nucleus, ANT = anterior thalamic nucleus, Heterogeneous = both partial onset
and multifocal/generalised seizures, OLS = Open Label Sudy, RCT = Randomized Controlled Trial", 
       show.footnote = TRUE, alternate.rows = TRUE )


tab_df(meta,
       title = "Literature review - Seizure reduction studies", footnote = "STN = subthalamic
 nucleus, CM = central medial thalamic nucleus, ANT = anterior thalamic nucleus, Heterogeneous = both partial onset
and multifocal/generalised seizures, OLS = Open Label Sudy, RCT = Randomized Controlled Trial", 
       show.footnote = TRUE, alternate.rows = TRUE )


lit_review<- read_xlsx("Literature_review_DBS.xlsx")


meta <- read_xlsx("Meta-analysis.xlsx")
responders<- read_xlsx("Responders.xlsx")
short_term_responders<- read_xlsx("Short_term_responders.xlsx")
long_term_responders<- read_xlsx("Long_term_responders.xlsx")

summary(meta)

#reduction


meta$n<-as.numeric(meta$n)

class(meta$n)

m.reduction <- metamean(n, Mean_value, SD, Author, data = meta )

summary(m.reduction)

meta::forest(m.reduction, layout="BMJ",common =  FALSE,
             sortvar = Author,
             xlab="Pooled seizure reduction",
             prediction = TRUE, 
             print.tau2 = TRUE,
             ff.xlab = "bold",
             fs.xlab = 12,
             leftcols = c("Author", "Year", "Time_point", "SD", "Mean_value"),
             leftlabs = c("First author et al.", "Year of publication", "Follow-up (months)", NA, "Seizure reduction"))

#Responders


responders <- metaprop(outcome, Number_of_treated_patients,
                                    data = responders,
                                    sm = "PRAW",
                                    studlab = Author)

summary(m.short_term_responders)



forest(responders, layout="Revman5",common = FALSE, 
       sortvar=Author,
       xlab="Pooled analysis \n of responders' proportion",
       pscale = 100,
       ff.xlab = "bold",
       fs.xlab = 12,
       leftcols=c("Author", "Year", "Time_point", "outcome", "Number_of_treated_patients", "w.random", "effect", "ci"),
       leftlabs=c("First author et al.", "Year of publication", "Follow-up (months)", "Responders", "Total patients", "w.random", "Proportion", NA),
       rightcols=FALSE,
       pooled.events = TRUE,
       pooled.totals = TRUE,
       random = TRUE)

###########################

#Seizure reduction



#short term reduction

red_short <- read_xlsx("Reduct_short.xlsx")

short.reduction <- metamean(n, Mean_value, SD, Author, data = red_short )

summary(short.reduction)




meta::forest(short.reduction, layout="BMJ",common =  FALSE,
             sortvar = Author,
             xlab="Short-term (0-12 months) pooled seizure reduction",
             prediction = TRUE, 
             print.tau2 = TRUE,
             ff.xlab = "bold",
             fs.xlab = 12,
             leftcols = c("Author", "Year", "Time_point", "SD"),
             leftlabs = c("First author et al.", "Year of publication", "Follow-up (months)", NA))


#medium term reduction


red_medium <- read_xlsx("Reduct_medium.xlsx")

medium.reduction <- metamean(n, Mean_value, SD, Author, data = red_medium )

summary(medium.reduction)



meta::forest(medium.reduction, layout="BMJ",common =  FALSE,
             sortvar = Author,
             xlab="Medium-term (12-24 months) pooled seizure reduction",
             prediction = TRUE, 
             print.tau2 = TRUE,
             ff.xlab = "bold",
             fs.xlab = 12,
             leftcols = c("Author", "Year", "Time_point", "SD"),
             leftlabs = c("First author et al.", "Year of publication", "Follow-up (months)", NA))



#long term reduction

red_long <- read_xlsx("Reduct_long.xlsx")

long.reduction <- metamean(n, Mean_value, SD, Author, data = red_long )

summary(long.reduction)

meta::forest(long.reduction, layout="BMJ", 
             sortvar = Author,
             xlab="Long-term (≥24 months) pooled seizure reduction",
             prediction = TRUE, 
             print.tau2 = FALSE,
             ff.xlab = "bold",
             fs.xlab = 12,
             leftlabs = c("Author", "g", "SE"))

meta::forest(long.reduction, layout="BMJ",common =  FALSE,
             sortvar = Author,
             xlab="Long-term (≥24 months) pooled seizure reduction",
             prediction = TRUE, 
             print.tau2 = TRUE,
             ff.xlab = "bold",
             fs.xlab = 12,
             leftcols = c("Author", "Year", "Time_point", "SD"),
             leftlabs = c("First author et al.", "Year of publication", "Follow-up (months)", NA))

#### Plot

library(meta)
library(ggplot2)

# Extract necessary data from the meta-analysis results
short_data <- data.frame(
  Time = "Short-term",
  Effect = short.reduction$TE.random,
  Lower_CI = short.reduction$lower.random,
  Upper_CI = short.reduction$upper.random
)

medium_data <- data.frame(
  Time = "Medium-term",
  Effect = medium.reduction$TE.random,
  Lower_CI = medium.reduction$lower.random,
  Upper_CI = medium.reduction$upper.random
)

long_data <- data.frame(
  Time = "Long-term",
  Effect = long.reduction$TE.random,
  Lower_CI = long.reduction$lower.random,
  Upper_CI = long.reduction$upper.random
)

# Combine data into one data frame
combined_data <- rbind(short_data, medium_data, long_data)

# Convert Time to a factor with the correct order
combined_data$Time <- factor(combined_data$Time, levels = c("Short-term", "Medium-term", "Long-term"))

ggplot(combined_data, aes(x = Time, y = Effect, group = 1)) +
  geom_line(color = "red", size = 1.2) +
  geom_point(color = "red", size = 3) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "red") +
  labs(title = "Seizure reduction at different time intervals",
       x = "Time",
       y = "Effect Size") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"))
    
    
ggplot(tgc, aes(x=dose, y=len, colour=supp)) + 
    geom_errorbar(aes(ymin=len-ci, ymax=len+ci), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd)



##################################

#Responders

short_term_responders<- read_xlsx("Short_term_responders.xlsx")
long_term_responders<- read_xlsx("Long_term_responders.xlsx")

#short term responders

m.short_term_responders <- metaprop(outcome, Number_of_treated_patients,
                  data = short_term_responders,
                  sm = "PRAW",
                  studlab = Author)

summary(m.short_term_responders)





forest(m.short_term_responders, layout="Revman5",common = FALSE, 
       sortvar=Author,
       xlab="Short-term (0-24 months) pooled analysis \n of responders' proportion",
       pscale = 100,
       ff.xlab = "bold",
       fs.xlab = 12,
       leftcols=c("Author", "Year", "Time_point", "outcome", "Number_of_treated_patients", "w.random", "effect", "ci"),
       leftlabs=c("First author et al.", "Year of publication", "Follow-up (months)", "Responders", "Total patients", "w.random", "Proportion", NA),
       rightcols=FALSE,
       pooled.events = TRUE,
       pooled.totals = TRUE,
       random = TRUE)

#long term responders

m.long_term_responders <- metaprop(outcome, Number_of_treated_patients,
                       data = long_term_responders,
                       sm = "PRAW",
                       studlab = Author)

summary(m.long_term_responders)





forest(m.long_term_responders, layout="Revman5",common = FALSE, 
       sortvar=Author,
       xlab="Long-term (≥24 months) pooled analysis \n of responders' proportion",
       pscale = 100,
       ff.xlab = "bold",
       fs.xlab = 12,
       leftcols=c("Author", "Year", "Time_point", "outcome", "Number_of_treated_patients", "w.random", "effect", "ci"),
       leftlabs=c("First author et al.", "Year of publication", "Follow-up (months)", "Responders", "Total patients", "w.random", "Proportion", NA),
       rightcols=FALSE,
       pooled.events = TRUE,
       pooled.totals = TRUE,
       random = TRUE)



