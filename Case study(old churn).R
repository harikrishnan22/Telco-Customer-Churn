churn <- read.csv("old churn.csv")
str(churn)
summary(churn)
#visualization 
library(ggplot2)
ggplot2::ggplot(churn)+geom_bar(mapping = aes(x = churn, fill = churn))
ggplot2::ggplot(churn)+geom_bar(mapping = aes(x = state,fill = churn))
ggplot2::ggplot(churn)+geom_bar(mapping = aes(x = international.plan,fill = churn))
ggplot2::ggplot(churn)+geom_bar(mapping = aes(x = voice.mail.plan,fill = churn))
ggplot2::ggplot(churn)+geom_boxplot(mapping = aes(y = number.vmail.messages,fill = churn))
ggplot2::ggplot(churn)+geom_boxplot(mapping = aes(y = total.day.minutes,fill = churn))
ggplot2::ggplot(churn)+geom_boxplot(mapping = aes(y = total.day.calls,fill = churn))
ggplot2::ggplot(churn)+geom_boxplot(mapping = aes(y = total.day.charge,fill = churn))
ggplot2::ggplot(churn)+geom_boxplot(mapping = aes(y = total.eve.minutes,fill = churn))
ggplot2::ggplot(churn)+geom_boxplot(mapping = aes(y = total.eve.calls,fill = churn))
ggplot2::ggplot(churn)+geom_boxplot(mapping = aes(y = total.eve.charge,fill = churn))
ggplot2::ggplot(churn)+geom_boxplot(mapping = aes(y = total.night.minutes,fill = churn))
ggplot2::ggplot(churn)+geom_boxplot(mapping = aes(y = total.night.calls,fill = churn))
ggplot2::ggplot(churn)+geom_boxplot(mapping = aes(y = total.night.charge,fill = churn))
ggplot2::ggplot(churn)+geom_boxplot(mapping = aes(y = total.intl.minutes,fill = churn))
ggplot2::ggplot(churn)+geom_boxplot(mapping = aes(y = total.intl.calls,fill = churn))
ggplot2::ggplot(churn)+geom_boxplot(mapping = aes(y = customer.service.calls,fill = churn))
