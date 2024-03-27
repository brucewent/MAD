
library(tidyverse)

trends1 <- read_csv('data/MAD Training Trends.csv',
                    col_types='Diiiiic')
trends1
# View(trends1)
# glimpse(trends1)
# knitr::kable(trends1)
# trends1$Date
cc<-colors()

trends2 <- select(trends1, !File_Name) |>
  pivot_longer(cols = c("Trained","Not_Trained","YPT_Current","Never_Taken","Expired"),
               names_to = "Status", values_to = "Count") |>
  mutate(Status = factor(Status, levels=c("Not_Trained","Trained","Never_Taken","Expired","YPT_Current")))
trends2

#,color=c("Red","Blue")

filter(trends2, Status=="Trained" | Status=="Not_Trained") |>
  ggplot(aes(x=Date, y=Count, fill=Status)) +
  geom_bar(position = "fill",stat = "identity") +
  geom_text(stat = "count", aes(nudge_y = 1)) +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("Percentage") +
  ggtitle("Mercer Area District Trained Leaders")

filter(trends2, Status=="YPT_Current" | Status=="Never_Taken" | Status=="Expired") |>
  ggplot(aes(x=Date, y=Count, fill=Status)) +
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("Percentage") +
  ggtitle("Mercer Area District Youth Protection Trining")

###

vignette("ggplot2-specs")

filter(trends2, Status=="Trained" | Status=="Not_Trained") |>
  ggplot(aes(x=Date, y=Count, fill=Status)) +
  geom_area(position = "fill",stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_area(alpha=.5, linewidth=1, colour="black") +
  ylab("Percentage") +
  ggtitle("Mercer Area District Trained Leaders")

library(plotly)
library(ggplot2)
ggplotly(filter(trends2, Status=="Trained" | Status=="Not_Trained") |>
           ggplot(aes(x=Date, y=Count, fill=Status)) +
           geom_area(position = "fill",stat = "identity") +
           scale_y_continuous(labels = scales::label_percent()) +
           geom_area(alpha=0.6 , linewidth=1, colour="black") +
           ylab("Percentage") +
           ggtitle("Mercer Area District Trained Leaders"))

filter(trends2, Status=="Trained" | Status=="Not_Trained") |>
  ggplot(aes(x=Date, y=Count, fill=Status)) +
  geom_point() +
  geom_smooth()
