library(tidyverse)

#read in vouchers
roy_vouch<-read_csv("data_Royal/royal_all_vouchers_curated_2024_09_23.csv")
yos_vouch<-read_csv("data_Yosemite/Yosemite_CCH_records_2024_09_22.csv")
yos_vouch<-filter(yos_vouch,year>1850,keep_omit=="keep")
roy_inat<-read_csv("data_Royal/inat_rg_27may2024.csv")
yos_inat<-read_csv("data_Yosemite/Yosemite_ResearchGrade.csv")
roy_inat$year<-year(dmy(roy_inat$observed_on))
yos_inat$year<-year(ymd(yos_inat$observed_on))

roy_vouch$park<-"Royal"
roy_inat$park<-"Royal"
yos_vouch$park<-"Yosemite"
yos_inat$park<-"Yosemite"


roy_vouch$`Record type`<-"Voucher"
yos_vouch$`Record type`<-"Voucher"
roy_inat$`Record type`<-"Photograph"
yos_inat$`Record type`<-"Photograph"

both<-rbind(select(roy_vouch,year,park,`Record type`),select(yos_vouch,year,park,`Record type`),select(roy_inat,year,park,`Record type`),select(yos_inat,year,park,`Record type`))

# Event data frame
events <- data.frame(
  year = c(1890, 1864,1879),
  event = c("Park Formation", "Yosemite Grant","Park Formation"),
  park = c("Yosemite", "Yosemite","Royal"),
  y = c(3200,3200, 1900)  # Adjust the vertical position as needed
)

# Plot
ggplot(both, aes(x = year )) +
  geom_bar(aes(fill = `Record type`)) +
  facet_grid(park ~ .,scales = "free") +
  theme_bw() +
  # Add arrows coming down from the top
  geom_segment(
    data = events,
    aes(
      x = year, xend = year,
      y = 700, yend = 0  # Arrow points down to the x-axis
    ),
    arrow = arrow(length = unit(0.2, "cm")),
    color = "blue"
  ) +
  # Add smaller labels for the events
  geom_text(
    data = events,
    aes(
      x = year, y = y,  # Position slightly above the arrow start
      label = event
    ),
    size = 3,  # Smaller text size
    angle = 90, vjust = 0.5, hjust = 1,
    color = "blue"
  )+ylab("Number of records per year")+geom_vline(xintercept=1990,lty="dashed")+xlab("")
