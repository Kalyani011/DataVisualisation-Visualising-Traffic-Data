library(readr)
library(ggplot2)
library(dplyr)
library(ggforce)
library(colorspace)
library(viridis)
library(lubridate)
library(scales)
library(RColorBrewer)
library(treemapify)
library(plotly)
library(ggridges)

TRAFFIC <- read_csv(file = 'Junction Turning Counts 2016 Outside DSI_LONG_FORMAT.csv')

# PART 1 START
TRAFFIC <- TRAFFIC %>%  mutate(vehicle_fullname = case_when(
  vehicle == "PCL" ~ "Pedal Cycle",
  vehicle == "MCL"  ~ "Motorcycle",
  vehicle == "CAR" ~ "Cars",
  vehicle == "TAXI" ~ "Taxi Vehicles",
  vehicle == "LGV" ~ "Light Goods Vehicle",
  vehicle == "OGV1" ~ "Ordinary Goods Vehicle 1",
  vehicle == "OGV2" ~ "Ordinary Goods Vehicle 2",
  vehicle == "CDB" ~ "City Direct Bus",
  vehicle == "BEB" ~ "Bus Eireann Bus",
  vehicle == "OB" ~ "Other Bus"
))

TRAFFIC_SEC1 <- TRAFFIC %>% group_by(TIME, vehicle_fullname) %>% summarize(count=sum(count))

TRAFFIC_SEC1 <- TRAFFIC_SEC1 %>%
  mutate(Hour = paste0(format.Date(TIME, "%H:%M"),"\nVehicle Type: ", vehicle_fullname,
                       "\nVolume of Traffic: ", count))
library(plotly)
g <- ggplot(TRAFFIC_SEC1, aes(y=vehicle_fullname, x=count, label=Hour)) + 
  geom_point(size = 1.25, alpha = 0.2, 
              position = position_jitter(width = 1.7)
             ) +
  scale_x_continuous(expand=c(0,0), 
                     breaks = seq(0,450,by=50),
                     limits= c(0,450),
                     name = "Count of Vehicles at Junction per 15 Minutes") +
  ggtitle("Distribution of Traffic at Junction on November 23, 2016") +
  theme_minimal() +
  theme (
    panel.grid.major.y =element_line(colour = "gray95", size=0.25),
    panel.grid.minor.x =element_line(colour = "gray95", size=0.15),
    panel.grid.major.x =element_line(colour = "gray95", size=0.25),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size=8, face="bold"),
    axis.text.x = element_text(size=8),
    legend.title = element_blank(),
    plot.margin = margin(t = 4, r = 10, b = 4, l = 4, "pt"),
    plot.title = element_text(size=12, hjust=0.45, face="bold"),
    legend.position = "none"
    )

fig <- ggplotly(g,tooltip = c("label"))
fig

# PART 1 END

# PART 2 START
TRAFFIC_SEC2 <- filter(TRAFFIC, turn %in% c("DA", "DB", "DC"))

TRAFFIC_SEC2 <- TRAFFIC_SEC2 %>%
  mutate(TIME = case_when(
    (TIME >= "2016-11-23 07:00:00 UTC" & TIME < "2016-11-23 09:30:00 UTC")  ~ "Early Morning",
    (TIME >= "2016-11-23 09:30:00 UTC" & TIME < "2016-11-23 12:00:00 UTC")  ~ "Late Morning",
    (TIME >= "2016-11-23 12:00:00 UTC" & TIME < "2016-11-23 14:30:00 UTC")  ~ "Afternoon",
    (TIME >= "2016-11-23 14:30:00 UTC" & TIME < "2016-11-23 17:00:00 UTC")  ~ "Late Afternoon",
    (TIME >= "2016-11-23 17:00:00 UTC" & TIME < "2016-11-23 19:00:00 UTC")  ~ "Evening",
    )) %>% 
  group_by(turn, TIME) %>%
  summarize(count=sum(count)) %>%
  mutate(TIME = factor(TIME, levels =
                         c("Early Morning",
                           "Late Morning",
                           "Afternoon",
                           "Late Afternoon",
                           "Evening")))

TRAFFIC_SEC2 <- TRAFFIC_SEC2 %>%  mutate(turn = case_when(
  turn == "DA" ~ "Road D to Road A",
  turn == "DB" ~ "Road D to Road B",
  turn == "DC" ~ "Road D to Road C"
)) %>%  mutate(turn = factor(turn, levels = c(
  "Road D to Road A",
  "Road D to Road B",
  "Road D to Road C"
)))

# TRAFFIC_SEC2 <- TRAFFIC_SEC2[order(TRAFFIC_SEC2$TIME),]

TRAFFIC_SEC2_ps <- gather_set_data(TRAFFIC_SEC2, c(1,2))

TRAFFIC_SEC2_ps$x <- factor(TRAFFIC_SEC2_ps$x, levels = c("turn",  "TIME"))


ggplot(TRAFFIC_SEC2_ps, aes(x, id=id, split=y, value=count)) +
  geom_parallel_sets(aes(fill = turn), alpha = 0.5, axis.width = 0.11) +
  geom_parallel_sets_axes(axis.width = 0.1, 
                          fill = "grey80", color = "grey80"
                          ) +
  geom_parallel_sets_labels(
    color = 'black',
    size = 6/.pt,
    angle = 0
  ) +
  scale_x_discrete(
    name = NULL,
    expand = c(0, 0.1),
    labels = c("TURN", "TIME")
  ) +
  scale_y_continuous(
    #breaks = NULL,
    expand = c(0, 0),
    limits = c(0, 3800),
    breaks = seq(0, 3800, by=600),
    labels = seq(0, 3800, by=600),
    name = "Vehicle count at each turn"
  )+
  scale_fill_manual(
    values = qualitative_hcl(3, palette = "cold"),
    guide = "none"
  ) +
  theme(
    axis.line = element_blank(),
    plot.margin = margin(14, 2, 2, 2),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.background = element_blank(),
    legend.key        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    strip.background  = element_blank(),
    plot.background   = element_blank(),
    panel.grid= element_blank(),
    plot.title = element_text(size = 11, hjust = 0.24)
  )+
  ggtitle("Proportions of traffic coming from road D turning into roads A, B and C")
# PART 2 END

# PART 3 START
TRAFFIC_SEC3 <- TRAFFIC %>% group_by(vehicle_fullname, TIME) %>% summarise(all_count=sum(count))

g <- ggplot(TRAFFIC_SEC3, aes(x=TIME, y=vehicle_fullname, fill=all_count, label=all_count)) +
  # setting the heat-map cells to be filled by scale_fill_viridis_C
  geom_tile(colour="white", size=1) +
  # geom_text(aes(label=all_count),
  #           colour = ifelse(TRAFFIC_SEC3$all_count>100,"white", "black"),
  #           size=1.95) +
  # removing the spaces between plot and y-axis
  scale_y_discrete(name=NULL, expand=c(0,0)) +
  
  # scaling the TIME variable
  scale_x_datetime(date_breaks="15 min", date_labels="%H:%M", expand=c(0,0)) +
  # setting the gradients in reverse order
  scale_fill_viridis_c(option="B", direction=-1, 
                       guide=guide_colourbar(direction="horizontal",
                                             barwidth=14,
                                             barheight=1.5)) +
  ggtitle("Volume of Vehicles at Junction on November 23, 2016") +
  # setting the theme of plot
  theme(axis.text.y = element_text(size=10, face="bold", vjust=-0.2),
        axis.text.x = element_text(size=8, angle=45, vjust=0.55),
        axis.ticks.x = element_line(size=0.5, colour="grey30"),
        axis.line.x = element_line(size=0.5, colour="grey30"),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        legend.text = element_text(size=9),
        legend.position = c(0.5, -0.4),
        legend.title = element_blank(),
        legend.margin = margin(r=5, l=5, t=2, b=2),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(1, 0.25, 2.5, 0), "cm"),
        plot.title = element_text(size=16, hjust=0.45, vjust=0.2, face="bold")
  )

fig <- ggplotly(g,tooltip = c("label"))
fig
  
# PART 3 END

# PART 4 START
TRAFFIC_SEC4 <- TRAFFIC %>% mutate(vehicle_category = case_when(
  vehicle == "PCL" ~ "Two-wheel vehicles",
  vehicle == "MCL"  ~ "Two-wheel vehicles",
  vehicle == "CAR" ~ "Cars",
  vehicle == "TAXI" ~ "Cars",
  vehicle == "LGV" ~ "Goods vehicles",
  vehicle == "OGV1" ~ "Goods vehicles",
  vehicle == "OGV2" ~ "Goods vehicles",
  vehicle == "CDB" ~ "Buses",
  vehicle == "BEB" ~ "Buses",
  vehicle == "OB" ~ "Buses"
))

TRAFFIC_SEC4$vehicle_category <- factor(TRAFFIC_SEC4$vehicle_category, 
                                        levels = c("Two-wheel vehicles", 
                                                   "Cars", 
                                                   "Goods vehicles",
                                                   "Buses"))

TRAFFIC_SEC4 <- TRAFFIC_SEC4 %>%
  group_by(vehicle_category, vehicle_fullname) %>%
  summarise(vehicle_count=sum(count))

TRAFFIC_SEC4$vehicle_count_norm <- as.numeric((TRAFFIC_SEC4$vehicle_count - 
                                 min(TRAFFIC_SEC4$vehicle_count)) / 
  (max(TRAFFIC_SEC4$vehicle_count) - min(TRAFFIC_SEC4$vehicle_count))) + (0.00001 * 2000)

n <- length(unique(TRAFFIC_SEC4$vehicle_category))


TRAFFIC_SEC4$index <- as.numeric(factor(TRAFFIC_SEC4$vehicle_category))


TRAFFIC_SEC4 <- TRAFFIC_SEC4 %>%
  group_by(index) %>%
  mutate(
    max_count = max(vehicle_count),
    colour = gradient_n_pal(
      sequential_hcl(
        6,
        h = 360 * index[1]/n,
        c = c(40, 10),
        l = c(20, 90),
        power = 0.6)
    )(1-(vehicle_count/max_count)))


ggplot(data=TRAFFIC_SEC4, aes(area=vehicle_count_norm, 
                               fill=colour, 
                               subgroup=vehicle_category)) +
  geom_treemap(colour="white", size=0.5*.pt, alpha = NA) +
  geom_treemap_text(aes(label=vehicle_fullname), 
                    colour=ifelse(TRAFFIC_SEC4$vehicle_count_norm<0.03,"grey97","grey30"),
                    size=14,
                    place="center",
                    fontface="bold",
                    padding.x = grid::unit(1.5, "mm"),
                    padding.y = grid::unit(1.5, "mm")
                    ) +
  geom_treemap_text(aes(label = format(vehicle_count, nsmall=0, big.mark=",",trim=TRUE)), 
                    color = "black", 
                    size = 14,  
                    min.size = 0, 
                    padding.x = grid::unit(1.5, "mm"),
                    padding.y = grid::unit(1.5, "mm"),
                    place = "topright"
  ) +
  geom_treemap_subgroup_border(colour="white", size=2) +
  geom_treemap_subgroup_text(grow=FALSE,
                             colour="#FAFAFA", 
                             size=42,
                             place="bottomleft", 
                             fontface = "bold", 
                             alpha = 0.4) +
  ggtitle("Proportions of Vehicle Categories and Sub-Categories") +
  # setting to ensure the scaling applied does not change
  scale_fill_identity() +
  theme(
    plot.title = element_text(size=16, hjust = 4.5, face="bold", color = "grey20")
  )
# +
  # coord_cartesian(clip="off") +
  # guides(colour="NA", fill="none") 
# PART 4 END
