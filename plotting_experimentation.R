library(nba)
sp_pol <- half_court_raster()
sp_pol <- merge_shot_data(sp_pol, filter(shots_1516, PLAYER_NAME == "LeBron James", SHOT_ZONE_BASIC != "Restricted Area"))

# colors <- c("white", "black")
colors <- c("#b7d2e4", "#ff6688")
# colors <- c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177','#49006a')
num_cols <- length(unique(sp_pol@data$counts))
plotclr <- colorRampPalette(colors)(num_cols)
# breaks <- seq(0, max(max(unique(sp_pol@data$counts), na.rm = TRUE)), length.out = num_cols + 1)
# class <- classIntervals(temp$trump, num_cols, style = "fixed", fixedBreaks = breaks)
colcode <- plotclr[findInterval(sp_pol@data$counts, vec = sort(unique(sp_pol@data$counts)))]
# plot(sp.p, col = colcode, border = "white")

# pdf(file = "test_shotchart.pdf", height = 10, width = 10)
# plot(sp_pol, col = colcode, border = "transparent")
# dev.off()

plot(sp_pol, col = colcode, border = "transparent")
draw_halfcourt(add = TRUE)
legend(0, -50, c("low shot freq", "high shot freq"),
       col = c("#b7d2e4", "#ff6688"), pch = c(15,15), pt.cex = 2,
       cex = 0.7, ncol = 2, xjust = 0.5, bty = "n")


shots_1516 <- get_shotchart(
  SeasonType = "Regular+Season",
  LeagueID = "00",
  Season = "2015-16",
  PlayerID = "0",
  TeamID = "1610612739",
  GameID = "",
  ContextMeasure = "FGA",
  PlayerPosition = "",
  DateFrom = "",
  DateTo = "",
  GameSegment = "",
  LastNGames = "0",
  Location = "",
  Month = "0",
  OpponentTeamID = "0",
  Outcome = "",
  SeasonSegment = "",
  VSConference = "",
  VSDivision = "",
  RookieYear = "",
  Period = "0",
  StartPeriod = "",
  EndPeriod = "")

inferno_colors <- c('#000004', '#010107', '#02020C', '#030312', '#050417', '#07051D', '#0A0723', '#0D0829', '#100A2F', '#140B35', '#170C3B', '#1B0C41', '#1F0C48', '#230C4E', '#280B53', '#2C0B58', '#310A5D', '#350960', '#3A0963', '#3E0966', '#430A68', '#470B6A', '#4B0C6B', '#4F0D6C', '#540F6D', '#58106E', '#5C126E', '#60136E', '#64156E', '#68166E', '#6C186E', '#70196E', '#741B6E', '#781C6D', '#7D1E6D', '#811F6C', '#85216B', '#89226A', '#8D2369', '#912568', '#952667', '#992865', '#9D2964', '#A12B62', '#A52D60', '#A92E5E', '#AD305C', '#B1325A', '#B53458', '#B93656', '#BD3853', '#C03A51', '#C43C4E', '#C83F4C', '#CB4149', '#CF4446', '#D24644', '#D54941', '#D84C3E', '#DB4F3B', '#DE5338', '#E15635', '#E45A32', '#E65D2F', '#E9612B', '#EB6528', '#ED6925', '#EF6D22', '#F1711E', '#F3751B', '#F47A18', '#F67E14', '#F78311', '#F8870E', '#F98C0A', '#FA9008', '#FB9506', '#FB9A06', '#FC9F07', '#FCA409', '#FCA80D', '#FCAD12', '#FCB217', '#FBB71C', '#FBBC22', '#FAC128', '#F9C72E', '#F8CC35', '#F7D13C', '#F6D643', '#F5DB4B', '#F4E054', '#F3E45D', '#F2E967', '#F1EE71', '#F2F27C', '#F3F587', '#F5F991', '#F8FC9B', '#FCFFA4')

ggplot(filter(shots_1516, PLAYER_NAME == "LeBron James", SHOT_ZONE_BASIC != "Restricted Area"), aes(LOC_X, LOC_Y)) +
  stat_density2d(geom = "raster", aes(fill = ..density..), contour = FALSE, interpolate = TRUE, n = 200) +
  scale_fill_gradientn("Frequency", colors = viridis::magma(100),
                       limits = c(0,1),
                       breaks = c(0,1),
                       labels = c("low", "high"),
                       guide = guide_colorbar(barwidth = 10, barheight = 0.5, ticks = FALSE, title.position = "top")) +
  theme(panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        panel.grid = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.title.align = 0.5,
        legend.title = element_text(size = 10),
        legend.text = element_text(color = "white"),
        title = element_text(color = "white")) +
  ggtitle("Player Shot Frequency", subtitle = "LeBron James")


ggplot(filter(shots_1516, PLAYER_NAME == "LeBron James"), aes(LOC_X, LOC_Y)) +
  geom_point(alpha = 0.7, size = 3, aes(color = EVENT_TYPE), shape=3) +
  scale_color_manual(values=c("#FF6688", "#336688"), name = "") +
  theme(panel.background = element_rect(fill = "#f5f5f2", color = "#f5f5f2"),
        plot.background = element_rect(fill = "#f5f5f2", color = "#f5f5f2"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks.length = unit(0, "lines"),
        panel.grid = element_blank(),
        title = element_text(color = "black"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom") +
  ggtitle("Player Shot Accuracy", subtitle = "LeBron James")

# ggplot(filter(shots_1516, PLAYER_NAME == "LeBron James"), aes(LOC_X, LOC_Y)) +
#   stat_bin_hex(bins = 30) +
#   scale_fill_gradientn(colors = viridis::magma(100)) +
#   theme(panel.background = element_rect(fill = "#f5f5f2", color = "#f5f5f2"),
#         plot.background = element_rect(fill = "#f5f5f2", color = "#f5f5f2"),
#         axis.title=element_blank(),
#         axis.text=element_blank(),
#         axis.ticks.length = unit(0, "lines"),
#         panel.grid = element_blank(),
#         title = element_text(color = "black"),
#         legend.background = element_blank(),
#         legend.key = element_blank(),
#         legend.position = "bottom") +
#   ggtitle("Player Shot Accuracy")

dat <- filter(shots_1516, PLAYER_NAME == "LeBron James")
hx <- hexbin(dat$LOC_X, dat$LOC_Y, xbins = 30, IDs = TRUE)
hx
hex_df <- data.frame(cbind(count = hx@count, cell = hx@cell, LOC_X = hx@xcm, LOC_Y = hx@ycm))
dat$cell <- hx@cID
hex_df$count_scaled <- sapply(hex_df$count,
                              function(x){
                                out <- which(x == sort(unique(hex_df$count)))
                                return(log(out + 1))
                                })
success_table <- summarise(group_by(dat, cell), success = length(which(EVENT_TYPE == "Made Shot"))/n())
hex_df <- left_join(hex_df, success_table, by = "cell")

colorful <- c("#236e96", "#15b2d3", "#5abccc", "#ffbe42", "#ff7f00", "#f4543b", "#f4143b")

ggplot(hex_df, aes(LOC_X, LOC_Y)) +
  geom_point(shape = 15, alpha = 0.5, aes(color = success, size = hex_df$count_scaled)) +
  scale_color_gradientn("Success",
                        # colors = viridis::magma(100),
                        colors = colorRampPalette(colorful)(100),
                        limits = c(0, 1),
                        breaks = c(0, 1),
                        labels = c("low", "high"),
                        guide = guide_colorbar(barwidth = 5, title.position = "top", ticks = FALSE)) +
  scale_size_continuous("Frequency", breaks = c(1,3), labels = c("low", "high"),
                        guide = guide_legend(override.aes = list(colour = "#ababa9", alpha = 1), title.position = "top")) +
  theme(panel.background = element_rect(fill = "#f5f5f2", color = "#f5f5f2"),
        plot.background = element_rect(fill = "#f5f5f2", color = "#f5f5f2"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks.length = unit(0, "lines"),
        panel.grid = element_blank(),
        title = element_text(color = "black"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.title.align = 0.5,
        legend.title = element_text(size = 10)) +
  ggtitle("LeBron James", subtitle = "Player Shot Frequency & Success Rate")

fsp_pol@data$lon <- coordinates(sp_pol)[1,]
sp_pol@data$lat <- coordinates(sp_pol)[2,]

ggplot(data = sp_pol@data) +
  geom_polygon(aes(x = lon, y = lat, fill = layer)) +
  theme(panel.background = element_blank(),
        plot.background = element_blank())

us_df <- fortify(sp_pol, region = "layer")
sp_pol@data$id <- sp_pol@data$layer
us_df <- plyr::join(us_df, sp_pol@data, by="id")

pdf("shotchart_ggplot.pdf", height = 10, width = 10)
ggplot(us_df) +
  geom_polygon(aes(x = long, y = lat, fill = counts, group = id)) +
  scale_fill_gradientn(colors = viridis::magma(100), na.value = "#f5f5f2") +
  theme(panel.background = element_blank(),
        plot.background = element_blank())
dev.off()

