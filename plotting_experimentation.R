library(nba)

sp_pol <- half_court_raster()
sp_pol <- merge_shot_data(sp_pol, filter(shots_1617, PLAYER_NAME == "LeBron James", SHOT_ZONE_BASIC != "Restricted Area"))

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

ggplot(filter(shots_1617, PLAYER_NAME == "LeBron James", SHOT_ZONE_BASIC != "Restricted Area"), aes(LOC_X, LOC_Y)) +
  stat_density2d(geom = "raster", aes(fill = ..density..), contour = FALSE, interpolate = TRUE, n = 200) +
  scale_fill_gradientn("Frequency", colors = viridis::magma(100),
                       limits = c(0,1.7e-5),
                       breaks = c(0,1.7e-5),
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


ggplot(filter(shots_1617, PLAYER_NAME == "LeBron James"), aes(LOC_X, LOC_Y)) +
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

dat <- filter(shots_1617, PLAYER_NAME == "Stephen Curry")
hx <- hexbin(dat$LOC_X, dat$LOC_Y, xbins = 30, IDs = TRUE)
hx
hex_df <- data.frame(cbind(count = hx@count, cell = hx@cell, LOC_X = hx@xcm, LOC_Y = hx@ycm))
dat$cell <- hx@cID
hex_df$count_scaled <- sapply(hex_df$count,
                              function(x){
                                out <- which(x == sort(unique(hex_df$count)))
                                return(log(out))
                                })
success_table <- summarise(group_by(dat, cell), success = length(which(EVENT_TYPE == "Made Shot"))/n())
hex_df <- left_join(hex_df, success_table, by = "cell")

colorful <- c("#236e96", "#15b2d3", "#5abccc", "#ffbe42", "#ff7f00", "#f4543b", "#f4143b")

ggplot(hex_df, aes(LOC_X, LOC_Y)) +
  geom_point(shape = 15, alpha = 0.5, aes(color = success, size = hex_df$count_scaled)) +
  scale_color_gradientn("Success (FG%)",
                        # colors = viridis::magma(100),
                        colors = colorRampPalette(colorful)(100),
                        limits = c(0, 1),
                        breaks = c(0, 1),
                        labels = c("low", "high"),
                        guide = guide_colorbar(barwidth = 10, barheight = 0.5, title.position = "top", ticks = FALSE)) +
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
  xlim(-300, 300) + ylim(-100,500) +
  ggtitle("LeBron James", subtitle = "Player Shot Frequency & Success Rate")

# Plot hexagon shot chart

dat_filt <- filter(shots_1617, PLAYER_NAME == "LeBron James", SHOT_ZONE_BASIC != "Restricted Area", LOC_Y <= 500)

hex_grid <- nba:::half_court_hex(cellsize = 20)
hex_grid <- nba::merge_shot_data(hex_grid, dat_filt, hex = TRUE)
resized_hexes <- nba:::resize_hexes(hex_grid)

colorful <- c("#236e96", "#15b2d3", "#5abccc", "#ffbe42", "#ff7f00", "#f4543b", "#f4143b")

resized_hexes@data$id <- as.character(seq(1, nrow(resized_hexes@data)))
ggplot_df <- fortify(resized_hexes, region = "id")
ggplot_df <- left_join(ggplot_df, resized_hexes@data, by = "id")

ggplot(ggplot_df, aes(x = long, y = lat, group = id, fill = FGP)) +
  geom_polygon() +
  scale_fill_gradientn("Success (FG%)",
                       # colors = viridis::magma(100),
                       colors = colorRampPalette(colorful)(10),
                       limits = c(0, 1),
                       breaks = c(0, 1),
                       labels = c("low", "high"),
                       na.value = "#f5f5f2",
                       guide = guide_colorbar(barwidth = 10, barheight = 0.5, title.position = "top", ticks = FALSE)) +
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
  xlim(-300, 300) + ylim(-100,500) +
  ggtitle("LeBron James", subtitle = "Player Shot Frequency & Success Rate")


### base r

# hexagon maps
remove(list = ls())
dat_filt <- filter(shots_1617, PLAYER_NAME == "Karl-Anthony Towns", LOC_Y <= 500)
# Construct hexagon shot chart (area/color varying)
hex_grid <- nba:::half_court_hex(cellsize = 15)
hex_grid <- nba:::merge_shot_data(hex_grid, dat_filt, hex = TRUE)
resized_hexes <- nba:::resize_hexes(hex_grid)
# Plot hexagon shot chart
breaks <- seq(0, max(hex_grid@data$FGP, na.rm = TRUE), length.out = 10)
colorful <- c("#236e96", "#15b2d3", "#5abccc", "#ffbe42", "#ff7f00", "#f4543b", "#f4143b")
colcode <- colorful[findInterval(resized_hexes@data$FGP, breaks)]
par(bg = "#f5f5f2")
draw_halfcourt()
plot(hex_grid, col = colcode, border = NA, add = TRUE)

# streamline plotting

plot_hexes <- function(dat, player, ..., variable = "FGP", cellsize = 15, scale = TRUE) {
  col_breaks <- 7
  dat_filt <- filter(dat, PLAYER_NAME %in% player, ...)
  dat_leagueomit <- filter(shots_1617, !PLAYER_NAME %in% player, ...)
  hex_grid <- nba:::half_court_hex(cellsize = cellsize)
  hex_grid_league <- hex_grid

  hex_grid <- nba:::merge_shot_data(hex_grid, dat_filt, hex = TRUE)
  if (variable == "FGPvLeague") {
    hex_grid_league <- nba:::merge_shot_data(hex_grid_league, dat_leagueomit, hex = TRUE)
    hex_grid@data$FGPvLeague <- hex_grid@data$FGP - hex_grid_league@data$FGP
  }

  indx <- which(variable == colnames(hex_grid@data))

  if (variable == "FGPvLeague") {
    # breaks <- seq(min(hex_grid@data[,indx], na.rm = TRUE), max(hex_grid@data[,indx], na.rm = TRUE), length.out = col_breaks)
    breaks <- seq(-1, 1, length.out = col_breaks)
  }
  else {
    # breaks <- seq(0, max(hex_grid@data[,indx], na.rm = TRUE), length.out = col_breaks)
    breaks <- seq(0, 1, length.out = col_breaks)
  }

  colorful <- c("#236e96", "#15b2d3", "#5abccc", "#ffbe42", "#ff7f00", "#f4543b", "#f4143b")
  # colorful_sel <- colorRampPalette(colorful)(col_breaks)
  colcode <- colorful[findInterval(hex_grid@data[,indx], breaks)]

  # scale (if applicable) and plot
  if (scale) {
    resized_hexes <- nba:::resize_hexes(hex_grid)
    plot(resized_hexes, col = colcode, border = NA, add = TRUE)
  }
  else {
    plot(hex_grid, col = colcode, border = NA, add = TRUE)
  }

  # include legend
  nba:::hex_legend(variable = variable, colorful = colorful, scale = scale)
  # title
  text(-300, 475, player, pos = 4, cex = 1.5)
  # subtitle
  if (variable == "FGP")
    text(-300, 450, "Shot Frequency and Success Rate", pos = 4, cex = 0.8)
  else
    text(-300, 450, "Shot Frequency and Performance Against Average", pos = 4, cex = 0.8)
}

pdf("tmp.pdf", width = 8, height = 8)
par(bg = "#f5f5f2")
draw_halfcourt()
plot_hexes(shots_1617, player = "Stephen Curry", variable = "FGPvLeague",
           scale = T, cellsize = 15,
           LOC_Y <= 500)
dev.off()

