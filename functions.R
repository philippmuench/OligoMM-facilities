# calculate mean/sd of BC distances of random pairs between facilities and within one facility
getWithinBetweenStatistics <- function(dist, types) {
  datalist <- list()
  for (type in as.character(unique(types))) {
    dist_subset_within <-
      dist[which(types %in% type), which(types %in% type)]
    "%ni%" <- Negate("%in%")
    dist_subset_between <-
      dist[which(types %in% type),  which(types %ni% type)]
    datalist[[length(datalist) + 1]] <- data.frame(
      facility = type,
      mean_within = mean(dist_subset_within[upper.tri(dist_subset_within)]),
      sd_within = sd(dist_subset_within[upper.tri(dist_subset_within)]),
      mean_between = mean(dist_subset_between),
      sd_between = sd(dist_subset_between)
    )
  }
  return(do.call(rbind, datalist))
}

plotComparisonScatter <- function(df, col) {
  require(ggplot2)
  p <-
    ggplot(df, aes(x = mean_within, y = mean_between, color = facility))
  p <- p + geom_point()  + xlim(c(0, .5)) + ylim(c(0, .5))
  p <-
    p + geom_errorbarh(aes(
      xmax = mean_within + sd_within,
      xmin = mean_within - sd_within,
      height = 0
    ))
  p <-
    p + geom_errorbar(aes(
      ymin = mean_between + sd_between,
      ymax = mean_between - sd_between,
      width = 0
    ))
  p <-
    p +  theme_pmuench() + theme(aspect.ratio = 1) + geom_abline(slope = 1,
                                                                 intercept = 0,
                                                                 linetype = 2)
  p <-
    p + xlab("mean Bray Curtis dissimilarity within facility") + ylab("mean Bray Curtis dissimilarity between facilities")
  p <- p + scale_color_manual(values = col)
  return(p)
}

col <- c(
  "1-1" = "#e41a1c",
  "1-2" = "#377eb8",
  "2" = "#4daf4a",
  "3" = "#984ea3",
  "4-1" =  "#ff7f00",
  "4-2" = "#ffff33",
  "4-3" = "#a65628",
  "4-4" = "#f781bf",
  "5" = "#999999"
)

doTest <- function(stability_table){
  within <- data.frame(type = "within", values = stability_table$mean_within)
  between <- data.frame(type = "between", values = stability_table$mean_between)
  test_table <- rbind(within, between)
  print(paste("mean between", round(mean(between$values), digits = 2)))
  print(paste("mean within", round(mean(within$values), digits = 2)))
  print(paste("sd between", round(sd(between$values), digits = 2)))
  print(paste("sd within", round(sd(within$values), digits = 2)))
  
  welch.test(values ~ type, data = test_table) # welch test maybe not appropriate, since the data are paired
  print("shapiro test")
  print(shapiro.test(within$values - between$values))
  print("t-test paired")
  print(t.test(stability_table$mean_within, stability_table$mean_between, paired = T))
  print("bayes, paired")
  print(bayes.t.test(stability_table$mean_within, stability_table$mean_between,
               paired = T))
  print("bayes, unpaired")
  print(bayes.t.test(stability_table$mean_within, stability_table$mean_between,
               paired = F))
}
