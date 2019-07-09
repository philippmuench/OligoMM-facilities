### compare intra and inter distances


# calculate mean/sd of BC distances of random pairs between facilities and within one facility
getWithinBetweenStatistics <- function(dist, types) {
  datalist <- list()
  for (type in as.character(unique(types))) {
    print(type)
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
  source("utils.R")
  p <-
    ggplot(df, aes(x = mean_within, y = mean_between, color = facility))
  p <- p + geom_point()  + xlim(c(0, .3)) + ylim(c(0, .3))
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

# table 1
dat <-
  read.table(
    "data/plot1.csv",
    sep = ";",
    header = T,
    stringsAsFactors = F
  )
types <- dat$facility
dat[, c(1:2)] <- NULL
stability_table <-
  getWithinBetweenStatistics(as.matrix(vegan::vegdist(dat, method = 'bray')), types)
p <- plotComparisonScatter(stability_table, col)
p
pdf(file = "stability_table1.pdf",
    width = 4,
    height = 3)
print(p)
dev.off()
write.table(
  stability_table,
  file = "distances_table1.tsv",
  sep = "\t",
  col.names = T,
  row.names = F,
  quote = F
)

# table 2
dat <-
  read.table(
    "data/plot2.csv",
    sep = ";",
    header = T,
    stringsAsFactors = F
  )
types <- dat$facility
dat[, c(1:2)] <- NULL
stability_table <-
  getWithinBetweenStatistics(as.matrix(vegan::vegdist(dat, method = 'bray')), types)

within <- data.frame(type = "within", values = stability_table$mean_within)
between <- data.frame(type = "between", values = stability_table$mean_between)
test_table <- rbind(within, between)

require(onewaytests)
welch.test(values ~ type, data = test_table)


library(devtools)
require(BayesianFirstAid)

bayes.t.test(stability_table$mean_within, stability_table$mean_between)

p <- plotComparisonScatter(stability_table, col)
p
pdf(file = "stability_table2.pdf",
    width = 4,
    height = 3)
print(p)
dev.off()
write.table(
  stability_table,
  file = "distances_table2.tsv",
  sep = "\t",
  col.names = T,
  row.names = F,
  quote = F
)


# table 3
dat <-
  read.table(
    "data/plot3.csv",
    sep = ";",
    header = T,
    stringsAsFactors = F
  )
types <- dat$facility
dat[, c(1:2)] <- NULL
stability_table <-
  getWithinBetweenStatistics(as.matrix(vegan::vegdist(dat, method = 'bray')), types)
p <- plotComparisonScatter(stability_table, col)
p
pdf(file = "stability_table3.pdf",
    width = 4,
    height = 3)
print(p)
dev.off()
write.table(
  stability_table,
  file = "distances_table3.tsv",
  sep = "\t",
  col.names = T,
  row.names = F,
  quote = F
)
