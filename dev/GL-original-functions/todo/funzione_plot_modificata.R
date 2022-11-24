plot_DTR_gl<-function (x, confidence.interval = FALSE, xlab = "Time", ylab = "Survival probability", 
          line.color = c("black", "grey20", "grey40", "grey60", "grey80", "grey90"), legend.position = "right", 
          censored = FALSE, ...) 
{
  if (confidence.interval != FALSE & confidence.interval != 
      TRUE & confidence.interval != T & confidence.interval != 
      F) 
    stop("confidence.interval input can not be recognized")
  if (is.null(xlab)) 
    stop("xlab can not be empty")
  if (is.character(xlab) == FALSE) 
    stop("xlab has to be character")
  if (is.null(ylab)) 
    stop("ylab can not be empty")
  if (is.character(ylab) == FALSE) 
    stop("ylab has to be character")
  if (is.null(line.color)) 
    stop("line.color can not be empty")
  if (is.null(legend.position)) 
    stop("legend.position can not be empty")
  if (is.null(censored)) 
    stop("censored can not be empty")
  if (max(x$censortime) > max(x$time)) {
    group <- c(rep("A1B1", length(x$time)), 
               rep("A1B2", length(x$time)), 
               rep("A2B1", length(x$time)), 
               rep("A2B2", length(x$time)),
               rep("A3B1", length(x$time)), 
               rep("A3B2", length(x$time)),
               "A1B1", "A1B2", "A2B1", "A2B2", "A3B1", "A3B2")
    time <- c(rep(x$time, 6), rep(max(x$censortime), 6))
    surv <- c(x$SURV11, x$SURV12, x$SURV21, x$SURV22, x$SURV31, x$SURV32,
              min(x$SURV11), min(x$SURV12), min(x$SURV21), min(x$SURV22), min(x$SURV31), min(x$SURV32))
    se <- c(x$SE11, x$SE12, x$SE21, x$SE22,  x$SE31,  x$SE32, 
            x$SE11[length(x$SE11)], 
            x$SE12[length(x$SE12)], 
            x$SE21[length(x$SE21)], 
            x$SE22[length(x$SE22)],
            x$SE31[length(x$SE31)],
            x$SE32[length(x$SE32)]
            )
  }
  else {
    group <- c(rep("A1B1", length(x$time)), 
               rep("A1B2", length(x$time)), 
               rep("A2B1", length(x$time)), 
               rep("A2B2", length(x$time)),
               rep("A3B1", length(x$time)),
               rep("A3B2", length(x$time))
               )
    time <- rep(x$time, 6)
    surv <- c(x$SURV11, x$SURV12, x$SURV21, x$SURV22, x$SURV31, x$SURV32)
    se <- c(x$SE11, x$SE12, x$SE21, x$SE22, x$SE31, x$SE32)
  }
  plot.result <- data.frame(group, time, surv, se)
  xpool <- c(0.05, 0.1, seq(0.2, 2, 0.2), seq(0.5, 10, 0.5), 
             seq(10, 50, 5), seq(50, 100, 10), seq(100, 1500, 50))
  xdiff <- (xpool - max(x$time)/10)[(xpool - max(x$time)/10) >= 
                                      0]
  x.scale <- max(x$time)/10 + unique(xdiff[which(xdiff == 
                                                   min(xdiff))])
  g <- ggplot(data = plot.result) + geom_step(aes(x = time, 
                                                  y = surv, color = group), size = 1.2) + scale_color_manual(values = line.color) + 
    scale_x_continuous(breaks = seq(0, x.scale * 10, x.scale)) + 
    xlab(xlab) + ylab(ylab) + theme_bw() + theme(title = element_text(size = 15), 
                                                 axis.text = element_text(size = 15), legend.position = legend.position, 
                                                 legend.title = element_blank(), legend.text = element_text(size = 15), 
                                                 legend.key.size = unit(1, "cm"))
  if (censored == TRUE) {
    censorgroup <- x$censorDTR
    censortime <- x$censortime
    censorsurv <- x$censorsurv
    plot.censor <- data.frame(censorgroup, censortime, censorsurv)
    g <- g + geom_point(data = plot.censor, aes(x = censortime, 
                                                y = censorsurv, col = censorgroup), shape = "|", 
                        size = 6, show.legend = FALSE)
  }
  if (confidence.interval == FALSE) {
    g <- g
  }
  else {
    g <- g + geom_ribbon(aes(x = time, ymin = surv - 1.96 * 
                               se, ymax = surv + 1.96 * se, fill = group), alpha = 0.1, 
                         stat = "stepribbon", direction = "hv") + scale_fill_manual(values = line.color) + 
      geom_step(aes(x = time, y = surv - 1.96 * se, color = group), 
                linetype = "dotted", size = 0.5) + geom_step(aes(x = time, 
                                                                 y = surv + 1.96 * se, color = group), linetype = "dotted", 
                                                             size = 0.5)
  }
  g
}
