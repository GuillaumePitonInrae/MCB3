#Adaptation of PLOT_INPUT from package HYRISK to plot directly in a png file
library("sets")

PLOT_INPUTnew<-function (input, N = 1000, mode = "IND") 
{
  choice_opt = "L-BFGS-B"
  param_opt = NULL
  corr = 0.01
  NL = 10
  ntot = length(input)
  ncol = ceiling(sqrt(ntot))
  nrow = ceiling(ntot/ncol)
  # dev.new()
  par(mfrow = c(nrow, ncol), cex = 1.1, mex = 1, mar = c(4.5,4.005, 2, 1),bty="n")
  ndiscrprob = 1000
  for (i in 1:length(input)) {
    inp = input[[i]]
    if (inp$type == "possi") {
      if (inp$distr == "interval") {
        plot(c(inp$param[1], inp$param[1], inp$param[2], 
               inp$param[2]), c(0, 1, 1, 0), type = "l", 
             lwd = 2, col = "grey25", xlab = inp$name, ylab = expression(Pi)
             #,bty = "o"
             ,xlim=c(0,max(inp$param)),#bty = "L",
             main = "Possibility")
      }
      else {
        plot(tuple(inp$fuzzy), type = "l", lwd = 2, 
             col = "grey25", xlab = inp$name, ylab = expression(Pi),#bty = "o", 
             xlim=c(0,max(inp$param)),#bty = "L",
             main = "Possibility")
      }
    }
    else if (inp$type == "proba") {
      x <- do.call(inp$rfun, as.list(c(ndiscrprob, inp$param)))
      plot(ecdf(x), lwd = 2, col = "grey25", xlab = inp$name,  xlim=c(0,max(inp$param)),
           ylab = "CDF", main = inp$type, col.01line = NULL)
    }
    else if (inp$type == "fixed") {
      plot(c(inp$param, inp$param), c(0, 1), ty = "l", 
           col = "grey25", cex = 1, lwd = 2, yaxt = "n", 
           #bty = "L",
           xlim=c(0,10^(ceiling(log10(max(inp$param))))),
           xlab = inp$name, ylab = "", main = "Fixed")
    }
    else if (inp$type == "impr proba") {
      if (mode == "IND") {
        d = length(input)
        rr <- matrix(runif(N * d, 0, 1 - corr), ncol = d)
      }
      if (mode == "DEP") {
        d = length(input)
        rr = matrix(, nrow = N * NL, ncol = d)
        l = rep(seq(0, 1 - corr, length = NL), each = N)
        allpossi = 0
        for (i in 1:d) {
          if (input[[i]]$type == "possi") {
            rr[, i] = l
            allpossi = allpossi + 1
          }
          else if (input[[i]]$type == "proba") {
            rr[, i] = runif(N * NL)
          }
        }
        if (allpossi == d) {
          rr = matrix(rep(seq(0, 1 - corr, length = N * 
                                NL), d), ncol = d)
        }
      }
      subtype <- NULL
      for (j in input[inp$param]) {
        subtype = c(subtype, j$type)
      }
      if (("possi" %in% subtype) & (!("proba" %in% subtype)) & 
          (!("impr proba" %in% subtype))) {
        Xr <- NULL
        Xr <- apply(rr, 1, PLOT_INPUT_fun, i, input, 
                    choice_opt, param_opt)
        plot(ecdf(Xr[1, ]), xlim = c(min(knots(ecdf(Xr[1, 
                                                       ]))), max(knots(ecdf(Xr[2, ])))), col = "grey25", 
             cex = 1, lwd = 2, xlab = inp$name, ylab = "CDF", 
             main = inp$type)
        lines(ecdf(Xr[2, ]), col = "grey25", cex = 1, 
              lwd = 2)
      }
      else if (("proba" %in% subtype) & (!("possi" %in% 
                                           subtype)) & (!("impr proba" %in% subtype))) {
        Xr <- NULL
        Xr <- apply(rr, 1, PLOT_INPUT_fun2, i, N, input, 
                    choice_opt, param_opt)
        plot(ecdf(Xr[1, ]), xlim = c(min(knots(ecdf(Xr[1, 
                                                       ]))), max(knots(ecdf(Xr[2, ])))), col = "grey25", 
             cex = 1, lwd = 2, xlab = inp$name, ylab = "CDF", 
             main = inp$type)
        lines(ecdf(Xr[2, ]), col = "grey25", cex = 1, 
              lwd = 1.5)
      }
      else if (("fixed" %in% subtype) & (!("possi" %in% 
                                           subtype)) & (!("proba" %in% subtype)) & (!("impr proba" %in% 
                                                                                      subtype))) {
        Xr <- NULL
        Xr <- apply(rr, 1, PLOT_INPUT_fun, i, input, 
                    choice_opt, param_opt)
        plot(ecdf(Xr[1, ]), xlim = c(min(knots(ecdf(Xr[1, 
                                                       ]))), max(knots(ecdf(Xr[2, ])))), col = "grey25", 
             cex = 1, lwd = 2, xlab = inp$name, ylab = "CDF", 
             main = inp$type)
        lines(ecdf(Xr[2, ]), col = "grey25", cex = 1, 
              lwd = 2)
      }
      else if ("impr proba" %in% subtype) {
        stop("Error: imprecise probabilistic subvariable(s) of a variable")
      }
      else {
        stop("Error: possibilistic and probabilistic subvariables of the same variable")
      }
    }
  }
}
