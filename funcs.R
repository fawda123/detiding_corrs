anoms.fun <- function(nem.in, pgvar = 'Pg', rtvar = 'Rt') {
  
  Pg <- nem.in[, pgvar]
  Pg <- sum(Pg <= 0,na.rm = T)/length(na.omit(Pg))
  Rt <- nem.in[, rtvar]
  Rt <- sum(Rt >= 0,na.rm = T)/length(na.omit(Rt))
  
  out <- data.frame(Pg,Rt)
  names(out) <- c(pgvar, rtvar)
  
  return(out)
  
  }

met_sums <- function(tab_in){

  x <- tab_in
  
  # detided
  anoms <- anoms.fun(x, pgvar = 'Pg_dtd', rtvar = 'Rt_dtd')
  anoms <- data.frame(variable = 'Anom', anoms)
  anoms <- melt(anoms, id.var = 'variable', variable.name = 'X1')[, c(2, 1, 3)]
  subs <- na.omit(x[, c('Pg_dtd', 'Rt_dtd', 'NEM_dtd')])  
  sums <- adply(t(subs),
    1, 
    function(y) data.frame(Avg = mean(y), se = sd(y)/sqrt(length(y)))
    )
  sums <- melt(sums, id.var = 'X1')
  dtd <- rbind(anoms, sums)
  dtd$X1 <- gsub('_dtd', '', dtd$X1)
  dtd$Input <- 'Detided'
  
  # observed
  anoms <- anoms.fun(x, pgvar = 'Pg', rtvar = 'Rt')
  anoms <- data.frame(variable = 'Anom', anoms)
  anoms <- melt(anoms, id.var = 'variable', variable.name = 'X1')[, c(2, 1, 3)]
  subs <- na.omit(x[, c('Pg', 'Rt', 'NEM')])  
  sums <- adply(t(subs),
    1, 
    function(y) data.frame(Avg = mean(y), se = sd(y)/sqrt(length(y)))
    )
  sums <- melt(sums, id.var = 'X1')
  obs <- rbind(anoms, sums)
  obs$Input <- 'Observed'  
  
  # output
  out <- rbind(dtd, obs)
  names(out) <- c('Metab', 'Metric', 'Value', 'Input')
  
  
  #  1mmolO2 = 32 mg O2, 1000mg = 1g, multiply by 32/1000
  sel_vec <- !out$Metric %in% 'Anom'
  out$Value[sel_vec] <- 0.032 * out$Value[sel_vec]
  
  # make columns as factors for correct row, column order w/ dcast
  out$Input <- factor(out$Input, levels = c('Observed', 'Detided'), labels = c('Observed', 'Filtered'))
  out$Metab <- factor(out$Metab, levels = c('Pg', 'Rt', 'NEM'))
  out$Metric <- factor(out$Metric, levels = c('Avg', 'se', 'Anom'))
  
  # dcast long to wide
  out <- dcast(out, Input ~ Metab + Metric, value.var = 'Value')
  
  return(out)
  
}
