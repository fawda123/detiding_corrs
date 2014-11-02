# packages to use
library(reshape2) 
library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(RColorBrewer)

load('case_grds.RData')
load('cor_res.RData')
load('met_ls.RData')

# set ggplot theme
theme_set(theme_bw())
# custom theme, mod of theme_bw
my_theme <- theme(
  legend.title = element_blank(),legend.position = 'top',
  axis.title.x = element_blank(),legend.box= 'horizontal',
  plot.margin= unit(c(0, 1, 0, 1), "lines"), 
  text = element_text(size = 16)
)

# Define server logic required to generate and plot data
shinyServer(function(input, output) {

  # plot 1
  output$corrplot <- renderPlot({
    
    # input from ui
    site <- input$site
    months <- input$months
    day <- input$day
    hour <- input$hour
    tide <- input$tide
    
#     browser()
    
    ind <- cor_res$dec_time == as.numeric(day) & cor_res$hour == as.numeric(hour) & cor_res$Tide == as.numeric(tide)
    to_plo <- cor_res[ind & grepl(site, cor_res$L1), ]
    
    # reassign factor labels
    to_plo$month <- factor(to_plo$month, levels = c('01', '02', '03', '04',
      '05', '06', '07', '08', '09', '10', '11', '12'), 
      labels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11',' 12')
      )
    to_plo$var <- factor(to_plo$var, levels = c('Pg', 'Rt', 'NEM', 'do'), 
      labels = c('Pg', 'Rt', 'NEM', 'DO'))
    to_plo$sub_var <- factor(to_plo$sub_var, levels = c('dtd', 'obs'), 
      labels = c('Filtered', 'Observed'))
    
    # sub by months
    inds <- as.numeric(as.character(to_plo$month)) >= months[1] & as.numeric(as.character(to_plo$month)) <= months[2]
    to_plo <- to_plo[inds, ]
    
    p <- ggplot(to_plo, aes(x = factor(month), y = value, group = sub_var, colour = sub_var)) + 
      geom_line() +
      geom_point() + 
      geom_hline(yintercept = 0, linetype = 'dashed') + 
      facet_grid( ~ var) + 
      scale_y_continuous(limits = c(-1, 1)) +
      ylab('Correlation with tide') +
      xlab('Month') +
      theme(text = element_text(size=18), 
        legend.position = 'top',
        legend.direction = 'horizontal',
        legend.title = element_blank()
        )
    
    print(p)
    
    },height = 300, width = 900)

  ##
  # plot 2
  output$metabplot <- renderPlot({
    
    # input from ui
    site <- input$site
    months <- input$months
    day <- input$day
    hour <- input$hour
    tide <- input$tide
    
#     browser()
    
    # files to get from input
    ind <- case_grds$dec_time == as.numeric(day) & case_grds$hour == as.numeric(hour) & case_grds$Tide == as.numeric(tide)
    case_get <- rownames(case_grds[ind, ])
    files <- list.files('wtreg/', paste0(site, '_wtreg_', case_get, '\\.RData'))
    
    ##
    # metab data
    met_sub <- met_ls[names(met_ls) %in% files]
  
    # melt for plotting
    to_plo1 <- melt(met_sub, id.var = c('Date'), 
      measure.var = grep('Pg|Rt|NEM', names(met_sub[[1]]), value = T)
    )
    to_plo1$Input <- 'Observed'
    to_plo1$Input[grep('dtd', to_plo1$variable)] <- 'Detided'
    to_plo1$Input <- factor(to_plo1$Input, levels = c('Observed', 'Detided'))
    to_plo1$variable <- gsub('_dtd', '', to_plo1$variable)
    to_plo1$month <- as.numeric(strftime(to_plo1$Date, '%m'))
    
    # month subs
    inds <- to_plo1$month >= months[1] & to_plo1$month <= months[2]
    to_plo1 <- to_plo1[inds, ]
    
    ##
    # actual data
    files_ls <- vector('list', length = length(files))
    nms <- gsub('\\.RData$', '', files)
    names(files_ls) <- nms
    for(i in 1:length(files)){
      load(paste0('wtreg/', files[i]))
      files_ls[[nms[i]]] <- get(nms[i])
      rm(list = nms[i])
    }
    
    # melt for plotting
    to_plo2 <- melt(files_ls, id.var = c('DateTimeStamp','Tide', 'TotPAR'), 
      measure.var = grep('DO_obs|DO_nrm', names(files_ls[[1]]), value = T)
    )
    to_plo2$Input <- 'Observed'
    to_plo2$Input[grep('nrm$', to_plo2$variable)] <- 'Detided'
    to_plo2$Input <- factor(to_plo2$Input, levels = c('Observed', 'Detided'))
    to_plo2$variable <- gsub('_dtd', '', to_plo2$variable)
    tzone <- attr(to_plo2$DateTimeStamp, 'tzone')
    to_plo2$month <- as.numeric(strftime(to_plo2$DateTimeStamp, '%m', tz = tzone))
    
    # month subs
    inds <- to_plo2$month >= months[1] & to_plo2$month <= months[2]
    to_plo2 <- to_plo2[inds, ]
    
    # function for setting range on y axis
    rng.fun<-function(vec.in){
      rngs<-range(vec.in,na.rm=T)
      buffs<-0.07*abs(diff(rngs))
      c(rngs[1]-buffs,rngs[2]+buffs)
    }

    ##
    # metab plot

    ylab<-expression(paste('g ',O [2], ' ', m^-2, d^-1))
    p1 <- ggplot(to_plo1, 
      aes(x = Date, y = 0.032 * value, group = variable,
            colour = variable)) +
      geom_line() +
      geom_point(size = 2) +
      facet_wrap(~Input, ncol = 1) +
      scale_y_continuous(ylab)  +
      my_theme
    
    ##
    # DO plot
    to_plo_obs <- to_plo2
      
    ylab<-expression(paste('DO (mg ',L^-1,')'))
    p2 <- ggplot(to_plo_obs, aes(x = DateTimeStamp)) + 
      geom_line(aes(y = value, colour = Input)) +
      scale_y_continuous(ylab)  +
      my_theme
    
    ##
    # DO plot
    to_plo3 <- to_plo_obs
    
    ylab<-expression(paste('Tide (m)'))
    p3 <- ggplot(to_plo3, aes(x = DateTimeStamp)) + 
      geom_line(aes(y = Tide, colour = TotPAR), size = 1.1) +
      scale_colour_gradientn(name = "Total PAR", 
        colours = rev(brewer.pal(7, 'Spectral'))) +
      scale_y_continuous(ylab)  +
      theme(
        legend.position = 'top', legend.text = element_text(size = 8),
        axis.title.x = element_blank(), legend.box= 'horizontal',
        plot.margin= unit(c(0, 1, 0, 1), "lines"), 
        text = element_text(size = 16)
        )
     
    # get widths
    pA <- ggplot_gtable(ggplot_build(p1))
    pB <- ggplot_gtable(ggplot_build(p2))
    pC <- ggplot_gtable(ggplot_build(p3))
    maxWidth = unit.pmax(pA$widths[2:3], pB$widths[2:3], 
                         pC$widths[2:3])
    
    # Set the widths
    pA$widths[2:3] <- maxWidth
    pB$widths[2:3] <- maxWidth
    pC$widths[2:3] <- maxWidth
    
    out <- arrangeGrob(pA, pB, pC, heights = c(3, 2, 2))
    
    print(out)
    
    },height = 600, width = 900)
  
    })