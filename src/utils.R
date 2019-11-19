

plotTopVars<- function(x, df, reorderCOlumns, dataLimit=15, alternativeTitle = ''){
  
  numCats= length(unique(df[[x]]))
  
  dataLimit <- ifelse(dataLimit < numCats, dataLimit, numCats)
  
  countDataset = df[, .N, by=x][order(-N)][1:dataLimit]
  
  yLim = max(countDataset$N)*1.3
  
  
  if(reorderCOlumns){
    topPlot <- countDataset%>% ggplot(aes(x=reorder(countDataset[[x]],-N), y=N))
  }else {
    topPlot <- countDataset%>% ggplot(aes(x=countDataset[[x]], y=N))
  }
  

  topPlot <-topPlot + geom_bar(aes(fill = -N),stat = 'identity') +
    theme_gray() +
    labs(x='', y='Quantidade')+
    ggtitle(ifelse(alternativeTitle=='',paste("Top 15 ",x),alternativeTitle)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
    geom_label(aes(label = paste((round(N / sum(N), 3))*100,'%'),alpha = 0.8) ,colour = "black", fontface = "bold",
               size = 2, fill ='white', vjust = 'bottom', label.size = 0)+
    ylim(0, yLim)
  
  return(topPlot)
}