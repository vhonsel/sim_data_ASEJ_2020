# Check and install dependencies
if(!require(sna)) install.packages("sna")
if(!require(igraph)) install.packages("igraph")
if(!require(reshape2)) install.packages("reshape2")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(caret)) install.packages("caret")
if(!require(jsonlite)) install.packages("jsonlite")

#load dependencies
library(sna)
library(igraph)
library(reshape2)
library(ggplot2)
library(caret)
library(jsonlite)


projects <- c("giraph","gora","directory-fotress-core","jspwiki","zookeeper","commons-collections","commons-io","commons-lang","nutch","deltaspike")

results <- data.frame(project=character(), mode=character(), year=character(), nodes=numeric(), edges=numeric(), modularity=numeric(), densitiy = numeric(), stringsAsFactors = FALSE)

results_commits <- data.frame(project=character(), mode=character(), year=character(), NOC=numeric(),stringsAsFactors = FALSE)
  
################################################################################
# DEFINE THE LOCATION OF THE REPLICATION KIT AND YOUR SAVING DIRECTORY HERE!!! #
# These are the only lines you have to change.                                 #
################################################################################


path <- "C:/Users/vhonsel/Documents/git/paper/ASE/ReplicationKit/SimSE_Data/"

savedir <- file.path("C:/Users/vhonsel/Documents/git/paper/ASE/ReplicationKit/plots")


########## using adjacency matrices ###########

for (j in 1:length(projects)) {
  
  setwd(path)
  setwd(paste(getwd(),"/",projects[j],sep = ""))
  
  
################# graph evolution ##################
  
  setwd("./empirical")
  
  noy <- length(list.files (pattern = "dot$"))
  
  years <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18")
  length(years) <- noy
  
  for (i in 1:length(years)) {
    
    dat = read.csv(paste("cc_",projects[j],"_",years[i],".csv", sep=""), header=FALSE)
    adj_matrix = as.matrix(dat)
    cc_graph <- graph_from_adjacency_matrix(adj_matrix,weighted=TRUE,mode="undirected")
    
    n <- length(V(cc_graph))
    e <- length(E(cc_graph))
    
    cl <- cluster_louvain(cc_graph,weights=E(cc_graph)$weight)
    mod <- modularity(cl)
    
    den <- edge_density(cc_graph, loops = FALSE)
    
    metrics <- list(project=projects[j], mode= "empirical", year = years[i], nodes=n, edges=e, modularity=mod, density = den)
    results <- rbind(results,metrics,stringsAsFactors=FALSE)
  
    ### here you can plot the change coupling graph
    # plot(cl, cc_graph, vertex.label=NA)
    
  }
  
  ################# simulated graph evolution ##################
  
  setwd("./..")
  setwd("./simulated/default")
  
  for (i in 1:length(years)) {
    
    edges = read.csv(paste("ChangeCoupling_Edges_", projects[j],"_",years[i],".csv", sep=""), header=TRUE)
    nodes = read.csv(paste("ChangeCoupling_Nodes_",projects[j],"_",years[i],".csv", sep=""), header=TRUE)
    cc_graph <- graph.data.frame(edges, directed=FALSE, vertices=nodes$Id)
    
    n <- length(V(cc_graph))
    e <- length(E(cc_graph))
    
    #deg <- igraph::degree(cc_graph, v= V(cc_graph), mode="all")
    
    cl <- cluster_louvain(cc_graph,weights=E(cc_graph)$weight)
    mod <- modularity(cl)
    
    Communities <- length(cl)
    
    den <- edge_density(cc_graph, loops = FALSE)
    
    metrics <- list(project=projects[j], mode= "project-specific simulation", year = years[i], nodes=n, edges=e, modularity=mod, density = den)
    results <- rbind(results,metrics,stringsAsFactors=FALSE)
    
  }
  
  setwd("./..")
  setwd("./average")
  
  
  for (i in 1:length(years)) {
    
    edges = read.csv(paste("ChangeCoupling_Edges_", projects[j],"_",years[i],".csv", sep=""), header=TRUE)
    nodes = read.csv(paste("ChangeCoupling_Nodes_",projects[j],"_",years[i],".csv", sep=""), header=TRUE)
    cc_graph <- graph.data.frame(edges, directed=FALSE, vertices=nodes$Id)
    
    n <- length(V(cc_graph))
    e <- length(E(cc_graph))
    
    cl <- cluster_louvain(cc_graph,weights=E(cc_graph)$weight)
    mod <- modularity(cl)
    
    Communities <- length(cl)
    
    den <- edge_density(cc_graph, loops = FALSE)
    
    metrics <- list(project=projects[j], mode= "average simulation", year = years[i], nodes=n, edges=e, modularity=mod, density = den)
    results <- rbind(results,metrics,stringsAsFactors=FALSE)
    
    
  }
  
  # commit bahavior including comparison with onion model
  
  setwd("./..")
  setwd("./..")
  setwd("./empirical")
  
  commits <- read.csv(paste("NoC_",projects[j],".csv",sep=""), header=TRUE)
  
  setwd("./..")
  setwd("./simulated/default")
  
  commits_sim <- read.delim(paste("sim_noc_",projects[j],".txt",sep=""), sep = ",", header=TRUE)
  commits_sim$year <- commits$year
  commits_sim$no <- NULL
  
  setwd("./..")
  setwd("./average")
  
  commits_sim_av <- read.delim(paste("sim_noc_",projects[j],".txt",sep=""), sep = ",", header=TRUE)
  commits_sim_av$year <- commits$year
  commits_sim_av$no <- NULL
  
  setwd("./..")
  setwd("./snapshot")
  
  commits_sim_ss <- read.delim(paste("sim_noc_",projects[j],".txt",sep=""), sep = ",", header=TRUE)
  commits_sim_ss$year <- commits$year
  commits_sim_ss$no <- NULL
  
  reshaped <- melt(commits, id="year")
  reshaped_sim <- melt(commits_sim, id="year")
  reshaped_sim_av <- melt(commits_sim_av, id="year")
  reshaped_sim_ss <- melt(commits_sim_ss, id="year")
  
  for (i in 1:nrow(reshaped)) {
    if (reshaped$variable[i] == "core" || reshaped$variable[i] == "peripheral") {
      reshaped$model[i] <- "onion model"
    } else {
      reshaped$model[i] <- "own model"
    }
  }
  
  for (i in 1:nrow(reshaped_sim)) {
    if (reshaped_sim$variable[i] == "core" || reshaped_sim$variable[i] == "peripheral") {
      reshaped_sim$model[i] <- "onion model"
    } else {
      reshaped_sim$model[i] <- "own model"
    }
  }
  
  for (i in 1:nrow(reshaped_sim_av)) {
    if (reshaped_sim_av$variable[i] == "core" || reshaped_sim_av$variable[i] == "peripheral") {
      reshaped_sim_av$model[i] <- "onion model"
    } else {
      reshaped_sim_av$model[i] <- "own model"
    }
  }
  
  for (i in 1:nrow(reshaped_sim_ss)) {
    if (reshaped_sim_ss$variable[i] == "core" || reshaped_sim_ss$variable[i] == "peripheral") {
      reshaped_sim_ss$model[i] <- "onion model"
    } else {
      reshaped_sim_ss$model[i] <- "own model"
    }
  }
  
  for (i in 1:nrow(reshaped)) {
    reshaped$commits[i] <- sum(reshaped$value[reshaped$year[i]== reshaped$year & reshaped$model[i]==reshaped$model]) 
  }
  for (i in 1:nrow(reshaped_sim)) {
    reshaped_sim$commits[i] <- sum(reshaped_sim$value[reshaped_sim$year[i]== reshaped_sim$year & reshaped_sim$model[i]==reshaped_sim$model]) 
  }
  for (i in 1:nrow(reshaped_sim_av)) {
    reshaped_sim_av$commits[i] <- sum(reshaped_sim_av$value[reshaped_sim_av$year[i]== reshaped_sim_av$year & reshaped_sim_av$model[i]==reshaped_sim_av$model]) 
  }
  
  for (i in 1:nrow(reshaped_sim_ss)) {
    reshaped_sim_ss$commits[i] <- sum(reshaped_sim_ss$value[reshaped_sim_ss$year[i]== reshaped_sim_ss$year & reshaped_sim_ss$model[i]==reshaped_sim_ss$model]) 
  }
  
  
  ###################### plots for the metric evolution #########################
  
  
  results_project <- results[results$project==projects[j],]
  
  nodes_plot <- ggplot(results_project, aes(year, nodes))
  np <- nodes_plot + geom_line(aes(group=mode,color=mode),size=1.5)+ylim(0,max(results_project$nodes)+100)+theme_set(theme_gray(base_size = 20))
  print(np)
  ggsave(np, file=paste(projects[j],"_nodes.png"), path = savedir)
  dev.off()
  
  edges_plot <- ggplot(results_project, aes(year, edges))
  ep <- edges_plot + geom_line(aes(group=mode,color=mode),size=1.5)+ylim(0,max(results_project$edges)+1000)+theme_set(theme_gray(base_size = 20))
  print(ep)
  ggsave(ep, file=paste(projects[j],"_edges.png"), path = savedir)
  dev.off()
  
  mod_plot <- ggplot(results_project, aes(year, modularity))
  mp <- mod_plot + geom_line(aes(group=mode,color=mode),size=1.5)+ylim(0,1)+theme_set(theme_gray(base_size = 20))
  print(mp)
  ggsave(mp, file=paste(projects[j],"_modularity.png"), path = savedir)
  dev.off()
  
  dens_plot <- ggplot(results_project, aes(year, density))
  denp <- dens_plot + geom_line(aes(group=mode,color=mode),size=1.5)+ylim(0,1)+theme_set(theme_gray(base_size = 20))
  print(denp)
  ggsave(denp, file=paste(projects[j],"_density.png"), path = savedir)
  dev.off()
  
  commits_plot <- ggplot(reshaped, aes(year, value,color=variable,linetype=model))
  cp <- commits_plot +
    geom_line(size=1.5)+
    scale_color_discrete(name="role")+
    scale_linetype_discrete(name="model")+
    theme_set(theme_gray(base_size = 20))
  print(cp)
  ggsave(cp, file=paste(projects[j],"_commits.png"), path = savedir)
  dev.off()
  
  commits_emp <- reshaped[c(1,5)]
  commits_emp <- unique(commits_emp)
  for (m in 1:nrow(commits_emp)){
    metrics <- list(project=projects[j], mode= "empirical", year = commits_emp$year[m], NOC=commits_emp$commits[m])
    results_commits <- rbind(results_commits,metrics,stringsAsFactors=FALSE)
  }
  commits_sim <- reshaped_sim[,c(1,5)]
  commits_sim <- commits_sim[commits_sim$commits!=0,]
  commits_sim <- unique(commits_sim)
  for (m in 1:nrow(commits_sim)){
    metrics <- list(project=projects[j], mode= "project-specific simulation", year = commits_sim$year[m], NOC=commits_sim$commits[m])
    results_commits <- rbind(results_commits,metrics,stringsAsFactors=FALSE)
  }
  commits_sim_av <- reshaped_sim_av[,c(1,5)]
  commits_sim_av <- commits_sim_av[commits_sim_av$commits!=0,]
  commits_sim_av <- unique(commits_sim_av)
  for (m in 1:nrow(commits_sim_av)){
    metrics <- list(project=projects[j], mode= "average simulation", year = commits_sim_av$year[m], NOC=commits_sim_av$commits[m])
    results_commits <- rbind(results_commits,metrics,stringsAsFactors=FALSE)
  }
  commits_sim_ss <- reshaped_sim_ss[,c(1,5)]
  commits_sim_ss <- commits_sim_ss[commits_sim_ss$commits!=0,]
  commits_sim_ss <- unique(commits_sim_ss)
  for (m in 1:nrow(commits_sim_ss)){
    metrics <- list(project=projects[j], mode= "snapshot simulation", year = commits_sim_ss$year[m], NOC=commits_sim_ss$commits[m])
    results_commits <- rbind(results_commits,metrics,stringsAsFactors=FALSE)
  }
  
}

### calculate error for different simulation modes 

error <- data.frame(project=character(), mode=character(), metric=character(), MAE=numeric(), RMSE=numeric(), stringsAsFactors = FALSE) 

for (i in 1:length(projects)){
  
  results_p <- results[results$project==projects[i],]
  results_p_NOC <- results_commits[results_commits$project==projects[i],]
  
  #modularity
  modularity <- results_p[,c(2,3,6)]
  empirical<-modularity$modularity[modularity$mode=="empirical"]
  predicted<-modularity$modularity[modularity$mode=="project-specific simulation"]  
  predicted2<-modularity$modularity[modularity$mode=="average simulation"]
  
  MAE=MAE(predicted,empirical)
  RMSE=RMSE(predicted, empirical)
  
  sim_error <- list(project=projects[i], mode="project-specific", metric="modularity",MAE=MAE,RMSE=RMSE)
  error <- rbind(error,sim_error,stringsAsFactors=FALSE)
   
  MAE=MAE(predicted2,empirical)
  RMSE=RMSE(predicted2, empirical)
  
  sim_error <- list(project=projects[i], mode="average",metric="modularity",MAE=MAE,RMSE=RMSE)
  error <- rbind(error,sim_error,stringsAsFactors=FALSE)
  
  #density
  density <- results_p[,c(2,3,7)]
  empirical<-density$density[density$mode=="empirical"]
  predicted<-density$density[density$mode=="project-specific simulation"]  
  predicted2<-density$density[density$mode=="average simulation"]  
  
  MAE=MAE(predicted,empirical)
  RMSE=RMSE(predicted, empirical)
  
  sim_error <- list(project=projects[i],mode="project-specific",metric="density",MAE=MAE,RMSE=RMSE)
  error <- rbind(error,sim_error,stringsAsFactors=FALSE)
  
  MAE=MAE(predicted2,empirical)
  RMSE=RMSE(predicted2, empirical)

  sim_error <- list(project=projects[i], mode="average",metric="density",MAE=MAE,RMSE=RMSE)
  error <- rbind(error,sim_error,stringsAsFactors=FALSE)
  
  
  #NOC
  
  empirical<-results_p_NOC$NOC[results_p_NOC$mode=="empirical"]
  predicted<-results_p_NOC$NOC[results_p_NOC$mode=="project-specific simulation"]  
  predicted2<-results_p_NOC$NOC[results_p_NOC$mode=="average simulation"]  

  MAE=MAE(predicted,empirical)
  RMSE=RMSE(predicted, empirical)

  sim_error <- list(project=projects[i],mode="project-specific",metric="NOC",MAE=MAE,RMSE=RMSE)
  error <- rbind(error,sim_error,stringsAsFactors=FALSE)
  
  MAE=MAE(predicted2,empirical)
  RMSE=RMSE(predicted2, empirical)
  
  sim_error <- list(project=projects[i], mode="average",metric="NOC",MAE=MAE,RMSE=RMSE)
  error <- rbind(error,sim_error,stringsAsFactors=FALSE)
  
  
  #NOF
  nodes <- results_p[,c(2,3,4)]
  empirical<-nodes$nodes[nodes$mode=="empirical"]
  predicted<-nodes$nodes[nodes$mode=="project-specific simulation"]  
  predicted2<-nodes$nodes[nodes$mode=="average simulation"]
  
  MAE=MAE(predicted,empirical)
  RMSE=RMSE(predicted, empirical)
  
  sim_error <- list(project=projects[i], mode="project-specific", metric="NOF",MAE=MAE,RMSE=RMSE)
  error <- rbind(error,sim_error,stringsAsFactors=FALSE)
  
  MAE=MAE(predicted2,empirical)
  RMSE=RMSE(predicted2, empirical)
  
  sim_error <- list(project=projects[i], mode="average",metric="NOF",MAE=MAE,RMSE=RMSE)
  error <- rbind(error,sim_error,stringsAsFactors=FALSE)
  
}

### error boxplot

error <- na.omit(error)

#MAE

p<-ggplot(error[error$metric=="density",], aes(x=metric, y=MAE, fill=mode))+geom_boxplot()
print(p)

p<-ggplot(error[error$metric=="modularity",], aes(x=metric, y=MAE, fill=mode))+geom_boxplot()
print(p)

p<-ggplot(error[error$metric=="NOC",], aes(x=metric, y=MAE, fill=mode))+geom_boxplot()
print(p)

p<-ggplot(error[error$metric=="NOF",], aes(x=metric, y=MAE, fill=mode))+geom_boxplot()
print(p)


#RMSE

p<-ggplot(error[error$metric=="density",], aes(x=metric, y=RMSE, fill=mode))+geom_boxplot()
print(p)

p<-ggplot(error[error$metric=="modularity",], aes(x=metric, y=RMSE, fill=mode))+geom_boxplot()
print(p)

p<-ggplot(error[error$metric=="NOC",], aes(x=metric, y=RMSE, fill=mode))+geom_boxplot()
print(p)

p<-ggplot(error[error$metric=="NOF",], aes(x=metric, y=RMSE, fill=mode))+geom_boxplot()
print(p)


#### test normality

# shapiro wilk 

# bonferroni correction with n=32
# alpha = 0.00156

# MAE 

shapiro.test(error$MAE[error$metric=="density"&error$mode=="average"])
shapiro.test(error$MAE[error$metric=="density"&error$mode=="project-specific"])

wilcox.test(error$MAE[error$metric=="density"&error$mode=="average"], 
            error$MAE[error$metric=="density"&error$mode=="project-specific"][-10], paired = TRUE)

shapiro.test(error$MAE[error$metric=="modularity"&error$mode=="average"])
shapiro.test(error$MAE[error$metric=="modularity"&error$mode=="project-specific"])

wilcox.test(error$MAE[error$metric=="modularity"&error$mode=="average"], 
            error$MAE[error$metric=="modularity"&error$mode=="project-specific"], paired = TRUE)

shapiro.test(error$MAE[error$metric=="NOC"&error$mode=="average"])
shapiro.test(error$MAE[error$metric=="NOC"&error$mode=="project-specific"])

wilcox.test(error$MAE[error$metric=="NOC"&error$mode=="average"], 
            error$MAE[error$metric=="NOC"&error$mode=="project-specific"], paired = TRUE)

shapiro.test(error$MAE[error$metric=="NOF"&error$mode=="average"])
shapiro.test(error$MAE[error$metric=="NOF"&error$mode=="project-specific"])

wilcox.test(error$MAE[error$metric=="NOF"&error$mode=="average"], 
            error$MAE[error$metric=="NOF"&error$mode=="project-specific"], paired = TRUE)

# RMSE 

shapiro.test(error$RMSE[error$metric=="density"&error$mode=="average"])
shapiro.test(error$RMSE[error$metric=="density"&error$mode=="project-specific"])

wilcox.test(error$RMSE[error$metric=="density"&error$mode=="average"], 
            error$RMSE[error$metric=="density"&error$mode=="project-specific"][-10], paired = TRUE)

shapiro.test(error$RMSE[error$metric=="modularity"&error$mode=="average"])
shapiro.test(error$RMSE[error$metric=="modularity"&error$mode=="project-specific"])

wilcox.test(error$RMSE[error$metric=="modularity"&error$mode=="average"], 
            error$RMSE[error$metric=="modularity"&error$mode=="project-specific"], paired = TRUE)

shapiro.test(error$RMSE[error$metric=="NOC"&error$mode=="average"])
shapiro.test(error$RMSE[error$metric=="NOC"&error$mode=="project-specific"])

wilcox.test(error$RMSE[error$metric=="NOC"&error$mode=="average"], 
            error$RMSE[error$metric=="NOC"&error$mode=="project-specific"], paired = TRUE)

shapiro.test(error$RMSE[error$metric=="NOF"&error$mode=="average"])
shapiro.test(error$RMSE[error$metric=="NOF"&error$mode=="project-specific"])

wilcox.test(error$RMSE[error$metric=="NOF"&error$mode=="average"], 
            error$RMSE[error$metric=="NOF"&error$mode=="project-specific"], paired = TRUE)



### long-term vs. short-term prediction


################# graph evolution ##################
  
 
  for (j in 1:length(projects)) {
    
  setwd(path)
  setwd(paste(getwd(),"/",projects[j],sep = ""))
  setwd("./simulated/snapshot")
  
  noy <- length(list.files (pattern = "dot$"))
  
  years <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18")
  length(years) <- noy
  
  for (i in 1:length(years)) {
    
    edges = read.csv(paste("ChangeCoupling_Edges_", projects[j],"_",years[i],".csv", sep=""), header=TRUE)
    nodes = read.csv(paste("ChangeCoupling_Nodes_",projects[j],"_",years[i],".csv", sep=""), header=TRUE)
    cc_graph <- graph.data.frame(edges, directed=FALSE, vertices=nodes$Id)
    
    n <- length(V(cc_graph))
    e <- length(E(cc_graph))
    
    den <- edge_density(cc_graph, loops = FALSE)
    
    cl <- cluster_louvain(cc_graph,weights=E(cc_graph)$weight)
    mod <- modularity(cl)
    
    metrics <- list(project=projects[j], mode= "snapshot", year = years[i], nodes=n, edges=e, modularity=mod, density = den)
    results <- rbind(results,metrics,stringsAsFactors=FALSE)

  }
}


### snapshot error

error_snapshot <- data.frame(project=character(), mode=character(), metric=character(), MAE=numeric(), RMSE=numeric(), stringsAsFactors = FALSE) 


for (i in 1:length(projects)){
  
  results_p <- results[results$project==projects[i],]
  results_p_NOC <- results_commits[results_commits$project==projects[i],]
  
  offset <- min(results_p_NOC$year[results_p_NOC$mode=="snapshot simulation"])
  offset_value <- results_p_NOC$NOC[results_p_NOC$mode=="empirical"&results_p_NOC$year==offset-1]
  
  for (j in 1:nrow(results_p_NOC)){
    
    if (results_p_NOC$mode[j]=="snapshot simulation") {
      results_p_NOC$NOC[j] <- results_p_NOC$NOC[j]+offset_value
   }
  }
  
  ### modularity
  modularity <- results_p[,c(2,3,6)]
  empirical<-modularity$modularity[modularity$mode=="empirical"]
  predicted<-modularity$modularity[modularity$mode=="project-specific simulation"]  
  predicted2<-modularity$modularity[modularity$mode=="snapshot"]
  
  MAE=MAE(predicted,empirical)
  RMSE=RMSE(predicted, empirical)
  
  sim_error <- list(project=projects[i], mode="project-specific", metric="modularity",MAE=MAE,RMSE=RMSE)
  error_snapshot <- rbind(error_snapshot,sim_error,stringsAsFactors=FALSE)
  
  #start comparison after snapshot initialization
  
  temp <- length(predicted2[predicted2==0])
  predicted2 <- predicted2[predicted2!=0]
  empirical <- empirical[temp+1:length(empirical)]
  length(empirical)<-length(predicted2)
  
  MAE=MAE(predicted2,empirical)
  RMSE=RMSE(predicted2, empirical)
  
  sim_error <- list(project=projects[i], mode="snapshot", metric="modularity",MAE=MAE,RMSE=RMSE)
  error_snapshot <- rbind(error_snapshot,sim_error,stringsAsFactors=FALSE)
  
  ### density
  # omit first observation for density since it is initialized with 1
  density <- results_p[,c(2,3,7)]
  empirical<-density$density[density$mode=="empirical"]
  predicted<-density$density[density$mode=="project-specific simulation"]  
  empirical<-empirical[2:length(empirical)]
  predicted<-predicted[2:length(predicted)]
  predicted2<-density$density[density$mode=="snapshot"]
  
  MAE=MAE(predicted,empirical)
  RMSE=RMSE(predicted, empirical)
  
  sim_error <- list(project=projects[i], mode="project-specific", metric="density",MAE=MAE,RMSE=RMSE)
  error_snapshot <- rbind(error_snapshot,sim_error,stringsAsFactors=FALSE)
  
  #start comparison after snapshot initialization
  
  temp <- length(predicted2[predicted2==1])
  predicted2 <- predicted2[predicted2!=1]
  empirical <- empirical[temp:length(empirical)]
  length(empirical)<-length(predicted2)
  
  MAE=MAE(predicted2,empirical)
  RMSE=RMSE(predicted2, empirical)
  
  sim_error <- list(project=projects[i], mode="snapshot", metric="density",MAE=MAE,RMSE=RMSE)
  error_snapshot <- rbind(error_snapshot,sim_error,stringsAsFactors=FALSE)
  
  ### NOC
  empirical<-results_p_NOC$NOC[results_p_NOC$mode=="empirical"]
  predicted<-results_p_NOC$NOC[results_p_NOC$mode=="project-specific simulation"]  
  predicted2<-results_p_NOC$NOC[results_p_NOC$mode=="snapshot simulation"]  
  
  MAE=MAE(predicted,empirical)
  RMSE=RMSE(predicted, empirical)
  
  sim_error <- list(project=projects[i],mode="project-specific",metric="NOC",MAE=MAE,RMSE=RMSE)
  error_snapshot <- rbind(error_snapshot,sim_error,stringsAsFactors=FALSE)
  
  #start comparison after snapshot initialization
  
  temp <- length(predicted2[predicted2!=0])
  predicted2 <- predicted2[predicted2!=0]
  empirical <- empirical[(length(empirical)-length(predicted2)+1):length(empirical)]
  
  
  MAE=MAE(predicted2,empirical)
  RMSE=RMSE(predicted2, empirical)
  
  sim_error <- list(project=projects[i], mode="snapshot", metric="NOC",MAE=MAE,RMSE=RMSE)
  error_snapshot <- rbind(error_snapshot,sim_error,stringsAsFactors=FALSE)
  
  
  ### NOF
  nodes <- results_p[,c(2,3,4)]
  empirical<-nodes$nodes[nodes$mode=="empirical"]
  predicted<-nodes$nodes[nodes$mode=="project-specific simulation"]  
  predicted2<-nodes$nodes[nodes$mode=="snapshot"]
  
  MAE=MAE(predicted,empirical)
  RMSE=RMSE(predicted, empirical)
  
  sim_error <- list(project=projects[i],mode="project-specific",metric="NOF",MAE=MAE,RMSE=RMSE)
  error_snapshot <- rbind(error_snapshot,sim_error,stringsAsFactors=FALSE)
  
  #start comparison after snapshot initialization
  
  temp <- length(predicted2[predicted2!=2])
  predicted2 <- predicted2[predicted2!=2]
  empirical <- empirical[(length(empirical)-length(predicted2)+1):length(empirical)]

  
  MAE=MAE(predicted2,empirical)
  RMSE=RMSE(predicted2, empirical)
  
  sim_error <- list(project=projects[i], mode="snapshot", metric="NOF",MAE=MAE,RMSE=RMSE)
  error_snapshot <- rbind(error_snapshot,sim_error,stringsAsFactors=FALSE)
  
}
 

#### test normality

# shapiro wilk 

# bonferroni correction with n=32
# alpha = 0.00156

# MAE 

# wilcox test for significance
# alpha = 0.0031

shapiro.test(error_snapshot$MAE[error_snapshot$metric=="density"&error_snapshot$mode=="snapshot"])
shapiro.test(error_snapshot$MAE[error_snapshot$metric=="density"&error_snapshot$mode=="project-specific"])

wilcox.test(error_snapshot$MAE[error_snapshot$metric=="density"&error_snapshot$mode=="snapshot"], 
            error_snapshot$MAE[error_snapshot$metric=="density"&error_snapshot$mode=="project-specific"], paired = TRUE)

shapiro.test(error_snapshot$MAE[error_snapshot$metric=="modularity"&error_snapshot$mode=="snapshot"])
shapiro.test(error_snapshot$MAE[error_snapshot$metric=="modularity"&error_snapshot$mode=="project-specific"])

wilcox.test(error_snapshot$MAE[error_snapshot$metric=="modularity"&error_snapshot$mode=="snapshot"], 
            error_snapshot$MAE[error_snapshot$metric=="modularity"&error_snapshot$mode=="project-specific"], paired = TRUE)

shapiro.test(error_snapshot$MAE[error_snapshot$metric=="NOC"&error_snapshot$mode=="snapshot"])
shapiro.test(error_snapshot$MAE[error_snapshot$metric=="NOC"&error_snapshot$mode=="project-specific"])

wilcox.test(error_snapshot$MAE[error_snapshot$metric=="NOC"&error_snapshot$mode=="snapshot"], 
            error_snapshot$MAE[error_snapshot$metric=="NOC"&error_snapshot$mode=="project-specific"], paired = TRUE)

shapiro.test(error_snapshot$MAE[error_snapshot$metric=="NOF"&error_snapshot$mode=="snapshot"])
shapiro.test(error_snapshot$MAE[error_snapshot$metric=="NOF"&error_snapshot$mode=="project-specific"])

wilcox.test(error_snapshot$MAE[error_snapshot$metric=="NOF"&error_snapshot$mode=="snapshot"], 
            error_snapshot$MAE[error_snapshot$metric=="NOF"&error_snapshot$mode=="project-specific"], paired = TRUE)

# RMSE 


shapiro.test(error_snapshot$RMSE[error_snapshot$metric=="density"&error_snapshot$mode=="snapshot"])
shapiro.test(error_snapshot$RMSE[error_snapshot$metric=="density"&error_snapshot$mode=="project-specific"])

wilcox.test(error_snapshot$RMSE[error_snapshot$metric=="density"&error_snapshot$mode=="snapshot"], 
            error_snapshot$RMSE[error_snapshot$metric=="density"&error_snapshot$mode=="project-specific"], paired = TRUE)

shapiro.test(error_snapshot$RMSE[error_snapshot$metric=="modularity"&error_snapshot$mode=="snapshot"])
shapiro.test(error_snapshot$RMSE[error_snapshot$metric=="modularity"&error_snapshot$mode=="project-specific"])

wilcox.test(error_snapshot$RMSE[error_snapshot$metric=="modularity"&error_snapshot$mode=="snapshot"], 
            error_snapshot$RMSE[error_snapshot$metric=="modularity"&error_snapshot$mode=="project-specific"], paired = TRUE)

shapiro.test(error_snapshot$RMSE[error_snapshot$metric=="NOC"&error_snapshot$mode=="snapshot"])
shapiro.test(error_snapshot$RMSE[error_snapshot$metric=="NOC"&error_snapshot$mode=="project-specific"])

wilcox.test(error_snapshot$RMSE[error_snapshot$metric=="NOC"&error_snapshot$mode=="snapshot"], 
            error_snapshot$RMSE[error_snapshot$metric=="NOC"&error_snapshot$mode=="project-specific"], paired = TRUE)

shapiro.test(error_snapshot$RMSE[error_snapshot$metric=="NOF"&error_snapshot$mode=="snapshot"])
shapiro.test(error_snapshot$RMSE[error_snapshot$metric=="NOF"&error_snapshot$mode=="project-specific"])

wilcox.test(error_snapshot$RMSE[error_snapshot$metric=="NOF"&error_snapshot$mode=="snapshot"], 
            error_snapshot$RMSE[error_snapshot$metric=="NOF"&error_snapshot$mode=="project-specific"], paired = TRUE)


### error boxplots


#MAE

p<-ggplot(error_snapshot[error_snapshot$metric=="density",], aes(x=metric, y=MAE, fill=mode))+geom_boxplot()
print(p)

p<-ggplot(error_snapshot[error_snapshot$metric=="modularity",], aes(x=metric, y=MAE, fill=mode))+geom_boxplot()
print(p)

p<-ggplot(error_snapshot[error_snapshot$metric=="NOC",], aes(x=metric, y=MAE, fill=mode))+geom_boxplot()
print(p)

p<-ggplot(error_snapshot[error_snapshot$metric=="NOF",], aes(x=metric, y=MAE, fill=mode))+geom_boxplot()
print(p)

#RMSE

p<-ggplot(error_snapshot[error_snapshot$metric=="density",], aes(x=metric, y=RMSE, fill=mode))+geom_boxplot()
print(p)

p<-ggplot(error_snapshot[error_snapshot$metric=="modularity",], aes(x=metric, y=RMSE, fill=mode))+geom_boxplot()
print(p)

p<-ggplot(error_snapshot[error_snapshot$metric=="NOC",], aes(x=metric, y=RMSE, fill=mode))+geom_boxplot()
print(p)

p<-ggplot(error_snapshot[error_snapshot$metric=="NOF",], aes(x=metric, y=RMSE, fill=mode))+geom_boxplot()
print(p)



###### developer classification
### for the zookeeper example from the paper ###

setwd(path)
setwd(paste(getwd(),"/",projects[5],sep = ""))
setwd("./empirical")

commits <- fromJSON(paste(projects[5],"_data.json",sep=""), flatten=TRUE)

# onion model threshold
quantile(commits$identities$numberOfCommits, 0.8) 

ggplot(commits$identities, aes(x = reorder(name, -numberOfCommits), y = (numberOfCommits),group=type, fill=type))+
  geom_bar(stat = "identity")+
  labs(x = "Developer", y="Number of Commits",fill="role")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13))+
  geom_vline(xintercept=20.5, colour="blue",size=1.5)+
  geom_text(aes(35,500,label = "onion model threshold", vjust = -1), size=4.5, colour="blue")





