## This file compares frequencies in volunteer data v. surgery data
# We will do monocytes, CD4 T cells, CD8 T cells, B cells, NK cells, Granulocytes
# Need to locate both data sets, put them into data frames

library(dplyr)
library(ggplot2)

## Get volunteer data (n = 20)
# Run 1-make-signaling-structures.R
# Run 2-make-frequency-structures.R

volunteers_df <- dplyr::filter(frequency_df, Conditions== "Basal")
volunteers_df <- dplyr::filter(volunteers_df, ! Individuals %in% c("AT1", "AT2","CVID1", "CVID2","CVID3","CVID4"))
volunteers_df <- dplyr::select(volunteers_df, Sample_ID:classical_monocytes_frequency)


## Get surgery data (n = 26)

surgery_directory <- "~/Documents/stims_v_postsurg/postSurg/"
matrices <- list.files(surgery_directory, pattern = "frequencies")
timepts <- c("BL", "1h", "24h", "72h", "1mo")
surgery_df <- c()
surgery_difference_df <- c()

for (i in 1:length(timepts)){
  ind <- grep(timepts[i], matrices)
  file <- read.csv(paste(surgery_directory,matrices[ind],sep=""))
  df <- data.frame(Patients = file$X, Timepoint = rep(timepts[i],26), file[,c(2:4,6,7,9)])
  surgery_df <- rbind(surgery_df, df)
  if (i == 1){
    fileBL <- file
  } else {
    diff_df <- data.frame(Patients = file$X, Timepoint = rep(paste(timepts[i],"vBL",sep=""),26),
                          (file[,c(2:4,6,7,9)] - fileBL[,c(2:4,6,7,9)]))
    surgery_difference_df <- rbind(surgery_difference_df, diff_df)
  }
  
}

# first want to plot for a given cell type all the values unpaired
# conclusion: see differences but would not be able to be given a random value and say this is out of the norm (need presurg ref)

plot_values <- function(data_frame, plotname, directoryname){
  p <- list()
  
  for (i in 1:6){
    a <- ggplot(data_frame, aes_string(x = "Timepoint",y = colnames(data_frame)[i+2]))
    p[[i]] <- a + geom_point()
  }
  
  for (i in 1:length(p)) {
    ggsave(paste(directoryname, plotname, i, '.pdf', sep=''), p[[i]], width = 11, height = 8)
  }
}

plot_values(surgery_df, "surgery_plot", '~/Documents/healthyNormals/frequency_work/plots/')

# next would like to compare differences, i.e. how big do I change from day 1 to day 2 v. from before to after surgery?
# we will want the 6 comparisons from the normals and 4 comparisons of surgery (BL to each other one)

# surgery alone
plot_values(surgery_difference_df, "surgery_difference_plot", '~/Documents/healthyNormals/frequency_work/plots/')

# need volunteers difference data frame; here we are taking absolute value differences
volunteers_difference_df <- c()
days = volunteers_df$Timepoints[1:4]

for (i in 1:length(days)){
  for (j in 2:length(days)){
    if (i < j){
      df1 <- dplyr::filter(volunteers_df, Timepoints == days[i])
      df2 <- dplyr::filter(volunteers_df, Timepoints == days[j])
      df1 <- dplyr::select(df1, neutrophils_frequency:classical_monocytes_frequency)
      df2 <- dplyr::select(df2, neutrophils_frequency:classical_monocytes_frequency)
      vol_diff_df <- data.frame(Individuals = volunteers_df$Individuals, Timepoint = rep(paste(days[j]," v ",days[i],sep=""),20),
                                abs(df2 - df1))
      volunteers_difference_df <- rbind(volunteers_difference_df, vol_diff_df)
    }
  }
}

plot_values(volunteers_difference_df, "volunteers_difference_plot", '~/Documents/healthyNormals/frequency_work/plots/')

# To compare on same plot, we need a single data frame with both the volunteer differences and the surgery differences, 
# with the same names for frequencies

vol <- data.frame(Individuals = volunteers_difference_df$Individuals, Timepoint = volunteers_difference_df$Timepoint,
           Neutrophils = volunteers_difference_df$neutrophils_frequency,
           Monocytes = volunteers_difference_df$classical_monocytes_frequency,
           CD4Tcells = volunteers_difference_df$CD4_T_cells_frequency,
           CD8Tcells = volunteers_difference_df$CD8_T_cells_frequency,
           Bcells = volunteers_difference_df$B_cells_frequency,
           NKcells = volunteers_difference_df$NK_cells_frequency)

surg <- data.frame(Individuals = as.factor(surgery_difference_df$Patients), Timepoint = surgery_difference_df$Timepoint,
                   Neutrophils = surgery_difference_df$Granulocytes_freq,
                   Monocytes = surgery_difference_df$AllMonos_freq,
                   CD4Tcells = surgery_difference_df$CD4Tcells_freq,
                   CD8Tcells = surgery_difference_df$CD8Tcells_freq,
                   Bcells = surgery_difference_df$Bcells_freq,
                   NKcells = surgery_difference_df$NKcells_freq)

differences_df <- rbind(vol,surg)

plot_values(differences_df, "difference_plot", '~/Documents/healthyNormals/frequency_work/plots/')

# for absolute differences

nums <- sapply(differences_df, is.numeric)
abs_differences_df <- cbind(differences_df[,!nums],abs(differences_df[,nums]))
plot_values(abs_differences_df, "abs_difference_plot", '~/Documents/healthyNormals/frequency_work/plots/')

# calculating statistics




