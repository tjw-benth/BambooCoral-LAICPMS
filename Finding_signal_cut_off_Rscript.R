j <- 1

datapoints <- ggplot(data = Runs_Sample_23_16_1[[j]], aes(x = `Time [Sec]`)) 

for (a in 2:11) {
  datapoints <- datapoints + geom_line(aes_string(y = names(Runs_Sample_23_16_1[[j]])[a], color =  shQuote(names(Runs_Sample_23_16_1[[j]])[a])))
}

if (names(Runs_Sample_23_16_1[[j]][7]) != "Ca43") {stop(paste0("Wrong element! ", names(Runs_Sample_23_16_1[[j]][7]), " for Runs_Sample_23_16_1_", j))} # Ca43

Time_length <- length(Runs_Sample_23_16_1[[j]]$`Time [Sec]`)

Delta_Ca_dataframe <- data.frame(Delta_Ca = as.numeric(rep(NA, Time_length+14)),
                                 Time = c(Runs_Sample_23_16_1[[j]]$`Time [Sec]`,rep(NA,14))
                                  )

# Rolling 4 step difference
for(x in 1:length(Runs_Sample_23_16_1[[j]]$`Time [Sec]`)) {
Delta_Ca <- (Runs_Sample_23_16_1[[j]]$Ca43[x+3] - Runs_Sample_23_16_1[[j]]$Ca43[x])/(Runs_Sample_23_16_1[[j]]$`Time [Sec]`[x+3] - Runs_Sample_23_16_1[[j]]$`Time [Sec]`[x])
  Delta_Ca_dataframe$Delta_Ca[x+3]  <- ceiling(Delta_Ca)
  Delta_Ca_dataframe
}

# In case there is a jump in the calcium at the exterior of the coral (due to calcification boundary)
Cut_off1 <- max(Delta_Ca_dataframe[which(Delta_Ca_dataframe$Time <= 50),]$Delta_Ca, na.rm = T)
Cut_off2 <- min(Delta_Ca_dataframe[which(Delta_Ca_dataframe$Time >= max(Delta_Ca_dataframe$Time, na.rm = T)/2),]$Delta_Ca, na.rm = T)
Pre_blank_cut_off <- Delta_Ca_dataframe[which(Delta_Ca_dataframe$Delta_Ca == Cut_off1),]$Time
Post_blank_cut_off <- Delta_Ca_dataframe[which(Delta_Ca_dataframe$Delta_Ca == Cut_off2),]$Time

datapoints + 
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 30.5) + # preblank cut off
  geom_vline(xintercept = max(Runs_Sample_23_16_1[[j]]$`Time [Sec]`, na.rm = T)) +
  geom_vline(xintercept = max(Runs_Sample_23_16_1[[j]]$`Time [Sec]`, na.rm = T)-45) + #postblank cut off
  geom_vline(xintercept = Pre_blank_cut_off, linetype = 2) + # start of analysis
  geom_vline(xintercept = Post_blank_cut_off-2, linetype = 2) + # end of analysis
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "l") +
  annotate("text", x = 100, y = 100, hjust = 1, vjust = 2, label = names(Runs_Sample_23_16_1[j]), col = "black") +
  ylab("Signal") +
  scale_fill_discrete(name = "Element") +
  theme_classic()
