###################################################################
# UOC - TFG
# Codi corresponent al TFG:
# Patrons de connexió al CV de la UOC 
# Capítol 6
# Armand Adroher Salvia 01/2015
###################################################################

# Biblioteques
library("magrittr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("reshape2")
library("doParallel")
library("foreach")
library("markovchain")
library("HMM")

#######################
# Build the markov chains on states.
# Begin on weeks


student_weeks_stats <- 
  student_weeks %>%
  mutate(is_active = (activity > 0) * 1) %>%
  group_by(student_num) %>%
  summarise(
    is_active = mean(is_active),
    activity_rate = mean(activity_rate)
  ) %>%
  arrange(desc(activity_rate))

regular_students <-
  student_weeks_stats %>%
  filter(is_active < 1)

student_mcms <- NULL 
for(i in regular_students$student_num) {
  student_i_weeks <-
    student_weeks %>%
    filter(student_num == i ) %>%
    mutate(is_active = activity > 0) %>%
    arrange(p_week)
  
  student_i_days <-
    student_days %>%
    filter(student_num == i) %>%
    arrange(date)
  
  #         num_week_mcm1 <- student_i_weeks$activity %>% as.character %>% markovchainFit
  bin_week_mcm1 <- student_i_weeks$is_active %>% as.character %>% markovchainFit
  bin_date_mcm1 <- student_i_days$is_active %>% as.character %>% markovchainFit
  
  student_mcms <- 
    student_i_weeks %>%
    group_by(student_num) %>%
    summarise(
      activity_rate = mean(activity_rate),
      wFF = bin_week_mcm1$estimate[1,1],
      wFT = bin_week_mcm1$estimate[1,2],
      wTF = bin_week_mcm1$estimate[2,1],
      wTT = bin_week_mcm1$estimate[2,2],
      dFF = bin_date_mcm1$estimate[1,1],
      dFT = bin_date_mcm1$estimate[1,2],
      dTF = bin_date_mcm1$estimate[2,1],
      dTT = bin_date_mcm1$estimate[2,2]
    ) %>%
    rbind(student_mcms)
}

superactive <- 
  student_weeks_stats %>%
  filter(is_active == 1)

##
# Get a plot

m <-
  student_mcms %>%
  select(student_num, dFT, dTT) %>%
  ggplot(aes(x = dFT, y = dTT))

p <-
  m +
  geom_point(alpha = .1, shape = 20) +
  xlab(expression(alpha)) +
  ylab(expression(beta)) +
  xlim(0,1) +
  coord_fixed() +
  theme_grey(8) +
  stat_binhex() +
  scale_fill_gradient(name = "count", trans = "log")
stat_density2d(
  geom="tile", 
  aes(fill = ..density..), 
  contour = FALSE)

m +
  stat_density2d(
    geom="tile", 
    aes(fill = ..density..), 
    contour = FALSE) +
  geom_raster() +
  scale_fill_gradient(name = "count", trans = "log")


ggsave(p, 
       filename = "mc_params_days.png",
       path = img_dir,
       units = "cm",
       width= 14,
       height = 14)

m <-
  student_mcms %>%
  select(student_num, wFT, wTT) %>%
  ggplot(aes(x = wFT, y = wTT))

p <-
  m +
  geom_point(alpha = .1, shape = 20) +
  xlab(expression(alpha)) +
  ylab(expression(beta)) +
  xlim(0,1) +
  coord_fixed() +
  theme_grey(8)

ggsave(p, 
       filename = "mc_params_weeks.png",
       path = img_dir,
       units = "cm",
       width= 14,
       height = 14)




student_attrs <-
  student_mcms %>%
  arrange(student_num) %>%
  select(wFF, wFT, wTF, wTT)



wss <- (nrow(student_attrs)-1)*sum(apply(student_attrs,2,var))
for (i in 2:16){
  varname <- paste("fit_mcmw", i, sep = "")
  
  assign(varname, 
         kmeans(student_attrs,
                algorithm="Lloyd",
                centers=i, 
                iter.max=1000))
  
  kmeans_clust <- 
    get(varname)$centers %>%
    data.frame() %>%
    tbl_df() %>%
    mutate(
      cluster = row_number() %>% as.character(),
      clust_size = get(varname)$size
    ) %>%
    group_by(cluster)
  
  m <- kmeans_clust %>% 
    ggplot(aes(x=wFT, y=wTT, 
               colour = cluster,
               size = clust_size))
  
  p <- m + 
    geom_point(shape = 16) +
    xlim(0,1) +
    ylim(0,1) +
    coord_fixed() +
    scale_size_area()
  
  ggsave(p, 
         filename = paste("mcmw", i, "clust.pdf", sep = "_"),
         path = img_dir)
  
  # Dendrograms 
  # Complete method
  dendrogram <- get(varname)$centers %>% 
    data.frame() %>% 
    tbl_df() %>% 
    dist(upper = TRUE) %>% 
    hclust(method="complete") %>% 
    ggdendrogram() + 
    ggtitle(paste("Markov chain week mat ", i, "-means complete dist", sep = ""))
  
  ggsave(dendrogram, 
         filename = paste("mcmw_complete_dendrogram_", i , "means_clusts.pdf", sep = ""),
         path = img_dir, 
         width= 10 * inch2cm,
         height = 10 * inch2cm)
  
  # Plot the proportion of memberships to each cluster
  
  p <- data.frame(cluster = get(varname)$cluster) %>%
    tbl_df() %>%
    mutate(cluster = factor(cluster)) %>%
    group_by(cluster) %>%
    ggplot(aes(x=cluster)) + 
    geom_bar() 
  
  ggsave(p, 
         filename = paste("mcmw_conn_", i , "means_clusts.pdf", sep = ""),
         path = img_dir, 
         width= 10 * inch2cm,
         height = 10 * inch2cm)
  
  
  
  wss[i] <- sum(get(varname)$withinss)
  
}
wthn_sq_sum <- tbl_df(data_frame(clust=c(1:length(wss)),wthn_sq_sum=wss))
m <- wthn_sq_sum %>% ggplot(aes(x = clust, y = wthn_sq_sum))
p <- m + geom_line() + geom_point()

ggsave(p, 
       filename = "clust_wss_mcmw_line.pdf",
       path = img_dir, 
       width= 10 * inch2cm,
       height = 10 * inch2cm)

###
# Try now with HMM
# We'll choose a 50% split for training and testing

train_ids <-
  students %>%
  select(student_id, student_num) %>%
  sample_frac(.5)

test_ids <-
  students %>%
  select(student_id, student_num) %>%
  setdiff(train_ids)


# Estimate p_aa
p_aa_df <- 
  train_ids %>%
  left_join(student_last_days) %>%
  mutate(activity_length = (last_day - first_day) %>% as.numeric) %>%
  select(activity_length) %>%
  summarise(
    med_activity_length = median(activity_length),
    #       p_aa = m_activity_length / ((last_day - first_day) %>% as.numeric),
    p_aa = (0.5)^(1/med_activity_length)
  )

# Estimate p_ac
p_ac_df <-
  train_ids %>%
  left_join(student_days) %>%
  left_join(student_last_days) %>%
  filter(date <= last_day) %>%
  group_by(student_num) %>%
  summarise(activity_connection_rate = sum(is_active) / n()) %>%
  ungroup() %>%
  summarise(p_ac = mean(activity_connection_rate))


# HMM params
states <- c("a", "q")
symbols <- c("1", "0")
start_p <- c(1,0)
trans_p <- c(p_aa_df$p_aa, (1 - p_aa_df$p_aa), 0, 1) %>% matrix(nrow=2, byrow = TRUE)
emission_p <- c(p_ac_df$p_ac, (1 - p_ac_df$p_ac), 0, 1) %>% matrix(nrow=2, byrow = TRUE)

hmm <- initHMM(
  states,
  symbols,
  start_p,
  trans_p,
  emission_p
)

# Try

stu_vit_test <-
  test_ids %>%
  select(student_num) %>%
  ungroup %>%
  left_join(student_days) %>%
  left_join(student_last_days) %>%
  mutate(quit = (date > last_day) * 1)

confusion_ms <- NULL

for(i in 2:nrow(semester)) {
  d <- semester[i,]$date
  confusion_ms <- 
    stu_vit_test %>%
    filter(date <= d) %>%
    group_by(student_num) %>%
    arrange(date) %>%
    mutate(
      is_active = is_active %>% as.character,
      pred_quit = (viterbi(hmm, is_active) == "q") * 1
    ) %>%
    ungroup() %>%
    filter(date == d) %>%
    mutate(
      tp = (quit == 1 & quit == pred_quit) * 1,
      tn = (quit == 0 & quit == pred_quit) * 1,
      fp = (quit == 0 & quit != pred_quit) * 1,
      fn = (quit == 1 & quit != pred_quit) * 1
    ) %>%
    summarise(
      pos = sum((quit == 1) * 1),
      neg = sum((quit == 0) * 1),
      tp = sum(tp),
      tn = sum(tn),
      fp = sum(fp),
      fn = sum(fn)
    ) %>% 
    mutate(date = d) %>%
    rbind(confusion_ms)
}

m <- 
  confusion_ms %>%
  mutate(
    pday = date,
    acc = (tp + tn) / (pos + neg) * 100,
    fn_r = fn / (pos + neg) * 100,
    fp_r = fp / (pos + neg) * 100,
    pos_r = pos / (pos + neg) * 100
  ) %>%
  select(pday,acc,fn_r,fp_r,pos_r) %>%
  melt(id.vars = "pday" ) %>%
  tbl_df() %>%
  group_by(variable) %>%
  ggplot(aes(x = pday, y = value, colour = variable)) 

p <- 
  m + 
  geom_line() +
  ggtitle("days") + 
  scale_color_brewer(
    palette = "Dark2",
    type = "qual",
    labels = c("accuracy", "false_negatives", "false_positives", "quit_users")
  ) +
  xlab(NULL) +
  ylab("%") +
  theme_grey(8)

ggsave(p, 
       filename = "hmm_days_confusion_m_day.pdf",
       path = img_dir,
       units = "cm",
       width= 14,
       height = 8)

# Do it with weeks

# Estimate p_aa_w
p_aa_w_df <- 
  train_ids %>%
  left_join(student_last_days) %>%
  mutate(
    activity_length = 
      ((last_day - first_day) %>% 
         as.integer()) %/% 7 
  ) %>%
  select(activity_length) %>%
  summarise(
    med_activity_length = median(activity_length),
    #       p_aa = m_activity_length / ((last_day - first_day) %>% as.numeric),
    p_aa_w = (0.5)^(1/med_activity_length)
  )

# Estimate p_ac
p_ac_w_df <- 
  train_ids %>%
  left_join(student_weeks) %>%
  left_join(student_last_days) %>%
  filter(p_week_date <= last_day) %>%
  mutate(is_active = (activity > 0) * 1) %>%
  group_by(student_num) %>%
  summarise(activity_connection_rate = sum(is_active) / n()) %>%
  ungroup() %>%
  summarise(p_ac_w = mean(activity_connection_rate))

# HMM params
states <- c("a", "q")
symbols <- c("1", "0")
start_p <- c(1,0)
trans_p <- c(p_aa_w_df$p_aa_w, (1 - p_aa_w_df$p_aa_w), 0, 1) %>% matrix(nrow=2, byrow = TRUE)
emission_p <- c(p_ac_w_df$p_ac_w, (1 - p_ac_w_df$p_ac_w), 0, 1) %>% matrix(nrow=2, byrow = TRUE)

hmm_weeks <- initHMM(
  states,
  symbols,
  start_p,
  trans_p,
  emission_p
)

stu_w_vit_test <-
  test_ids %>%
  select(student_num) %>%
  ungroup %>%
  left_join(student_weeks) %>%
  left_join(student_last_days) %>%
  mutate(
    quit = (p_week_date > last_day) * 1,
    is_active = (activity > 0) * 1 
  ) 

confusion_w_ms <- NULL

for(i in 2:17) {
  confusion_w_ms <- 
    stu_w_vit_test %>%
    filter(p_week <= d) %>%
    group_by(student_num) %>%
    arrange(p_week) %>%
    mutate(
      is_active = is_active %>% as.character,
      pred_quit = (viterbi(hmm_weeks, is_active) == "q") * 1
    ) %>%
    ungroup() %>%
    filter(p_week == i) %>%
    group_by(p_week_date, p_week) %>%
    mutate(
      tp = (quit == 1 & quit == pred_quit) * 1,
      tn = (quit == 0 & quit == pred_quit) * 1,
      fp = (quit == 0 & quit != pred_quit) * 1,
      fn = (quit == 1 & quit != pred_quit) * 1
    ) %>%
    summarise(
      pos = sum((quit == 1) * 1),
      neg = sum((quit == 0) * 1),
      tp = sum(tp),
      tn = sum(tn),
      fp = sum(fp),
      fn = sum(fn)
    ) %>% 
    ungroup() %>%
    rbind(confusion_w_ms)
}

m <- 
  confusion_w_ms %>%
  mutate(
    acc = (tp + tn) / (pos + neg) * 100,
    fn_r = fn / (pos + neg) * 100,
    fp_r = fp / (pos + neg) * 100,
    pos_r = pos / (pos + neg) * 100
  ) %>%
  select(p_week_date,acc,fn_r,fp_r,pos_r) %>%
  melt(id.vars = "p_week_date" ) %>%
  tbl_df() %>%
  group_by(variable) %>%
  ggplot(aes(x = p_week_date, y = value, colour = variable)) 

p <- 
  m + 
  geom_line() +
  geom_point() +
  ggtitle("weeks") + 
  scale_color_brewer(
    palette = "Dark2",
    type = "qual",
    labels = c("accuracy", "false_negatives", "false_positives", "quit_users")
  ) +
  xlab(NULL) +
  ylab("%") +
  theme_grey(8)

ggsave(p, 
       filename = "hmm_weeks_confusion_m_day.pdf",
       path = img_dir,
       units = "cm",
       width= 14,
       height = 8)

p <- 
  m + 
  geom_line() +
  geom_point() +
  scale_color_brewer(
    palette = "Dark2",
    type = "qual",
    labels = c("accuracy", "false_negatives", "false_positives", "quit_users")
  ) +
  xlab(NULL) +
  ylab("%") +
  theme_grey(6)

ggsave(p, 
       filename = "hmm_weeks_confusion_m_day_pres.pdf",
       path = img_dir,
       units = "cm",
       width= 14,
       height = 5)


confusion_ms %>% 
  arrange(desc(date)) %>%
  mutate(tp = (quit == 1 & quit == pred_quit) * 1)

cl <- makeCluster(detectCores())
registerDoParallel(cl)
confusion_ms <- foreach(i = 2:nrow(semester),
                        .combine = 'rbind',
                        .packages = c("dplyr",
                                      "magrittr",
                                      "lubridate",
                                      "HMM",)) %dopar% {
                                        d <- semester[i,]$date
                                        
                                        stu_vit_test %>%
                                          filter(date <= d) %>%
                                          group_by(student_num) %>%
                                          arrange(date) %>%
                                          mutate(
                                            is_active = is_active %>% as.character,
                                            pred_quit = (viterbi(hmm, is_active) == "q") * 1
                                          ) %>%
                                          summarise(
                                            tp = (quit == 1 && quit == pred_quit) * 1,
                                            tn = (quit == 0 && quit == pred_quit) * 1,
                                            fp = (quit == 0 && quit != pred_quit) * 1,
                                            fn = (quit == 1 && quit != pred_quit) * 1
                                          ) %>%
                                          summarise(
                                            tp = sum(tp),
                                            tn = sum(tn),
                                            fp = sum(fp),
                                            fn = sum(fn)
                                          ) %>% 
                                          mutate(date = d)                 
                                      }
stopCluster(cl)

m <- confusion_ms %>%
  ggplot(aes(x = date)) 
p <- m + geom_line(aes(y = tn))
