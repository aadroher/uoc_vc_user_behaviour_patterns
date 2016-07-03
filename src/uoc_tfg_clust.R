###################################################################
# UOC - TFG
# Codi corresponent al TFG:
# Patrons de connexió al CV de la UOC 
# Capítol 5
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

day_equitters <- 
  students %>%
    select(student_num, last_session) %>%
    mutate(
      last_session = last_session %>% as.Date,
      early_quitter = (last_session < last_session %>% median()) * 1
    ) %>% 
    arrange(student_num)

m <- day_equitters %>%
    ggplot(aes(x = last_session, fill=factor(early_quitter)))

p <-
  m +
  geom_bar(binwidth = 1) +
  scale_fill_brewer(
    name = expression(kappa),
    type = "qua"
  ) +
  xlab(expression(l_sess)) + 
  theme_grey(8)

ggsave(p, 
       filename = "l_sess_equitter_bar.pdf",
       path = img_dir,
       units = "cm",
       width= 10,
       height = 10)

acc <- function(v){
  if (mean(c) > 0.5) {
    return(sum(v)/length(v))
  } else {
    return(1 - sum(v)/length(v))
  }
}


student_attrs <- 
  student_days_wd %>%
  arrange(student_num) %>% 
  select(-student_num, -student_id)


wss <- (nrow(student_attrs)-1)*sum(apply(student_attrs,2,var))
accs <- (nrow(student_attrs)-1)*sum(apply(student_attrs,2,var))
for (i in 2:16){
  varname <- paste("fit_", i, sep = "")
  
  assign(varname, 
         kmeans(student_attrs,
                algorithm="Lloyd",
                centers=i, 
                iter.max=1000))
  
  kmeans_clust <- 
    get(varname)$centers %>%
    data.frame() %>%
    tbl_df() %>%
    mutate(cluster=row_number()) %>%
    melt(id=c("cluster"),
         variable.name="date",
         value.name="active") %>%
    tbl_df() %>% 
    mutate(date = (date %>%
                     as.character() %>%
                     substr(start=2, stop=20) %>%
                     ymd() %>%
                     as.Date())) %>%
    group_by(cluster)
  
  m <- kmeans_clust %>% ggplot(aes(x=date, 
                                   y=active))
  
  p <- 
    m + 
    geom_line() +
    stat_smooth(method="loess") +
    facet_grid(cluster ~ . ) +
    ylab("activity_rate") +
    theme_grey(8)
  
  ggsave(p, 
         filename = paste("presence", i, "clust.pdf", sep = "_"),
         path = img_dir,
         units = "cm",
         width= 10,
         height = 2.5 * i)
  
  p <- 
    m + 
    geom_line(size=.25) +
    stat_smooth(method="loess", size=.25) +
    facet_grid(cluster ~ . ) +
    ylab(NULL) +
    xlab(NULL) +
    scale_y_continuous(breaks = c(0,0.5,1)) +
    theme_grey(6)
  
  ggsave(p, 
         filename = paste("presence", i, "clust_pres.pdf", sep = "_"),
         path = img_dir,
         units = "cm",
         width= 7.5,
         height = 8)
  
  # Dendrograms 
  # Complete method
  dendrogram <- 
    get(varname)$centers %>% 
    data.frame() %>% 
    tbl_df() %>% 
    dist(upper = TRUE) %>% 
    hclust(method="complete") %>% 
    ggdendrogram() +
    xlab("clusters") +
    ylab(NULL) +
    theme_grey(8)
  
  ggsave(dendrogram, 
         filename = paste("presence_complete_dendrogram_", i , "means_clusts.pdf", sep = ""),
         path = img_dir,
         units = "cm",
         width= 5,
         height = 5)
  
  # Plot the proportion of memberships to each cluster
  
  early_quittance <- 
    data.frame(cluster = get(varname)$cluster) %>%
    tbl_df() %>%
    mutate(cluster = factor(cluster)) %>%
    cbind(day_equitters) %>%
    tbl_df()

  acc <-
    early_quittance %>%
    group_by(cluster) %>%
    summarise(acc = ((mean(early_quitter) > 0.5) * 1) *
                      (sum(early_quitter) / n()) +
                    ((mean(early_quitter) <= 0.5) * 1) *
                      (1 - (sum(early_quitter) / n()))
              ) %>%
    ungroup() %>%
    summarise(acc = mean(acc))

  p <- 
    early_quittance %>%
    group_by(cluster, early_quitter) %>%
    ggplot(aes(x=cluster)) + 
    geom_bar() +
    xlab("cluster") + 
    ylab("n") +
    theme_grey(8)
  
  ggsave(p, 
         filename = paste("user_presence_", i , "means_clusts.pdf", sep = ""),
         path = img_dir,
         units = "cm",
         width= i*0.5 + 4,
         height = 5)
  
  p <- 
    early_quittance %>%
    group_by(cluster, early_quitter) %>%
    ggplot(aes(x=cluster, fill=factor(early_quitter))) + 
    geom_bar() +
    xlab("cluster") +
    ylab("n") +
  ggtitle("user_days") +
    scale_fill_brewer(
      type="qual",
      name = expression(kappa)) + 
    theme_grey(8)
  
  ggsave(p, 
         filename = paste("user_presence_pred", i , "means_clusts.pdf", sep = ""),
         path = img_dir,
         units = "cm",
         width= i*0.3 + 4,
         height = 5)

  freqs  <- early_quittance %>% 
            group_by(cluster) %>% 
            summarise(n = n()) %>% 
            ungroup() %>% 
            mutate(f = n / sum(n)) 
  
  
  
  wss[i] <- sum(get(varname)$withinss)
  accs[i] <- acc$acc
}
d_wthn_sq_sum <- tbl_df(data_frame(cluster=c(2:length(wss)),wthn_sq_sum=wss[2:length(wss)]))
m <- d_wthn_sq_sum %>% ggplot(aes(x = cluster, y = wthn_sq_sum))
p <- 
  m + 
  geom_line() + 
  xlab(expression(k)) +
  ylab(expression(theta[k])) +
  ggtitle(expression(user_days)) + 
  scale_x_continuous(breaks = c(1:length(wss))) +
  theme_grey(8)

ggsave(p, 
       filename = "day_presence_clust_wss_line.pdf",
       path = img_dir,
       units = "cm",
       width= 10,
       height = 8)

d_accuracies <- tbl_df(data_frame(cluster=c(2:length(accs)),accuracy=accs[2:length(accs)]))

m <- 
    d_accuracies %>%
    mutate(accuracy = (2 * accuracy - 1) ) %>% 
    ggplot(aes(x = cluster, y = accuracy))

p <- 
  m + 
  geom_line() + 
  ggtitle(expression(user_days)) + 
  scale_x_continuous(
    breaks = 2:length(accs),
    limits = c(2,16)
  ) +
  ylab(expression(bar(acc[k]))) +
  xlab(expression(k)) +
  ylim(0,1) +
  theme_grey(8)

ggsave(p, 
       filename = "day_presence_clust_acc_line.pdf",
       path = img_dir,
       units = "cm",
       width= 10,
       height = 8)


#####################
# Clutsering on weeks

student_attrs <- 
  student_weeks_wd %>%
  arrange(student_num) %>% 
  select(-student_num, -student_id)

wss <- (nrow(student_attrs)-1)*sum(apply(student_attrs,2,var))
accs <- (nrow(student_attrs)-1)*sum(apply(student_attrs,2,var))
for (i in 2:16){
  varname <- paste("fit_weeks_", i, sep = "")
  
  assign(varname, 
         kmeans(student_attrs,
                algorithm="Lloyd",
                centers=i, 
                iter.max=1000))
  
  kmeans_clust <- 
    get(varname)$centers %>%
    data.frame() %>%
    tbl_df() %>%
    mutate(cluster=row_number()) %>%
    melt(id=c("cluster"),
         variable.name="p_week_date",
         value.name="activity") %>%
    tbl_df() %>% 
    mutate(p_week_date = (p_week_date %>%
                            as.character() %>%
                            substr(start=2, stop=20) %>%
                            ymd() %>%
                            as.Date())) %>%
    group_by(cluster)
  
  m <- kmeans_clust %>% ggplot(aes(x=p_week_date, y=activity))
  
  p <- 
    m + 
    geom_line(color="grey") +
    geom_point() +
    facet_grid(cluster ~ . ) +
    ylab("activity_rate") +
    theme_grey(8)
  
  ggsave(p, 
         filename = paste("week_presence", i, "clust.pdf", sep = "_"),
         path = img_dir, 
         units = "cm",
         width= 10,
         height = 2.5 * i)
  
  # Dendrograms 
  # Complete method
  dendrogram <- 
    get(varname)$centers %>% 
    data.frame() %>% 
    tbl_df() %>% 
    dist(upper = TRUE) %>% 
    hclust(method="complete") %>% 
    ggdendrogram() +
    xlab("clusters") +
    ylab(NULL) +
    theme_grey(8)
  #     ggtitle(paste(" ", i, "-means complete dist", sep = ""))
  
  ggsave(dendrogram, 
         filename = paste("week_presence_complete_dendrogram_", i , "means_clusts.pdf", sep = ""),
         path = img_dir,
         units = "cm",
         width= 5,
         height = 5)
  
  # Plot the proportion of memberships to each cluster
  
  early_quittance <- 
    data.frame(cluster = fit_weeks_6$cluster) %>%
    tbl_df() %>%
    mutate(cluster = factor(cluster)) %>%
    cbind(day_equitters) %>%
    tbl_df()
  
  acc <-
    early_quittance %>%
    group_by(cluster) %>%
    summarise(acc = ((mean(early_quitter) > 0.5) * 1) *
                (sum(early_quitter) / n()) +
                ((mean(early_quitter) <= 0.5) * 1) *
                (1 - (sum(early_quitter) / n()))
    ) %>%
    ungroup() %>%
    summarise(acc = mean(acc))
  
  p <- 
    early_quittance %>%
    group_by(cluster, early_quitter) %>%
    ggplot(aes(x=cluster)) + 
    geom_bar() +
    xlab("cluster") +
    ylab("n") +
    theme_grey(8)
  
  ggsave(p, 
         filename = paste("week_user_presence_", i , "means_clusts.pdf", sep = ""),
         path = img_dir,
         units = "cm",
         width= i*0.5 + 4,
         height = 5)
  
  p <- 
    early_quittance %>%
    group_by(cluster, early_quitter) %>%
    ggplot(aes(x=cluster, fill=factor(early_quitter))) + 
    geom_bar() +
    xlab("cluster") +
    ylab("n") +
    ggtitle("user_weeks") + 
    scale_fill_brewer(
      type="qual",
      name = expression(kappa)) + 
    theme_grey(8)
  
  ggsave(p, 
         filename = paste("week_user_presence_pred", i , "means_clusts.pdf", sep = ""),
         path = img_dir,
         units = "cm",
         width= i*0.3 + 4,
         height = 5)
  
  freqs  <- early_quittance %>% 
    group_by(cluster) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    mutate(f = n / sum(n)) 
  
  
  
  wss[i] <- sum(get(varname)$withinss)
  accs[i] <- acc$acc
}
w_wthn_sq_sum <- tbl_df(data_frame(cluster=c(2:length(wss)),wthn_sq_sum=wss[2:length(wss)]))
m <- w_wthn_sq_sum %>% ggplot(aes(x = cluster, y = wthn_sq_sum))
p <- 
  m + 
  geom_line() + 
  xlab(expression(k)) +
  ylab(expression(theta[k])) +
  ggtitle(expression(user_weeks)) + 
  scale_x_continuous(breaks = c(1:length(wss))) +
  theme_grey(8)

ggsave(p, 
       filename = "week_presence_clust_wss_line.pdf",
       path = img_dir,
       units = "cm",
       width= 10,
       height = 8)

w_accuracies <- tbl_df(data_frame(cluster=c(2:length(accs)),accuracy=accs[2:length(accs)]))

m <- 
  w_accuracies %>%
  mutate(accuracy = (2 * accuracy - 1)  ) %>%
  ggplot(aes(x = cluster, y = accuracy))

p <- 
  m + 
  geom_line() + 
  ggtitle(expression(user_weeks)) + 
  scale_x_continuous(
    breaks = 2:length(accs),
    limits = c(2,16)
  ) +
  xlab(expression(k)) +
  ylab(expression(bar(acc[k]))) + 
  ylim(0,1) +
  theme_grey(8)

ggsave(p, 
       filename = "week_presence_clust_acc_line.pdf",
       path = img_dir,
       units = "cm",
       width= 10,
       height = 8)
