# Needed libraries
library("doParallel")
library("foreach")
library("RMySQL")
library("magrittr")
library("dplyr")
library("lubridate")
library("ggplot2")

first_sess <- sessions %>%
          arrange(session_start) %>%
          slice(1)
last_sess <- sessions %>%
      arrange(desc(session_expiration)) %>%
      slice(1)
clean_sessions <- sessions
sessions <- clean_sessions %>%
  mutate(p_day = as.numeric(difftime(
    as.Date(central_activity_point), 
    as.Date(first_sess$session_start), unit='days')))

sessions <- sessions %>%
  mutate(p_week = floor(as.numeric(difftime(
    as.Date(central_activity_point), 
    as.Date(first_sess$session_start), unit='weeks'))))

sessions <- sessions %>%
  mutate(w_day = (wday(central_activity_point) - 1 + 6 ) %% 7)

by_hour <- sessions %>% group_by(hour = hour(central_activity_point)) 
hours <- summarise(by_hour, students = n_distinct(student_id) , n = n())

qplot(hour, n, data=hours, geom="bar", stat="identity")
qplot(hour, n, data=hours) + stat_smooth(method=loess)
qplot(hour, n, data=hours, geom="line")

qplot(hour, students, data=hours, geom="bar", stat="identity")
qplot(hour, students, data=hours) + stat_smooth(method=loess)
qplot(hour, students, data=hours, geom="line")
  

by_pday <- sessions %>% group_by(p_day)
days <- summarise(by_pday, students = n_distinct(student_id), n = n())
qplot(p_day,n, data=days)

by_date <- sessions %>% group_by(date = as.Date(central_activity_point))
dates <- summarise(by_date, students = n_distinct(student_id), n = n(), sum_activity_duration = sum(activity_duration))
qplot(date, n, data=dates) + stat_smooth(method=loess)
qplot(date, students, data=dates) + stat_smooth(method=loess)
qplot(date, sum_activity_duration, data=dates) + stat_smooth(method=loess)

by_wday <- sessions %>% group_by(wday = (wday(central_activity_point) - 1 + 6 ) %% 7) 
wdays <- summarise(by_wday, students = n_distinct(student_id), n = n())
qplot(wday, n, data=wdays, geom="bar", stat="identity")
qplot(wday, students, data=wdays, geom="bar", stat="identity")

by_week <- sessions %>% group_by(p_week)
weeks <- summarise(by_week, students = n_distinct(student_id), n = n())
qplot(p_week, n, data=weeks, ylims=0) + stat_smooth(method=loess)
qplot(p_week, students, data=weeks, ylims=0) + stat_smooth(method=loess)

# ACHTUNG!
# qplot(as.numeric(student_id),activity_duration, data=sessions)

ggplot(sessions, aes(x=1, y=activity_duration)) + geom_boxplot()

ggplot(sessions, aes(x=activity_duration)) + geom_line(stat="density")

# Student summaries
by_student <- sessions %>%
              group_by(student_id)
students <- by_student %>% 
            summarise(
              n_sessions = n(),
              sum_activity_duration = sum(activity_duration),
              m_activity_duration = mean(activity_duration),
              sd_activity_duration = sd(activity_duration),
              m_wday = mean(w_day),
              sd_wday = sd(w_day),
              m_hour = mean(hour(central_activity_point)),
              sd_hour = sd(hour(central_activity_point)),
              first_session = min(session_start),
              last_session = max(session_start)
            )
students

ggplot(students, aes(x=n_sessions)) + geom_line(stat="density")
ggplot(students, aes(x=sum_activity_duration)) + geom_line(stat="density")
ggplot(students, aes(x=m_activity_duration)) + geom_line(stat="density")
ggplot(students, aes(x=m_wday)) + geom_line(stat="density")
ggplot(students, aes(x=m_hour)) + geom_line(stat="density")
ggplot(students, aes(x=sd_hour)) + geom_line(stat="density")
ggplot(students, aes(x=first_session)) + geom_line(stat="density")
ggplot(students, aes(x=last_session)) + geom_line(stat="density")
qplot(n_sessions, sum_activity_duration, data=students) + stat_smooth(method=lm)
ggplot(students, aes(x=first_session, y=last_session)) + geom_point(shape=20, alpha=1/20)
qplot(first_session, last_session, data=students) + stat_smooth(method=lm)

sessions %>% select(p_day) %>% arrange(desc(p_day))

r_student <- students %>% 
              sample_n(1)
r_student[["pday_1"]] <- 1
r_student %>% mutate(p_days = c(1:118))
r_s_sessions <- sessions %>%
                filter(student_id == r_student$student_id) %>%
                arrange(session_start)

merge(
  sessions %>% select(p_day) %>% distinct() %>% arrange(p_day),
  sessions %>% select(student_id) %>% distinct() %>% arrange(student_id)
)

# simple_sessions <- sessions %>% 
#                     select(student_id, p_day, p_week, activity_duration) %>%
#                     arrange(p_day) %>%
#                     group_by(student_id, p_day, p_week) 
# 
# students_pday <- simple_sessions %>% 
#                   summarise(a_duration = min(24*3600,sum(activity_duration))) %>%
#                   ungroup()
# students_pday_smpl <- students_pday %>%
# #                         sample_n(1000)  %>%
#                         mutate(student = row_number()) %>%
#                         select(-student_id)

simple_sessions <- sessions %>% 
  select(student_id, p_week, activity_duration) %>%
  group_by(student_id, p_week) 

students_pweek <- simple_sessions %>% 
  summarise(a_duration = sum(activity_duration)) %>%
  ungroup() %>%
  mutate(p_week = paste('w',formatC(p_week, width=2, flag="0"), sep='_'))

students_pweek_smpl <- students_pweek %>%
  mutate(student = row_number()) %>%
  select(-student_id)

# Cast it
students_pweek_wide <- tbl_df(dcast(students_pweek, student_id ~ p_week,
                        fun.aggregate = sum,
                        fill= 0,
                        value.var = "a_duration"))
clusters <- students_pweek_wide %>%
            select(-student_id) %>%
            kmeans(5) 
students_pweek_wide_nums <- students_pweek_wide %>%
                            select(-student_id)

wss <- (nrow(students_pweek_wide_nums)-1)*sum(apply(students_pweek_wide_nums,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(students_pweek_wide_nums,
                                     algorithm="Lloyd",
                                     centers=i, iter.max=1000)$withinss)
wthn_sq_sum <- tbl_df(data_frame(clust=c(1:length(wss)),wthn_sq_sum=wss))
qplot(clust,wthn_sq_sum,data=wthn_sq_sum, geom="line")

fit_4 <- kmeans(students_pweek_wide_nums,
                algorithm="Lloyd",
                centers=4, iter.max=1000)

fit_8 <- kmeans(students_pweek_wide_nums,
                algorithm="Lloyd",
                centers=8, iter.max=1000)

clust_students_pweek <- students_pweek_wide %>%
                        mutate(clust_4 = fit_4$cluster) %>%
                        mutate(clust_8 = fit_8$cluster)

melted_stu <- clust_students_pweek %>%
              arrange(clust_8) %>%
              mutate(student_n = c(1:length(clust_students_pweek$student_id))) %>%
              select(-clust_8, -clust_4) %>%
              melt(id = c('student_id', 'student_n'),
                   variable.name="week_n",
                   value.name="a_duration") %>%
              tbl_df() %>%
              left_join(
                clust_students_pweek %>% 
                  select(student_id, clust_8)
              ) 



# Density map
p <- ggplot(melted_stu, aes(student_n,week_n,fill=a_duration))
plot <- p + geom_raster(interpolate=TRUE) + 
  scale_fill_gradient(
    low="white",
    high="#132B43"
  )
plot 
for (i in 1:8) {
  plot <- plot + geom_vline(xintercept=(melted_stu %>% filter(clust_8==i) %>% arrange(desc(student_n)) %>% slice(1))$student_n)
}
plot
m <- fit_8$centers
clusters_8 <- data.frame(week_n = rep(colnames(m), each = nrow(m)), 
                         cluster = rep(rownames(m), ncol(m)), 
                         a_duration = as.vector(m)) %>%
              tbl_df() %>%
              mutate(
                week_n = formatC(week_n, width=2, flag="0"),
                student_cluster = formatC(cluster, width=1, flag="0")
              ) 
p <- ggplot(clusters_8, aes(cluster,week_n,fill=a_duration))
plot <- p + geom_raster(interpolate=FALSE) + 
  scale_fill_gradient(
    low="white",
    high="#132B43"
  )
plot 


# Per day
simple_sessions_pday <- sessions %>% 
  select(student_id, p_day, activity_duration) %>%
  group_by(student_id, p_day) 

students_pday <- simple_sessions_pday %>% 
  summarise(a_duration = sum(activity_duration)) %>%
  ungroup() %>%
  mutate(p_day = paste('d',formatC(p_day, width=3, flag="0"), sep='_'))

students_pday_smpl <- students_pday %>%
  mutate(student = row_number()) %>%
  select(-student_id)

# Cast it
students_pday_wide <- tbl_df(dcast(students_pday, student_id ~ p_day,
                                    fun.aggregate = sum,
                                    fill= 0,
                                    value.var = "a_duration"))
clusters <- students_pweek_wide %>%
  select(-student_id) %>%
  kmeans(5) 
students_pday_wide_nums <- students_pday_wide %>%
  select(-student_id)

wss <- (nrow(students_pday_wide_nums)-1)*sum(apply(students_pday_wide_nums,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(students_pday_wide_nums,
                                     algorithm="Lloyd",
                                     centers=i, iter.max=10000)$withinss)
wthn_sq_sum <- tbl_df(data_frame(clust_d=c(1:length(wss)),wthn_sq_sum=wss))
qplot(clust_d,wthn_sq_sum,data=wthn_sq_sum, geom="line")

fit_9 <- kmeans(students_pday_wide_nums,
                algorithm="Lloyd",
                centers=9, iter.max=10000)

clust_students_pday <- students_pday_wide %>%
  mutate(clust_9 = fit_9$cluster)

melted_stu_pday <- clust_students_pday %>%
  arrange(clust_9) %>%
  mutate(student_n = c(1:length(clust_students_pday$student_id))) %>%
  select(-clust_9) %>%
  melt(id = c('student_id', 'student_n'),
       variable.name="pday_n",
       value.name="a_duration") %>%
  tbl_df() %>%
  left_join(
    clust_students_pday %>% 
      select(student_id, clust_9)
  ) 



# Density map
p <- ggplot(melted_stu_pday, aes(student_n,pday_n,fill=a_duration))
plot <- p + geom_raster(interpolate=TRUE) # + 
#   scale_fill_gradient(
#     low="white",
#     high="#132B43"
#   )
plot 
for (i in 1:9) {
  plot <- plot + geom_vline(xintercept=(melted_stu_pday %>% filter(clust_9==i) %>% arrange(desc(student_n)) %>% slice(1))$student_n)
}
plot
m <- fit_9$centers
clusters_9 <- data.frame(pday_n = rep(colnames(m), each = nrow(m)), 
                         cluster = rep(rownames(m), ncol(m)), 
                         a_duration = as.vector(m)) %>%
  tbl_df() %>%
  filter(as.numeric(cluster) > 1) %>%
  mutate(
    pday_n = formatC(pday_n, width=3, flag="0"),
    student_cluster = formatC(as.numeric(cluster), width=1, flag="0")
  ) 
p <- ggplot(clusters_9, aes(student_cluster,pday_n,fill=a_duration))
plot <- p + geom_raster(interpolate=FALSE) + 
  scale_fill_gradient(
    low="white",
    high="#132B43"
  )
plot 

m <- fit_9$centers
clusters_9 <- data.frame(pday_n = rep(colnames(m), each = nrow(m)), 
                         cluster = rep(rownames(m), ncol(m)), 
                         a_duration = as.vector(m)) %>%
  tbl_df() %>%
  filter(as.numeric(cluster) > 1) # %>%
#   mutate(
#     pday_n = formatC(pday_n, width=3, flag="0"),
#     student_cluster = formatC(as.numeric(cluster), width=1, flag="0")
#   ) 
p <- ggplot(clusters_9, aes(pday_n,a_duration,colour=cluster))
plot <- p + geom_line()
plot 
