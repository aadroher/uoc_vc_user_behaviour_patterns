###################################################################
# UOC - TFG
# Codi corresponent al TFG:
# Patrons de connexió al CV de la UOC 
# Capítols 2 i 3
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


# constants
data_dir <- "../src/papers/memoir/data/"
img_dir <- "../src/papers/memoir/img/"
inch2cm <- 0.393701




###################################
# Capítol 1
###################################
con <- dbConnect(MySQL())
raw_data <- dbReadTable(con, "sessions")
prev_sess <- tbl_df(raw_data)

# Convert data types to useful ones
prev_sess$student_id <- sprintf("%013s",prev_sess$student_id)
prev_sess$session_start <- ymd_hms(prev_sess$session_start)
prev_sess$last_request <- ymd_hms(prev_sess$last_request)
prev_sess$session_expiration <- ymd_hms(prev_sess$session_expiration)

# Derive relevant attrs

first_sess <- prev_sess %>%
  arrange(session_start) %>%
  slice(1)
last_sess <- prev_sess %>%
  arrange(desc(session_expiration)) %>%
  slice(1)

sessions <- prev_sess %>%
  mutate(student_num = dense_rank(student_id)) %>%
  mutate(total_duration = 
           as.numeric(session_start %--% session_expiration)) %>%
  mutate(activity_duration = 
           as.numeric(session_start %--% last_request)) %>%
  mutate(inactivity_duration = 
           as.numeric(last_request %--% session_expiration)) %>%
  mutate(central_activity_point = 
           session_start + activity_duration / 2) %>%
  filter(activity_duration >= 0
         & inactivity_duration >= 0) %>%
  mutate(session_start_pday = as.numeric(difftime(
    as.Date(session_start), 
    as.Date(first_sess$session_start), unit='days'))) %>%
  mutate(session_start_wday = (wday(session_start) - 1 + 6 ) %% 7) %>%
  mutate(central_activity_point_pday = as.numeric(difftime(
    as.Date(central_activity_point), 
    as.Date(first_sess$session_start +
              as.numeric(first_sess$session_start %--% 
                           first_sess$last_request) / 2), unit='days'))) %>%
  mutate(central_activity_point_day = (wday(central_activity_point) - 1 + 6 ) %% 7)

# Dimensions
n_students <-
  sessions %>%
  select(student_id) %>%
  distinct() %>%
  summarise(n_students = n())

percentiles <- function(data, qs) {
  return(data %>% quantile(qs, na.rm = TRUE)) 
}

qs <- c(.01,.05, .10, .25, .50, .75, .90, .95, .99)
sessions_desc <- data.frame(percentiles = qs * 100,
                            n = floor(nrow(sessions) * qs),
                            #                             not_n = floor(nrow(sessions) * (1 - qs)),
                            activity_duration = sessions$activity_duration %>% percentiles(qs),
                            inactivity_duration = sessions$inactivity_duration %>% percentiles(qs)) %>%
  tbl_df()

write.table(sessions_desc, 
            file=paste(data_dir,"sessions_percentiles.csv"),
            row.names=FALSE,
            col.names=FALSE,
            quote=FALSE,
            sep=" & ",
            eol=" \\\\ \n")

# Get the mode
a_du_mode <- 
  sessions %>%
  select(activity_duration) %>%
  group_by(activity_duration) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1)

# Plot the distribution of activity_duration

m <- sessions %>% 
  ggplot()
p <- m +
  aes(x = activity_duration) %>%
  geom_line(stat="density") +
  xlab(expression(activity_duration)) + 
  ylab(expression(p(activity_duration))) +
  ggtitle("sessions") +
  theme_grey(8)

ggsave(p, filename = "activity_duration_a_dens.pdf",
       path = img_dir,
       units = "cm",
       width= 8,
       height= 6,
) 



# Trim the sessions to view the distributions of the central values.
trim_sessions <-
  sessions %>%
  filter(
    activity_duration > 60
    & activity_duration < (sessions$activity_duration %>% percentiles(.95))
  )

# Get the mode
trim_a_du_mode <- 
  trim_sessions %>%
  select(activity_duration) %>%
  group_by(activity_duration) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1)


m <- trim_sessions %>% 
  ggplot()
p <- m +
  aes(x = activity_duration) %>%
  geom_line(stat="density") +
  xlab(expression(activity_duration)) + 
  ylab(expression(p(activity_duration))) +
  ggtitle("trim_sessions") +
  theme_grey(8)


ggsave(p, filename = "trim_activity_duration_dens.pdf",
       path = img_dir,
       units = "cm",
       width= 8,
       height= 6,
) 


# Plot the distribution of inactivity_duration

m <- sessions %>% 
  ggplot()
p <- m +
  aes(x = inactivity_duration) %>%
  geom_line(stat="density") +
  xlab(expression(inactivity_duration)) + 
  ylab(expression(p(inactivity_duration))) +
  ggtitle("sessions") +
  theme_grey(8)

ggsave(p, filename = "inactivity_duration_a_dens.pdf",
       path = img_dir,
       units = "cm",
       width= 8,
       height= 6,
) 

# Get the mean of inactivity_duration > 0
ina_du_mean <- 
  sessions %>%
  select(inactivity_duration) %>%
  filter(inactivity_duration > 0) %>%
  arrange(inactivity_duration) %>%
  summarise(m_inactivity_duration = mean(inactivity_duration))

######
# Estudis per cicle temporal

# Dia
sess_by_hour <- 
  sessions %>%
  mutate(
    session_start_h = session_start %>% hour(),
    last_request_h = last_request %>% hour(),
    session_expiration_h = session_expiration %>% hour(),
    central_activity_point_h = central_activity_point %>% hour(),
    central_activity_point_wday = (wday(central_activity_point) - 1 + 6 ) %% 7
  ) %>%
  select(
    id,
    student_id,
    session_start_h,
    last_request_h,
    session_expiration_h,
    central_activity_point_h,
    central_activity_point_wday,
    total_duration,
    activity_duration,
    inactivity_duration
  )

write.table(sess_by_hour, 
            file=paste(data_dir,"session_days.csv"),
            row.names=FALSE,
            col.names=FALSE,
            quote=FALSE,
            sep=" & ",
            eol=" \\\\ \n")

m <- sess_by_hour %>% 
  ggplot()
p <- m +
  aes(x = factor(central_activity_point_h)) %>%
  geom_bar(binwidth = 1) +
  ylab(expression(n)) +
  xlab(expression(hour(central_activity_point))) +
  scale_x_discrete(
    breaks = NULL
  ) +
  theme_grey(8)

ggsave(p, filename = "c_a_point_h_bar.pdf",
       path = img_dir,
       units = "cm",
       width= 5,
       height= 4,
) 

sess_by_hour_lab_days <-
  sess_by_hour %>%
  filter(not(central_activity_point_wday %in% c(5,6)))

m <- sess_by_hour_lab_days %>% 
  ggplot()
p <- m +
  aes(x = factor(central_activity_point_h)) %>%
  geom_bar(binwidth = 1) +
  ggtitle("Entre setmana") +
  ylab(expression(n)) +
  xlab(expression(hour(central_activity_point))) +
  scale_x_discrete(
    breaks = NULL
  ) +
  theme_grey(8)

ggsave(p, filename = "c_a_point_h_lab_days_bar.pdf",
       path = img_dir,
       units = "cm",
       width= 5,
       height= 4,
) 

sess_by_hour_weekend <-
  sess_by_hour %>%
  setdiff(sess_by_hour_lab_days)

m <- sess_by_hour_weekend %>% 
  ggplot()
p <- m +
  aes(x = factor(central_activity_point_h)) %>%
  geom_bar(binwidth = 1) +
  ggtitle("Cap de setmana") +
  ylab(expression(n)) +
  xlab(expression(hour(central_activity_point))) +
  scale_x_discrete(
    breaks = NULL
  ) +
  theme_grey(8)

ggsave(p, filename = "c_a_point_h_weekend_bar.pdf",
       path = img_dir,
       units = "cm",
       width= 5,
       height= 4,
)

users_by_hour <-
  sess_by_hour %>%
  select(student_id, central_activity_point_h) %>%
  distinct()

m <- users_by_hour %>% 
  ggplot()
p <- m +
  aes(x = factor(central_activity_point_h)) %>%
  geom_bar(binwidth = 1) +
  ylab(expression(unique(user_id))) +
  xlab(expression(hour(central_activity_point))) +
  scale_x_discrete(
    breaks = NULL
  ) + 
  theme_grey(8)

ggsave(p, filename = "users_by_hour_bar.pdf",
       path = img_dir,
       units = "cm",
       width= 5,
       height= 4,
)

# Build scatter/line plots on this.

student_session_hour <- 
  sess_by_hour %>%
  group_by(central_activity_point_h) %>%
  summarise(n_sessions = n()) %>%
  ungroup() %>%
  mutate(sessions_rate = (n_sessions / max(n_sessions) * 100)) %>%
  left_join(
    sess_by_hour %>%
      select(student_id, central_activity_point_h) %>%
      distinct() %>%
      group_by(central_activity_point_h) %>%
      summarise(students = n()) %>%
      ungroup() %>%
      mutate(students_rate = (students / max(students)) * 100)
  ) %>%
  mutate(sessions_per_student = (n_sessions / students) * 100) %>%
  mutate(sessions_per_student_rate = (sessions_rate / students_rate) * 100) %>%
  select(
    central_activity_point_h,
    sessions_rate,
    students_rate
  ) %>%
  melt(
    id.vars = c("central_activity_point_h")
  ) %>% 
  tbl_df() %>%
  group_by(variable)


m <- student_session_hour %>%
  ggplot(aes(x = central_activity_point_h,
             y = value,
             colour = variable))
p <- 
  m + 
  geom_point() +
  geom_line() +
  scale_x_continuous(
    breaks = 0:23
  ) +
  theme_grey(8) +
  ylab("%") +
  xlab(expression(hour(central_activity_point))) +
  scale_color_brewer(
                type="qual",
                name="variable",
                     labels = 
                       c("n_sessions", "n_users"))

ggsave(p, filename = "student_session_hour_scatter.pdf",
       path = img_dir,
       units = "cm",
       width= 12,
       height= 6,
) 

student_session_hour_labdays <-
  sess_by_hour %>%
  filter(not(central_activity_point_wday %in% c(5,6))) %>%
  group_by(central_activity_point_h) %>%
  summarise(n_sessions = n()) %>%
  ungroup() %>%
  mutate(sessions_rate = (n_sessions / max(n_sessions) * 100)) %>%
  left_join(
    sess_by_hour %>%
      select(student_id, central_activity_point_h) %>%
      distinct() %>%
      group_by(central_activity_point_h) %>%
      summarise(students = n()) %>%
      ungroup() %>%
      mutate(students_rate = (students / max(students)) * 100)
  ) %>%
  mutate(sessions_per_student = (n_sessions / students) * 100) %>%
  mutate(sessions_per_student_rate = (sessions_rate / students_rate) * 100) %>%
  select(
    central_activity_point_h,
    sessions_rate,
    students_rate
  ) %>%
  melt(
    id.vars = c("central_activity_point_h")
  ) %>% 
  tbl_df() %>%
  group_by(variable)

m <- student_session_hour_labdays %>%
  ggplot(aes(x = central_activity_point_h,
             y = value,
             colour = variable))
p <- 
  m + 
  geom_line() +
  scale_x_continuous(
    breaks = NULL
  ) +
  ggtitle("Dies laborables") +
  theme_grey(8) +
  theme( legend.position = "bottom") +
  ylab("%") +
  xlab(expression(hour(central_activity_point))) +
  scale_color_brewer(type="qual",
                    name=NULL,
                       labels = 
                         c("n_sessions", "n_users"))

ggsave(p, filename = "student_session_hour_labdays.pdf",
       path = img_dir,
       units = "cm",
       width= 5,
       height= 6,
) 

student_session_hour_weekend <-
  sess_by_hour %>%
  filter((central_activity_point_wday %in% c(5,6))) %>%
  group_by(central_activity_point_h) %>%
  summarise(n_sessions = n()) %>%
  ungroup() %>%
  mutate(sessions_rate = (n_sessions / max(n_sessions) * 100)) %>%
  left_join(
    sess_by_hour %>%
      select(student_id, central_activity_point_h) %>%
      distinct() %>%
      group_by(central_activity_point_h) %>%
      summarise(students = n()) %>%
      ungroup() %>%
      mutate(students_rate = (students / max(students)) * 100)
  ) %>%
  mutate(sessions_per_student = (n_sessions / students) * 100) %>%
  mutate(sessions_per_student_rate = (sessions_rate / students_rate) * 100) %>%
  select(
    central_activity_point_h,
    sessions_rate,
    students_rate
  ) %>%
  melt(
    id.vars = c("central_activity_point_h")
  ) %>% 
  tbl_df() %>%
  group_by(variable)

m <- student_session_hour_weekend %>%
  ggplot(aes(x = central_activity_point_h,
             y = value,
             colour = variable))
p <- 
  m + 
  geom_line() +
  scale_x_continuous(
    breaks = NULL
  ) +
  ggtitle("Cap de setmana") +
  theme_grey(8) +
  theme( legend.position = "bottom") +
  ylab("%") +
  xlab(expression(hour(central_activity_point))) +
  scale_color_brewer( type="qual",
                      name=NULL,
                       labels = 
                         c("n_sessions", "n_users"))

ggsave(p, filename = "student_session_hour_weekend.pdf",
       path = img_dir,
       units = "cm",
       width= 5,
       height= 6,
) 

# Week

sessions_by_wday <- 
  sessions %>%
  select(
    id,
    student_id, 
    central_activity_point_day
  )

write.table(sessions_by_wday, 
            file=paste(data_dir,"session_weeks.csv"),
            row.names=FALSE,
            col.names=FALSE,
            quote=FALSE,
            sep=" & ",
            eol=" \\\\ \n")

m <- sessions %>% 
  ggplot()
p <- m +
  aes(x = factor(central_activity_point_day)) %>%
  geom_bar(binwidth = 1) +
  ylab(expression(n)) +
  xlab(expression(wday(central_activity_point))) + 
  scale_x_discrete(
    labels = c("dl", "dm", "dx", "dj", "dv", "ds", "dg") 
  ) +
  theme_grey(8)

ggsave(p, filename = "sessions_wday_bar.pdf",
       path = img_dir,
       units = "cm",
       width= 5,
       height= 4,
) 

users_by_wday <- 
  sessions_by_wday %>%
  select(student_id, central_activity_point_day) %>%
  distinct()

m <- users_by_wday %>% 
  ggplot()
p <- m +
  aes(x = factor(central_activity_point_day)) %>%
  geom_bar(binwidth = 1) +
  ylab(expression(unique(user_id))) +
  xlab(expression(wday(central_activity_point))) +
  scale_x_discrete(
    labels = c("dl", "dm", "dx", "dj", "dv", "ds", "dg") 
  ) +
  theme_grey(8)

ggsave(p, filename = "users_wday_bar.pdf",
       path = img_dir,
       units = "cm",
       width= 5,
       height= 4,
) 

# Build scatter plots on this.


student_session_wday <- 
  sessions_by_wday %>%
  group_by(central_activity_point_day) %>%
  summarise(n_sessions = n()) %>%
  ungroup() %>%
  mutate(sessions_rate = (n_sessions / max(n_sessions) * 100)) %>%
  left_join(
    sessions_by_wday %>%
      select(student_id, central_activity_point_day) %>%
      distinct() %>%
      group_by(central_activity_point_day) %>%
      summarise(students = n()) %>%
      ungroup() %>%
      mutate(students_rate = (students / max(students)) * 100)
  ) %>%
  mutate(sessions_per_student = (n_sessions / students) * 100) %>%
  mutate(sessions_per_student_rate = (sessions_rate / students_rate) * 100) %>%
  select(
    central_activity_point_day,
    sessions_rate,
    students_rate
  ) %>%
  melt(
    id.vars = c("central_activity_point_day")
  ) %>% 
  tbl_df() %>%
  group_by(variable)


m <- student_session_wday %>%
  ggplot(aes(x = central_activity_point_day,
             y = value,
             colour = variable))
p <- 
  m + 
  geom_point() +
  geom_line() +
  scale_x_continuous(
    breaks = 0:6,
    labels = c("dl", "dm", "dx", "dj", "dv", "ds", "dg") 
  ) +
  theme_grey(8) +
  ylim(0,100) +
  ylab("%") +
  xlab(expression(wday(central_activity_point))) +
  scale_color_brewer(name=NULL,
                       labels = 
                         c("n_sessions", "n_users"),
                     type="qual")

ggsave(p, filename = "student_session_wday_scatter.pdf",
       path = img_dir,
       units = "cm",
       width= 10,
       height= 7,
) 



# Semester

n_sessions_p_day <-
  sessions %>%
  select(id,student_id, central_activity_point) %>%
  mutate(central_activity_point = central_activity_point %>% as.Date()) %>%
  group_by(central_activity_point) %>%
  summarise(n_sessions = n())

m <- n_sessions_p_day %>%
  ggplot()
p <- m +
  geom_line(aes(x = central_activity_point, 
                y = n_sessions),
            colour = "grey") +
  geom_point(aes(x = central_activity_point, 
                 y = n_sessions, 
                 colour = (wday(central_activity_point) - 1 + 6 ) %% 7 %>% factor())) +
  geom_smooth(aes(x = central_activity_point, 
                  y = n_sessions),
              method = "loess") +
  scale_color_brewer(type="qual",
                      name="Dia setmana",
                       labels = c("dl", "dm", "dx", "dj", "dv", "ds", "dg") ) +
  ylab(expression(n)) +
  xlab(expression(date(central_activity_point))) + 
  theme_grey(8)

ggsave(p, filename = "n_sessions_pday_scatter.pdf",
       path = img_dir,
       units = "cm",
       width= 13,
       height= 10,
)

n_users_p_day <-
  sessions %>%
  select(student_id, central_activity_point) %>%
  mutate(central_activity_point = central_activity_point %>% as.Date()) %>%
  distinct() %>%
  group_by(central_activity_point) %>%
  summarise( n_users = n())

m <- n_users_p_day %>%
  ggplot()
p <- m +
  geom_line(aes(x = central_activity_point, 
                y = n_users),
            colour = "grey") +
  geom_point(aes(x = central_activity_point, 
                 y = n_users, 
                 colour = (wday(central_activity_point) - 1 + 6 ) %% 7 %>% factor())) +
  geom_smooth(aes(x = central_activity_point, 
                  y = n_users),
              method = "loess") +
  scale_color_brewer(type="qual",
                        name="Dia setmana",
                       labels = c("dl", "dm", "dx", "dj", "dv", "ds", "dg") ) + 
  ylab(expression(unique(user_id))) +
  xlab(expression(date(central_activity_point))) + 
  theme_grey(8)

ggsave(p, filename = "n_users_pday_scatter.pdf",
       path = img_dir,
       units = "cm",
       width= 13,
       height= 10,
)

sessions_per_user_pday <- 
  n_sessions_p_day %>%
  left_join(n_users_p_day) %>%
  mutate(sessions_per_user = n_sessions / n_users)

write.table(sessions_per_user_pday, 
            file=paste(data_dir,"session_semester.csv"),
            row.names=FALSE,
            col.names=FALSE,
            quote=FALSE,
            sep=" & ",
            eol=" \\\\ \n")

m <- sessions_per_user_pday %>%
  ggplot()
p <- m +
  geom_line(aes(x = central_activity_point, 
                y = sessions_per_user),
            colour = "grey") +
  geom_point(aes(x = central_activity_point, 
                 y = sessions_per_user,
                 colour = (wday(central_activity_point) - 1 + 6 ) %% 7 %>% factor()  )) +
  geom_smooth(aes(x = central_activity_point, 
                  y = sessions_per_user),
              method = "loess") + 
  scale_color_brewer(type="qual",
                        name="Dia setmana",
                       labels = c("dl", "dm", "dx", "dj", "dv", "ds", "dg") ) + 
  ylab(expression(n / unique(user_id))) +
  xlab(expression(date(central_activity_point))) + 
  theme_grey(8)

ggsave(p, filename = "sessions_per_user_pday_scatter.pdf",
       path = img_dir,
       units = "cm",
       width= 13,
       height= 10,
)

# Students
students <- 
  sessions %>%
  group_by(student_num, student_id) %>%
  summarise(
    n_sessions = n(),
    sum_activity_duration = sum(activity_duration),
    duration_per_session = sum_activity_duration / n_sessions,
    m_activity_duration = mean(activity_duration),
    sd_activity_duration = sd(activity_duration),
    m_wday = mean(central_activity_point_day),
    m_hour = mean(hour(central_activity_point)),
    first_session = min(central_activity_point),
    q1_cap = percentiles(central_activity_point, c(.25)),
    q2_cap = percentiles(central_activity_point, c(.50)),
    q3_cap = percentiles(central_activity_point, c(.75)),
    last_session = max(central_activity_point),
    iqr_cap = (q3_cap - q2_cap) %>% as.numeric()
  ) %>% 
  ungroup()

write.table(students, 
            file=paste(data_dir,"users.csv"),
            row.names=FALSE,
            col.names=FALSE,
            quote=FALSE,
            sep=" & ",
            eol=" \\\\ \n")


# Values distribution 

users_desc <- data.frame(   percentiles = qs * 100,
                            n = floor(nrow(students) * qs),
                            n_sessions = students$n_sessions %>% percentiles(qs),
                            sum_activity_duration = students$sum_activity_duration %>% percentiles(qs),
                            m_activity_duration = students$m_activity_duration %>% percentiles(qs),
                            m_wday = students$m_wday %>% percentiles(qs),
                            m_hour = students$m_hour %>% percentiles(qs),
                            first_session = students$first_session %>% percentiles(qs),
                            q1_cap = students$q1_cap %>% percentiles(qs),
                            q2_cap = students$q2_cap %>% percentiles(qs),
                            q3_cap = students$q3_cap %>% percentiles(qs),
                            last_session = students$last_session %>% percentiles(qs),
                            iqr_session_start = students$iqr_cap %>% percentiles(qs)) %>%
              tbl_df()

write.table(users_desc %>% select(1:7) %>% format(digits = 3,
                                                  scientific=FALSE), 
            file=paste(data_dir,"users_percentiles_1.csv"),
            row.names=FALSE,
            col.names=FALSE,
            quote=FALSE,
            sep=" & ",
            eol=" \\\\ \n")



m <- students %>%
  ggplot()

p <- 
  m +
  geom_line(aes(x = n_sessions), stat="density") +
  scale_x_log10(breaks = c(1,10,100,1000,10000,100000)) +
  theme_grey(8) +
  xlab(expression(n_sess)) +
  ylab(expression(p(n_sess)))

ggsave(p, filename = "user_n_sessions_density_line_log.pdf",
       path = img_dir,
       units = "cm",
       width= 8,
       height= 6,
)

m <- students %>%
  ggplot()

p <- 
  m +
  geom_line(aes(x = sum_activity_duration), stat="density") +
  scale_x_log10(breaks = c(1,10,100,1000,10000,100000, 1000000, 10000000)) +
  theme_grey(8) +
  xlab(expression(sum_a_du)) +
  ylab(expression(p(sum_a_du)))

ggsave(p, filename = "user_sum_activity_duration_density_line_log.pdf",
       path = img_dir,
       units = "cm",
       width= 8,
       height= 6,
)

m <- students %>%
  ggplot()

p <- 
  m +
  geom_line(aes(x = sum_activity_duration), stat="density") +
  scale_x_continuous(labels = NULL) + 
  xlab(expression(sum_a_du)) +
  ylab(expression(p(sum_a_du))) + 
  theme_grey(8)

ggsave(p, filename = "user_sum_activity_duration_density_line.pdf",
       path = img_dir,
       units = "cm",
       width= 5,
       height= 4,
)

m <- students %>%
  ggplot()

p <- 
  m +
  geom_line(aes(x = iqr_rank_session_start), stat="density") 

m <- students %>%
  ggplot()

p <- 
  m +
  geom_line(aes(x = duration_per_session), stat="density")


m <- students %>%
  ggplot(aes(
    y = first_session,
    x = last_session))

p <-
  m +
  geom_point(alpha = .05, shape = 20) +
  theme_grey(8) +
  xlab(expression(l_sess)) +
  ylab(expression(f_sess))

ggsave(p, filename = "user_first_last_scatter.png",
       path = img_dir,
       units = "cm",
       width= 14,
       height= 14,
)

m <- students %>%
  ggplot(aes(
    y = n_sessions,
    x = last_session))

p <-
  m +
  geom_point(alpha = .05, shape = 20) +
  theme_grey(8) +
  scale_y_log10()

ggsave(p, filename = "user_n_sessions_last_scatter_log.pdf",
       path = img_dir,
       units = "cm",
       width= 10,
       height= 10,
)

m <- students %>%
  ggplot(aes(
    y = sum_activity_duration,
    x = last_session))

p <-
  m +
  geom_point(alpha = .05, shape = 20) +
  theme_grey(8) +
  scale_y_log10(breaks = )

ggsave(p, filename = "user_sum_ad_last_scatter_log.pdf",
       path = img_dir,
       units = "cm",
       width= 10,
       height= 8,
)
