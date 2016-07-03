###################################################################
# UOC - TFG
# Codi corresponent al TFG:
# Patrons de connexió al CV de la UOC 
# Capítol 4
# Armand Adroher Salvia 01/2015
###################################################################


# Discretise time

first_day <- floor_date(first_sess$session_start, unit=c("day")) %>% as.Date()
last_day <- ceiling_date(last_sess$session_expiration, unit=c("day")) %>% as.Date()

semester <- tbl_df(data.frame(
  date = 
    seq(from = first_day,
        to = last_day,
        by = "1 day")))

day_activity <- sessions %>%
  select(
    id,
    student_num,
    session_start,
    last_request) %>%
  mutate(
    start_day = session_start %>% as.Date(),
    end_day = last_request %>% as.Date()
  ) %>%
  select(
    id,
    student_num,
    start_day,
    end_day
  )

distinct_day_activity <- 
  day_activity %>%
  select(-id) %>%
  distinct() 

per_day_activity <- 
  distinct_day_activity %>%
  mutate(days = (end_day - start_day)) 

mult_day_activity <- 
  per_day_activity %>%
  filter(days > 0)


# New set that contains mult_day_activity

new_days <- foreach(i = 1:nrow(mult_day_activity),
                    .combine = 'rbind') %do% {
                      
                      mda <- mult_day_activity %>% 
                        slice(i) 
                      n <- mult_day_activity$days[1] %>% as.numeric(units="days")
                      nd <- tbl_df(data.frame(
                        student_num = rep(mda$student_num[1], n),
                        date = 
                          seq(from = mda$start_day[1],
                              to = mda$end_day[1],
                              by = "1 day"),
                        active = rep(1, n)
                      ))
                      
                      nd
                    }

student_active_days <- 
  per_day_activity %>%
  filter(days == 0) %>%
  mutate(active = 1,
         date = start_day) %>% 
  select(student_num, date, active) %>%
  rbind(new_days) %>%
  arrange(desc(active))

student_days_wd <- 
  student_active_days %>%
  distinct() %>%
  mutate(date = format(date)) %>%
  dcast(student_num ~ date,
        fun.aggregate = sum,
        fill= 0,
        value.var = "active") %>%
  tbl_df() %>%
  left_join(student_ids)

student_days <- student_days_wd %>%
  select(-student_id) %>%
  melt(
    id = "student_num",
    variable.name = "date",
    value.name = "is_active"
  ) %>%
  tbl_df() %>%
  mutate(
    date = date %>% 
      as.character() %>%
      ymd() %>%
      as.Date()
  )

student_weeks <-
  student_days %>%
  mutate(
    p_week = (((date - first_day) %>% as.numeric()) / 7) %>% floor(),
    p_week_date = first_day + p_week * 7
  ) %>%
  group_by(student_num, p_week_date) %>%
  summarise(
    activity = sum(is_active),
    p_week = mean(p_week)
  ) %>%
  ungroup() %>%
  mutate(activity_rate = activity / 7)

student_weeks_wd <- 
  student_weeks %>%
  select(student_num, p_week_date, activity) %>%
  distinct() %>%
  mutate(
    p_week_date = format(p_week_date),
    activity = (activity > 0) * 1
  ) %>%
  dcast(student_num ~ p_week_date,
        fill= 0,
        value.var = "activity") %>%
  tbl_df() %>%
  left_join(student_ids)
