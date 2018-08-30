### SET UP

#cleaning workspace, setting working directory
rm(list=ls())
setwd('~/RWD/clothescribe')     #C:/Users/USER/Documents/RWD/clothescribe

#importing libraries
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)


### NOTE: for the entire script, I aimed to make the code fully generalized (no hardcoded dates or other values) so that the code could be re-run
### easily on future data without further modification. for example, for logical operations to filter the data based on date, the function max() is used
### to call the max date value for logical comparison instead of hardcoding in something like '2018-05-01'.


### IMPORT AND DATA PREP

#importing data
accounts_upgraded <- read_csv('./accounts_upgraded.csv')

#'account_upgraded.csv' comes from the following SQL query:
#
# SELECT a.account_id, IF(max(p.number) > 1, 'YES', 'NO') as upgraded
# FROM account a
#         JOIN subscription s ON a.account_id = s.account_id
#         JOIN payment p ON s.subscription_id = p.subscription_id
# GROUP BY a.account_id

purchases <- read_csv('./purchases_number_1.csv')

#'purchases_number_1' stands for purchases made when payment.number = 1; looking at the first month to find the difference 
# between users who upgraded and users who did not. 'purchases_number_1.csv' comes from the following SQL query:

# SELECT purchases.*, upgraded.upgraded
# 
# FROM
# 
# (SELECT a.account_id, p.date_recorded, pr.*
#                 FROM account a
#         JOIN subscription s ON a.account_id = s.account_id
#         JOIN payment p ON s.subscription_id = p.subscription_id
#         JOIN product_selection ps ON p.payment_id = ps.payment_id
#         JOIN product pr ON ps.product_id = pr.product_id
#         WHERE p.number = 1) as purchases
# 
# JOIN
# 
# (SELECT a.account_id, IF(max(p.number) > 1, 'YES', 'NO') as upgraded
#         FROM account a
#         JOIN subscription s ON a.account_id = s.account_id
#         JOIN payment p ON s.subscription_id = p.subscription_id
#         GROUP BY a.account_id) as upgraded
# 
# ON purchases.account_id = upgraded.account_id

account <- read_csv('./account.csv')    #this is the just the 'account' SQL table as is; 'SELECT * FROM account'

#data formatting; adding whether or not account upgraded to 'account' data and also changing 'NA' acquisition channel to 'None'
account <- inner_join(account, accounts_upgraded, by = 'account_id') %>%
        select(account_id, upgraded, everything()) %>%
        mutate(acquisition_channel = ifelse(is.na(acquisition_channel), 'None', acquisition_channel))

#creating variable for rounding dates to nearest month for plotting and data aggregation purposes
account <- account %>% mutate(year_month = floor_date(trial_start_date, unit = 'month'))
purchases <- purchases %>% mutate(year_month = floor_date(date_recorded, unit = 'month'))

#filtering dates to exclude entries from 2018-05-01 and onwards (the most recent month), since May 2018 is the most recent month of data 
#and we do not yet know which if any of those users upgraded since we do not yet have June 2018 data (including May 2018 data makes the 
#plots look garbage; the upgrade counts slam to zero for that month on all the graphs)
account <- account %>% filter(year_month < max(year_month))
purchases <- purchases %>% filter(year_month < max(year_month))

#joinging acquisition channel onto purchases data
purchases <- inner_join(purchases, account %>% select(account_id, acquisition_channel), by = 'account_id')

### ANALYSIS

#looking at number of upgrades per acquisition channel over time
channel_data <- account %>% 
        group_by(year_month, acquisition_channel) %>% 
        summarize(upgraded = sum(ifelse(upgraded == 'YES', 1, 0)),
                  canceled = n() - upgraded,
                  total = n(),
                  proportion_upgraded = upgraded/total) %>%
        ungroup()

#creating factor levels for acquisition channels to make the ordering of the legend for acquisition channels prettier
channel_levels <- channel_data %>%
        filter(year_month == max(year_month)) %>%
        arrange(desc(upgraded)) %>%
        select(acquisition_channel)
channel_levels <- channel_levels[[1]]

##plotting acquisition channel upgrades over time, in total count per acquisition channel per month
#lifetime
channel_data %>%
        mutate(acquisition_channel = factor(acquisition_channel, levels = channel_levels)) %>%
        ggplot(aes(x = year_month, y = upgraded, color = acquisition_channel)) +
        geom_line(size = 1.1) +
        labs(title = 'Raw Count of Upgrades per Acquisition Channel',
             subtitle = 'Lifetime',
             color = 'Acquisition Channel') +
        xlab('Date (Rounded to Nearest Month)') +
        ylab('Total Upgraded')

#past year
channel_data %>%
        filter(year_month >= max(year_month) - years(1)) %>%
        mutate(acquisition_channel = factor(acquisition_channel, levels = channel_levels)) %>%
        ggplot(aes(x = year_month, y = upgraded, color = acquisition_channel)) +
        geom_line(size = 1.1) +
        labs(title = 'Raw Count of Upgrades per Acquisition Channel',
             subtitle = paste(max(account$year_month) - years(1), 'to', max(account$year_month)),
             color = 'Acquisition Channel') +
        xlab('Date (Rounded to Nearest Month)') +
        ylab('Total Upgraded')

##plotting proportion of users who upgraded per acquisition channel per month for the last year
channel_data %>%
        filter(year_month >= max(year_month) - years(1)) %>%
        mutate(acquisition_channel = factor(acquisition_channel, levels = channel_levels)) %>%
        ggplot(aes(x = year_month, y = proportion_upgraded, color = acquisition_channel)) +
        geom_line(size = 1.1) +
        labs(title = 'Proportion of Users Who Upgraded per Acquisition Channel',
             subtitle = paste(max(account$year_month) - years(1), 'to', max(account$year_month)),
             color = 'Acquisition Channel') +
        xlab('Date (Rounded to Nearest Month)') +
        ylab('Proportion Upgraded')

#table for proportion of users who upgrade per channel along w/ chi-square test of independence between acquisition_channel and upgrade
channel_proportions <- channel_data %>% 
        filter(year_month >= max(year_month) - years(1)) %>% 
        group_by(acquisition_channel) %>% 
        summarize(upgraded = sum(upgraded), 
                  canceled = sum(canceled), 
                  total = sum(total), 
                  proportion_upgraded = upgraded/total) %>% 
        arrange(desc(proportion_upgraded)) %>% 
        ungroup()

account %>% 
        filter(year_month >= max(year_month) - years(1)) %>%
        with(., chisq.test(acquisition_channel, upgraded, correct = FALSE))

#2-proportion hypothesis test for top vs bottom acquisition channels
channel_proportions <- channel_proportions %>% 
        mutate(rank = ifelse(row_number() <= nrow(.)/2, 'top', 'bottom')) 
channel_comparison <- channel_proportions %>%
        group_by(rank) %>%
        summarize(yes = sum(upgraded),
                  no = sum(canceled),
                  total = sum(total),
                  proportion_upgraded = yes/total) %>%
        arrange(desc(proportion_upgraded))
prop.test(channel_comparison$yes, channel_comparison$total, alternative = 'greater')

#channel exploration functions for purchase data
channel_brand <- function(channel = 'None', test = FALSE){
        if(channel == 'All'){
                data <- purchases %>%
                        group_by(brand) %>% 
                        summarize(yes = sum(ifelse(upgraded == 'YES', 1, 0)), 
                                  no = n() - yes, 
                                  total = n(), 
                                  proportion_upgraded = yes/total) %>% 
                        arrange(desc(proportion_upgraded))                
        }else{
                data <- purchases %>%
                        filter(acquisition_channel == channel) %>% 
                        group_by(brand) %>% 
                        summarize(yes = sum(ifelse(upgraded == 'YES', 1, 0)), 
                                  no = n() - yes, 
                                  total = n(), 
                                  proportion_upgraded = yes/total) %>% 
                        arrange(desc(proportion_upgraded))
        }
        print(data)
        if(test == TRUE){
                cat('\n\n')
                n = floor(nrow(data)/2)
                data <- data %>% 
                        mutate(rank = ifelse(row_number() <= n, 'top', 'bottom')) %>%
                        group_by(rank) %>%
                        summarize(yes = sum(yes),
                                  no = sum(no),
                                  total = sum(total),
                                  proportion_upgraded = yes/total) %>%
                        arrange(desc(proportion_upgraded))
                print(data)
                cat('\n\n')
                prop.test(data$yes, data$total, alternative = 'greater')
        }
}

channel_type <- function(channel = 'None', test = FALSE){
        if(channel == 'All'){
                data <- purchases %>%
                        group_by(type) %>% 
                        summarize(yes = sum(ifelse(upgraded == 'YES', 1, 0)), 
                                  no = n() - yes, 
                                  total = n(), 
                                  proportion_upgraded = yes/total) %>% 
                        arrange(desc(proportion_upgraded))
        }else{
                data <- purchases %>%
                        filter(acquisition_channel == channel) %>% 
                        group_by(type) %>% 
                        summarize(yes = sum(ifelse(upgraded == 'YES', 1, 0)), 
                                  no = n() - yes, 
                                  total = n(), 
                                  proportion_upgraded = yes/total) %>% 
                        arrange(desc(proportion_upgraded))
        }
        print(data)
        if(test == TRUE){
                cat('\n\n')
                n = floor(nrow(data)/2)
                data <- data %>% 
                        mutate(rank = ifelse(row_number() <= n, 'top', 'bottom')) %>%
                        group_by(rank) %>%
                        summarize(yes = sum(yes),
                                  no = sum(no),
                                  total = sum(total),
                                  proportion_upgraded = yes/total) %>%
                        arrange(desc(proportion_upgraded))
                print(data)
                cat('\n\n')
                prop.test(data$yes, data$total, alternative = 'greater')
        }
}


#looking at upgrade's relation to rating of each item
purchases %>%
        ggplot(aes(x = avg_user_rating, color = upgraded, fill = upgraded)) +
        geom_density(alpha = 0.5) +
        labs(title = 'Account Upgrades Based On Average Rating of Each Item',
             fill = 'Upgraded',
             color = 'Upgraded') +
        xlab('Average User Rating') +
        ylab('Density')
rating_upgraded <- purchases$avg_user_rating[purchases$upgraded == 'YES']
rating_canceled <- purchases$avg_user_rating[purchases$upgraded == 'NO']
t.test(rating_upgraded, rating_canceled)



## looking at 'account' variables state, gender, and age

##state
#chi-square test of independence between state and upgrade
account %>% 
        filter(year_month >= max(year_month) - years(1)) %>%
        with(., chisq.test(state, upgraded, correct = FALSE))

state_rank <- account %>% 
        filter(year_month >= max(year_month) - years(1)) %>%
        group_by(state) %>% 
        summarize(yes = sum(ifelse(upgraded == 'YES', 1, 0)), 
                  no = n() - yes, 
                  total = n(), 
                  proportion_upgraded = yes/total) %>% 
        arrange(desc(proportion_upgraded)) %>%
        ungroup()
states <- unique(state_rank$state)
state_rank <- state_rank %>% filter(yes >= 10 & no >= 10)
states_excluded <- states[!(states %in% state_rank$state)]

#state comparison function to compare top n states to bottom n states
library(magrittr)
state_comp <- function(n = 5){
        if(n > nrow(state_rank)/2){
                n <- floor(nrow(state_rank)/2)
                warning(paste('n > nrow(state_rank)/2; setting n = floor(nrow(state_rank)/2) = ', n, sep = ''))
        }
        top_states <- head(state_rank, n) %>% mutate(rank = 'top')
        bottom_states <- tail(state_rank, n) %>% mutate(rank = 'bottom')
        both_states <- bind_rows(top_states, bottom_states) %T>% 
                print() %>%
                group_by(rank) %>% 
                summarize(yes = sum(yes), 
                          no = sum(no), 
                          total = sum(total),
                          proportion_upgraded = yes/total) %>%
                arrange(desc(proportion_upgraded))
        cat('\n\n')
        print(both_states)
        cat('\n\n')
        prop.test(both_states$yes, both_states$total)
}

##gender
#table for proportion of users who upgrade based on gender along w/ chi-square test of independence between gender and upgrade
account %>%  
        filter(year_month >= max(year_month) - years(1)) %>%
        group_by(gender) %>% 
        summarize(yes = sum(ifelse(upgraded == 'YES', 1, 0)), 
                  no = n() - yes, 
                  total = n(), 
                  proportion_upgraded = yes/total)

account %>%  
        filter(year_month >= max(year_month) - years(1)) %>%
        with(., chisq.test(gender, upgraded, correct = FALSE))

##age
#exploration of age
account %>% 
        ggplot(aes(x = age, fill = upgraded, color = upgraded)) + 
        geom_density(alpha = 0.5) +
        labs(title = 'Account Upgrades Based On Age and Gender',
             fill = 'Upgraded',
             color = 'Upgraded') +
        xlab('Age') +
        ylab('Density')

#2-sample t-test to compare mean age for those who upgraded vs those who did not
age_upgraded <- account$age[account$upgraded == 'YES']
age_canceled <- account$age[account$upgraded == 'NO']
t.test(age_upgraded, age_canceled)