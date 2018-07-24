# install packages
# install.packages("data.table")
# install.packages("zoo")
# install.packages("ggplot2")

### clear environment ###
rm(list = ls())

### library ###
library(data.table)
library(zoo)
library(ggplot2)
options(scipen=999)

### load data ###
in_member_data <- as.data.table(read.csv(file = "Downloads/DA_Project_Member_Data.csv"))
in_revenue_data <- as.data.table(read.csv(file = "Downloads/DA_Project_Revenue_Data.csv"))

  # make working copies
  member_data  <- copy(in_member_data)
  revenue_data <- copy(in_revenue_data)

### clean data ###

  # fix colnames
  colnames(member_data)  <- gsub("\\.", "_",tolower(colnames(member_data)))
  colnames(revenue_data) <- gsub("\\.", "_",tolower(colnames(revenue_data)))  
  colnames(revenue_data) <- gsub("__", "_",colnames(revenue_data))
  
  # fix dates
  member_data[, signed_up_date := as.Date(signed_up_date, "%m/%d/%y")]
  member_data[, cancellation_date := as.Date(cancellation_date, "%m/%d/%y")]
  revenue_data[, new_invoice_date := as.yearmon(format(as.Date(invoice_invoice_date, "%m/%d/%y"), "%Y-%m"))]
  
  # check for duplicates in member data #qc
  setkeyv(member_data, "member_id")
  out_dups <- subset(member_data, duplicated(member_data, by = key(member_data)) | 
                                  duplicated(member_data, by = key(member_data), fromLast=TRUE))
  
### curiosity checks ###
    
  # create tables for curiosity
  insutries <- member_data[, list(count = .N), by = "industry"]
  states <- member_data[, list(count = .N), by = "billing_state_province"]
  sign_up_dates <- member_data[, list(count = .N), by = "signed_up_date"]
  
  # 45.54% of employees getting snack boxes are in the tech field 
  # next closest is 7.81% in consulting
  emp_counts <- member_data[ !is.na(industry), .(emp_count = sum(employees, na.rm = TRUE)), by = "industry"]
  emp_counts[, pct_of_total := round((emp_count/sum(emp_count, na.rm = TRUE)) *100, 2)]
  
  # 39.21% in california, next 13.03% Washington
  emp_counts_state <- member_data[ !is.na(billing_state_province), 
                                   .(emp_count = sum(employees, na.rm = TRUE)), by = "billing_state_province"]
  emp_counts_state[, pct_of_total := round((emp_count/sum(emp_count, na.rm = TRUE)) *100, 2)]
  
### calculate churn ###  
  
  # sum revenue totals by id, product and month
  monthly_sums <- revenue_data[ , .(sum = sum(invoice_item_charge_amount, na.rm = TRUE)), 
                               by = c("member_id", "product_name", "new_invoice_date")]
  
  # find earliest invoice date and latest invoice date
  monthly_sums[, ":=" (first_invoice_month = min(new_invoice_date), 
                       last_invoice_month = max(new_invoice_date)), 
               by = c("member_id", "product_name")]
  
  # create flag if date is the first invoice date
  monthly_sums[, first_record := ifelse(new_invoice_date == first_invoice_month, 1, 0)]
  
  # merge on member data to get more insights
  sum_monthly_info <- merge(monthly_sums, member_data, "member_id")
  
  monthly_start <- sum_monthly_info[ last_invoice_month != "Jul 2018" & first_record == 0,
                              .(start_member = .N, start_revenue = sum(sum)),
                              by = c("last_invoice_month")]
  
  # subset to find only the latest invoice date (this is the churn)
  max_month <- subset(sum_monthly_info, !is.na(industry) & last_invoice_month == new_invoice_date, 
                           select = -c(first_invoice_month, new_invoice_date))
 
  ### overall ###
  
  # find overall churn over all industries and states
  monthly_churn <- max_month[ last_invoice_month != "Jul 2018" & first_record == 0,
                            .(member_churn = .N, revenue_churn = sum(sum)),
                           by = c("last_invoice_month")]
  
  melted_monthly_churn <- melt(monthly_churn, "last_invoice_month")
  
  # merge start with monthly churn to find churn rate
  start_churn <- merge(monthly_start, monthly_churn, "last_invoice_month")
  
  # find revenue and member churn rates
  churn_rates <- start_churn[, .(member_churn_rate = (member_churn/start_member) * 100,
                                 revenue_churn_rate = (revenue_churn/start_revenue) * 100), by = "last_invoice_month"]
  
  melted_churn_rates <- melt(churn_rates, "last_invoice_month")
  
  ### by industry ###
  
  # find churn by industry and month (not including july 2018 as it's not finished yet)
  monthly_churn_industry <- max_month[ last_invoice_month != "Jul 2018", 
                              .(member_churn = .N, revenue_churn = sum(sum)),
                              by = c("industry", "last_invoice_month")]
  
  # find churn by industry to find best/worst performing industries
  industry_total_churn <- monthly_churn_industry[, .(total_churn_rev = sum(revenue_churn),
                                                     total_churn_mem = sum(member_churn),
                                                     churn_per_member = sum(revenue_churn)/sum(member_churn)), by = "industry"]

  industry_avgs <- industry_total_churn[, .(avg_rev_churn = mean(total_churn_rev),
                                            avg_mem_churn = mean(total_churn_mem),
                                            avg_rev_per_mem_churn = mean(churn_per_member),
                                            sd_rev_per_mem_churn = sd(churn_per_member))]
  
  
  
### plotting churn ###  
  
  ### overall plots ###
  
  # divide revenue churn by 100 so it can fit on the same chart with member churn
  melted_monthly_churn[ variable =="revenue_churn", value := value/100]
  
  mean_monthly_rev_churn <- mean(monthly_churn$revenue_churn)/100
  
  # plotting overall churn across all industries
  ggplotly(ggplot(melted_monthly_churn, aes(last_invoice_month, y= value, colour=variable)) +
    geom_line() + 
    geom_point() + 
    geom_text(x=2016, y=2500, label= "*Revenue Measured in Hundreds", size = 4, color = "black") +
    geom_text(x=2015.8, y=650, label= paste0("Revenue Churn Avg: ", round(mean_monthly_rev_churn, 2)), size = 3, color = "#1BC596") +
    geom_text(x=2015.8, y=300, label= paste0("Member Churn Avg: ", round(mean(monthly_churn$member_churn), 2)), size = 3, color = "#F1673B") +
    ggtitle("Monthly Net Revenue* and Member Churn") + ylab("# of Members/Revenue in Hundreds") +
    theme(legend.title=element_blank()) +
    geom_hline(aes(yintercept = mean_monthly_rev_churn)) +
    geom_hline(aes(yintercept = mean(monthly_churn$member_churn))) +
      scale_color_manual(values=c( "#F1673B",  "#1BC596")))
  
  # plot overall churn rates
  ggplotly(ggplot(melted_churn_rates, aes(last_invoice_month, y= value, color=variable)) +
    geom_line() + 
    geom_point() + 
    geom_hline(aes(yintercept = round(mean(churn_rates$revenue_churn_rate), 2))) +
    geom_hline(aes(yintercept = round(mean(churn_rates$member_churn_rate), 2))) +
    ggtitle("Revenue and Member Churn Rates") + ylab("Percentage Churn") +
    scale_color_manual(values=c( "#F1673B",  "#1BC596")) +
    geom_text(x=2018, y=22, label= paste0("Revenue Churn Rate Avg: ", round(mean(churn_rates$revenue_churn_rate), 2)), size = 4, color = "#1BC596") +
    geom_text(x=2018, y=29, label= paste0("Member Churn Rate Avg: ", round(mean(churn_rates$member_churn_rate), 2)), size = 4, color = "#F1673B"))
  
  ### by industry plots ###
  
  # plot churn by industry (need to narrow this to largest/smallest churn)
  ggplotly(ggplot(monthly_churn_industry, aes(last_invoice_month, revenue_churn, group = industry, color = industry)) + 
    geom_line() +
    geom_point() +
    ggtitle("Revenue Churn by Industry") + xlab("") + ylab("Revenue"))
  #1BC596, #F1673B
  
  # specify fill for total industry churn graph
  industry_total_churn$fill <- ifelse(industry_total_churn$churn_per_member > industry_avgs$avg_rev_per_mem_churn, 
                                      "#F1673B", "#1BC596")
  
  # plot total industry churn per member
  ggplotly(ggplot(industry_total_churn, aes(industry, churn_per_member, fill = fill)) + 
             geom_bar(stat="identity") + 
             coord_flip() +
             scale_fill_identity() +
             geom_hline(aes(yintercept = industry_avgs$avg_rev_per_mem_churn))+
             theme(legend.position="none") +
             ggtitle("Revenue Churn Per Member") + xlab("Industry") + ylab("Revenue") +
             geom_text(x=30, y=450, label=paste0("Average: ", round(industry_avgs$avg_rev_per_mem_churn, 2)), size = 5))
           
  
  
### finding months enrolled ###  
  
  # find difference between first and last monthinvoice
  enrollment_time <- sums[, .(months_enrolled = (last_invoice_month - first_invoice_month) * 12), 
                          by = c("member_id", "product_name")]
  
  # take out duplicates 
  setkeyv(enrollment_time, c("member_id", "product_name"))
  enrollment_time <- subset(enrollment_time, !duplicated(enrollment_time, by = key(enrollment_time)))
  
  # merge on member data to get more insights
  enroll <- merge(enrollment_time, member_data, "member_id")
  
  enroll[, begin_to_end := (cancellation_date - signed_up_date) / 30]
  
  
  
  
  