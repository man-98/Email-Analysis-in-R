#libraries 
library(gmailr)
library(tidyverse)
library(lubridate)
library(esquisse)

#setting up connecting to GMAIL

use_secret_file("../../Downloads/key.json")

gm_auth_configure(path = "../../Downloads/key.json")

my_email_message <- mime() %>% 
  gm_to("mabundey@gmail.com") %>% 
  gm_from("mabundey@gmail.com") %>% 
  gm_subject("Test Message") %>% 
  gm_text_body("Messag from R on MAC")

gm_send_message(my_email_message)


# starting extraction
messageIDs <- gm_messages(user_id = "mabundey@gmail.com", num_results = 200)


my_messages <- messageIDs[[1]]$messages %>% 
  modify_depth(1,"id") %>% 
  as_vector() %>% 
  map(gm_message)

write_rds(my_messages, "MarketingEmails.rds")

#trial check
new_my_messages[[1]] %>% gm_body %>% unlist %>% str_length()


#removing erros and preparing data
len <- 1:length(my_messages)

new_my_messages <- list()
i = len[1]

for(x in len){
  check <- my_messages[[x]] %>% gm_body %>% unlist
  if(check %>% class != typeof(NULL))
  {
    if(check %>%  str_length > 0){
      new_my_messages[i] <- my_messages[x]
      i <- i+1
    }
  }
}

new_my_messages


EmailTemplates <- tibble(
  id = new_my_messages %>% map_chr(gm_id),
  Subject = new_my_messages %>% map_chr(gm_subject),
  date = new_my_messages %>% map_chr(gm_date) %>% dmy_hms,
  bodyLength = new_my_messages %>% map_int(~gm_body(.) %>% unlist %>% str_length),
  
  #from = my_messages %>% map_chr(from)
)

EmailTemplates
esquisser(EmailTemplates)
