library(shiny)
library(shinydashboard)
library(DT)
library(data.table)
library(plotly)
library(leaflet)
library(readr)
library(googleVis)
library(tidyverse)
library(broom)
library(purrr)
library(tidytext)

airbnb<-read_csv("../data-raw/listings.csv",
                 col_types = cols(host_id = col_skip(), 
                                  calculated_host_listings_count=col_skip(),
                                  last_review=col_skip()))

bb<-read_csv("../data-raw/listings2.csv")
bb %>% 
  select(id,review_scores_rating)->
  bb


cc<-read_csv("../data-raw/reviews.csv")
cc %>% 
  select(listing_id,comments)->
  cc

airbnb %>% 
  left_join(bb,by="id") %>% 
  na.omit()->
  airbnb


airbnb %>% 
  mutate(neighbourhood_group=parse_factor(neighbourhood_group),
         room_type=parse_factor(room_type),
         neighbourhood=parse_factor(neighbourhood)) ->
  airbnb_p2s
airbnb_p2s %>% 
  filter(availability_365!=0) %>%
  mutate(minimum_nights=str_replace(minimum_nights,"\\d\\d+","10+"),
         minimum_nights=parse_factor(minimum_nights))%>% 
  filter(price>quantile(airbnb_p2s$price,.05)&price<quantile(airbnb_p2s$price,.95)
         &number_of_reviews>quantile(airbnb_p2s$number_of_reviews,.05)
         &number_of_reviews<quantile(airbnb_p2s$number_of_reviews,.95)
         &reviews_per_month>quantile(airbnb_p2s$reviews_per_month,.05,na.rm=T)
         &reviews_per_month<quantile(airbnb_p2s$reviews_per_month,.95,na.rm=T)
         &availability_365>quantile(airbnb_p2s$availability_365,.05)
         &availability_365<quantile(airbnb_p2s$availability_365,.95))->
  airbnb

airbnb %>% 
  left_join(cc,by=c("id"="listing_id")) %>% 
  select(id, comments) %>% 
  group_by(id) %>% 
  mutate(linenumber=row_number()) %>% 
  ungroup() %>% 
  unnest_tokens(word, comments) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>% 
  anti_join(stop_words, by = "word") %>% 
  na.omit()->
  airbnbroom

airbnb %>% 
  select(!name&!host_name&!neighbourhood&!latitude&!longitude&!id)->
  airbnb_p2

airbnb_p2s %>% 
  select(!colnames(airbnb_p2))->
  airbnb_other


colnames(airbnb_p2[map_lgl(airbnb_p2, is.double)])->col_num
airbnb_p2 %>% 
  select(all_of(col_num)) ->
  p2_num
airbnb_p2 %>% 
  select(!all_of(col_num))->
  p2_fct











