library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(dplyr)
library(readr)

houses <- read_csv("kc_house_data.csv")

houses$city <- ifelse(houses$zipcode %in% c("98001", "98002", "98092"), "Auburn",
                      ifelse(houses$zipcode %in% c("98003", "98023"), "Federal Way",
                             ifelse(houses$zipcode %in% c("98004", "98005", "98006", "98007", "98008"), "Bellevue",
                                    ifelse(houses$zipcode == "98010", "Black Diamond",
                                           ifelse(houses$zipcode == "98011", "Bothell",
                                                  ifelse(houses$zipcode == "98014", "Carnation",
                                                         ifelse(houses$zipcode == "98019", "Duvall",
                                                                ifelse(houses$zipcode == "98022", "Enumclaw",
                                                                       ifelse(houses$zipcode == "98024", "Fall City",
                                                                              ifelse(houses$zipcode %in% c("98027", "98029"), "Issaquah",
                                                                                     ifelse(houses$zipcode == "98028", "Kenmore",
                                                                                            ifelse(houses$zipcode %in% c("98030", "98031", "98032", "98042"), "Kent",
                                                                                                   ifelse(houses$zipcode %in% c("98033", "98034"), "Kirkland",
                                                                                                          ifelse(houses$zipcode %in% c("98038", "98040"), "Maple Valley",
                                                                                                                 ifelse(houses$zipcode == "98039", "Medina",
                                                                                                                        ifelse(houses$zipcode == "98045", "North Bend",
                                                                                                                               ifelse(houses$zipcode %in% c("98052", "98053"), "Redmond",
                                                                                                                                      ifelse(houses$zipcode %in% c("98055", "98056", "98058", "98059"), "Renton",
                                                                                                                                             ifelse(houses$zipcode == "98065", "Snoqualmie",
                                                                                                                                                    ifelse(houses$zipcode == "98070", "Vashon",
                                                                                                                                                           ifelse(houses$zipcode %in% c("98072", "98077"), "Woodinville",
                                                                                                                                                                  ifelse(houses$zipcode %in% c("98074", "98075"), "Sammamish", "Seattle"
                                                                                                                                                                  ))))))))))))))))))))))
houses <- houses %>% 
  mutate_at(vars(waterfront, view, zipcode, city), factor) %>% 
  select(-c(ends_with("15"), starts_with("yr"),"id", "condition", "date")) %>% 
  mutate_at(vars(bathrooms, floors), funs(round(., 0))) %>% 
  mutate_at(vars(grade), funs(round(./13*10, 0))) %>% 
  filter(bedrooms != 33)