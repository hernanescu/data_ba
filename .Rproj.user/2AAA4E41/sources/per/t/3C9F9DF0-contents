get_comunas <- function(user) {
    
    st_intersection(comunas, st_point(c(start_value[1,2], start_value[1,3]))) %>% 
        select(comuna) %>% 
        st_set_geometry(NULL) %>%
        pull() %>% 
        as.factor(.)
    
}

# sarasa2
# start_value
# 
# test <- st_intersection(comunas, st_point(c(start_value[1,2], start_value[1,3]))) %>% 
#     select(comuna) %>% 
#     st_set_geometry(NULL) %>%
#     pull() %>% 
#     as.factor(.)
# 
