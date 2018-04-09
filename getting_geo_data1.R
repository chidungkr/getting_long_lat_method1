

#====================================================
#   Hàm lấy dữ liệu long - lat nếu cho trước địa chỉ
#====================================================


library(magrittr)
library(tidyverse)
library(ggmap)


get_spatial_data <- function(your_add) {   
  geo_res <- geocode(your_add, 
                     output = "all", 
                     messaging = TRUE, 
                     override_limit = TRUE)

  answer <- data.frame(lat = NA, 
                       long = NA, 
                       accuracy = NA, 
                       formatted_address = NA, 
                       address_type = NA, 
                       status = NA)
  
  answer %<>% mutate(status = geo_res$status)
  
  # Kiểm tra điều kiện về giới hạn thời gian cho query: 
  while(geo_res$status == "OVER_QUERY_LIMIT") {
    print("Quá giới hạn tìm kiếm. Chờ thêm 60 phút nữa tính từ:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_res <- geocode(your_add, 
                       output = "all", 
                       messaging = TRUE, 
                       override_limit = TRUE)
    
    answer %<>% mutate(status = goe_res$status)
  }
  
  # Nếu  không được kết nối: 
  if (geo_res$status != "OK"){
    return(answer)
  }   
  
  # Nếu được kết nối làm các việc sau: 
  answer %<>% 
    mutate(lat = geo_res$results[[1]]$geometry$location$lat, 
           long = geo_res$results[[1]]$geometry$location$lng)
  
  
  if (length(geo_res$results[[1]]$types) > 0) {
    answer %<>% mutate(accuracy = geo_res$results[[1]]$types[[1]])

  }
  
  answer %<>% 
    mutate(address_type = paste(geo_res$results[[1]]$types, collapse = ","), 
           formatted_address = geo_res$results[[1]]$formatted_address)
  
  return(answer)
}


# Test hàm cho một địa chỉ" 
linh_dam_geo <- get_spatial_data("chung cư linh đàm, Hà Nội")


# Test hàm cho một loạt các địa chỉ (lấy từ https://batdongsan.com.vn/khu-do-thi-moi-ha-noi): 

add_list <- c("Đường 70, phường Đại Mỗ, quận Nam Từ Liêm, Hà Nội", 
              "Phú Cát City, Xã Thạch Hòa, Thạch Thất, Hà Nội")

all_geo_data <- lapply(add_list, get_spatial_data)

geo_data <- do.call("bind_rows", all_geo_data)


# Test map: 
map <- get_map(location = "Hanoi", zoom = 12)

ggmap(map) + 
  geom_point(data = geo_data, 
             aes(x = long, y = lat), color = "red", size = 3, alpha = 0.4) + 
  theme(axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white")) + 
  labs(title = "Location of Three Real Estate Projects Selected in Hanoi", 
       subtitle = "Data Source: https://batdongsan.com.vn/")



