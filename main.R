# 加载包。
pacman::p_load(dplyr, sf, ggplot2, forcats)

# 获取包含目标数据的文件的名称。
ehi_files <- list.files("data_raw/区县尺度") %>% 
  grep("_EHIstats.shp", ., value = TRUE)

# 函数：读取目标文件并获得所需数据，即各区县EHI平均值和中位数。
get_ehi <- function(file_x) {
  # 从文件名中提取都市圈名称。
  ma_name <- gsub("MA_D_EHIstats.shp", "", file_x)
  # 获得目标数据。
  my_dt <- st_read(paste0("data_raw/区县尺度/", file_x)) %>% 
    rename_with(~ tolower(.x))
  # Bug: 提醒谢命名应一致。
  if("name_1" %in% names(my_dt)) my_dt <- my_dt %>% rename("name" = "name_1")
  my_dt <- my_dt %>% 
    select(name, ehi_med, ehi_mean) %>% 
    mutate(ma = ma_name, .before =1 ) %>% 
    # Bug: 和谢讨论使用哪个EPSG。目前使用CGCS2000大地坐标系。它不带投影，代表了基于CGCS2000椭球体的经纬度坐标。
    st_transform(4490)
  return(my_dt)
}
# 读取各都市圈数据。
ma_ehi <- lapply(ehi_files, get_ehi) %>% 
  setNames(ehi_files)

# 制图：区县EHI平均值地图。
lapply(
  ehi_files, 
  function(x) {
    png(
      paste0("data_proc/", gsub("MA_D_EHIstats.shp", "", x), ".png"), 
      res = 300, width = 1000, height = 1000
    )
    plt <- ggplot(ma_ehi[[x]]) + 
      geom_sf(aes(fill = ehi_mean)) + 
      scale_fill_gradient(high = "#FF4500", low = "#1E90FF") + 
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 90))
    print(plt)
    dev.off()
  }
)

# 各都市圈内区县EHI值箱型图。
ma_ehi %>% 
  bind_rows() %>% 
  st_drop_geometry() %>% 
  ggplot() + 
  geom_boxplot(aes(fct_rev(ma), ehi_mean)) +
  coord_flip() + 
  theme_bw() + 
  labs(x = NULL, y = "Mean EHI")

# 以南京都市圈为例，计算最大值、最小值、平均值、标准差。
library(terra)
nanjing_rast <- rast("data_raw/01nanjingMA_EHI_scaled01.tif")

# 获取栅格数据的统计摘要
summary(nanjing_rast)
global(nanjing_rast, fun = "sd", na.rm = TRUE)
