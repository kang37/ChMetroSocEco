# 加载包。
pacman::p_load(dplyr, sf, ggplot2, forcats, terra, stringr)

# 获取包含目标数据的文件的名称。
ehi_files <- list.files("data_raw/Boundaries/区县尺度") %>% 
  grep("_EHIstats.shp", ., value = TRUE)

# 函数：读取目标文件并获得所需数据，即各区县EHI平均值和中位数。
get_ehi <- function(file_x) {
  # 从文件名中提取都市圈名称。
  ma_name <- gsub("MA_D_EHIstats.shp", "", file_x)
  # 获得目标数据。
  my_dt <- st_read(paste0("data_raw/Boundaries/区县尺度/", file_x)) %>% 
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


# 各都市圈各指标热力图。
# 获取目标tif文件名。
tar_files <- 
  lapply(
    c("EO", "EI", "ER", "ESCI"),
    function(x) {
      list.files(paste0("data_raw/", x, "_compare"), full.names = TRUE) %>% 
        grep("_scaled01.tif", ., value = TRUE) %>% 
        .[!grepl(".ovr", .)]
    }
  ) %>% 
  Reduce("c", .)

# 读取文件并且
get_tif_mean <- function(dir_x) {
  rast_dt <- rast(dir_x)
  res_mean <- global(rast_dt, fun = "mean", na.rm = TRUE)
  return(res_mean)
}

ma_index <- 
  # 计算各个tif平均值。
  lapply(tar_files, get_tif_mean) %>% 
  bind_rows() %>% 
  # 获取指标名称和城市信息。
  mutate(
    ma = lapply(
      tar_files, 
      function(x) str_extract(x, "(\\d+)([A-Za-z]+)(?=MA)")
    ) %>% 
      Reduce("c", .), 
    index = lapply(
      tar_files, 
      function(x) str_split(x, "/")[[1]][2] %>% gsub("_compare", "", .)
    ) %>% 
      Reduce("c", .), 
    .before = 1
  )

# 作热力图。
ggplot(ma_index) +
  geom_tile(aes(index, fct_rev(ma), fill = mean)) + 
  scale_fill_gradient(high = "red", low = "darkgreen") + 
  theme_bw() + 
  labs(x = "Index", y = NULL)

