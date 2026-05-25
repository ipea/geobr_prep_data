library(duckspatial)
library(dplyr)
library(mapview)

mapviewOptions(platform = "leaflet")
# mapviewOptions(platform = "leafgl")

df <- geobr::read_census_tract(code_tract = "all", year = 2010, simplified = FALSE)

# de_para <- readxl::read_excel("C:/Users/rafap/Downloads/Composição das Áreas de Ponderação.xls")
de_para2 <- readxl::read_excel("C:/Users/rafap/Downloads/Composição das Áreas de Ponderação_Alterada_Alguns_Municipios.xls")


de_para <- data.table::fread("C:/Users/rafap/Downloads/Documentacao/Documentaç╞o/╡reas de Ponderaç╞o/Composicao dasareas de Ponderacao.txt", colClasses = 'character')

names(de_para) <- c('code_weighting_area1', 'code_tract')
names(de_para2) <- c('code_tract', 'code_weighting_area2')

de_para$code_tract <- as.numeric(de_para$code_tract) |> as.character()
de_para$code_weighting_area1 <- as.numeric(de_para$code_weighting_area1)|> as.character()

unique(de_para$code_weighting_area1) |> length()
unique(de_para2$code_weighting_area2) |> length()



nchar(de_para$code_tract[1])
nchar(de_para2$code_tract[1])
nchar(df$code_tract[1])


de_para3 <- left_join(de_para, de_para2)


de_para3 <- de_para3 |> 
  mutate(
    code_weighting_area = ifelse(is.na(code_weighting_area2), code_weighting_area1, code_weighting_area2)
    ) |> 
  select(code_tract, code_weighting_area)

head(de_para3)
is.na(de_para3$code_weighting_area) |> sum()


unique(de_para3$code_weighting_area) |> length()

df2 <- left_join(df, de_para3, by = "code_tract") 
is.na(df2$code_weighting_area) |> sum()

unique(df2$code_weighting_area)

df2_sudeste <- df2 |> 
  filter(code_state %in% c( 35))

is.na(df2_sudeste$code_weighting_area) |> sum()
unique(df2_sudeste$code_weighting_area)

weighting_areas <- duckspatial::ddbs_union_agg(
  df2,
  by = "code_weighting_area"
  ) |> 
  duckspatial::ddbs_collect()

nrow(weighting_areas)
mapview(weighting_areas, zcol = "code_weighting_area")


class(weighting_areas) <- setdiff(class(weighting_areas), c("tbl_df", "tbl"))


duckspatial::ddbs_write_dataset(
  data = weighting_areas, 
  path = "weighting_areas_2010.parquet", 
  crs = "EPSG:4674"
  )



a <- weighting_areas |> 
  filter(code_weighting_area == "3554003003002")

plot(a)

b <- a %>% 
  st_make_valid() %>%
  st_combine() %>%
  st_union()

plot(b)

b2 <- a |> 
  duckspatial::ddbs_make_valid() |> 
  duckspatial::ddbs_combine() |> 
  duckspatial::ddbs_union() |> 
  duckspatial::ddbs_collect()

plot(b2)
