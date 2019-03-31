# Zadania 1

# 1. Zastanów się w czym R może tobie pomóc? 
# Dlaczego chcesz się nauczyć go używać do analiz geoinformacyjnych?
# 2. Przejrzyj [CRAN Task View: Analysis of Spatial Data](https://cran.r-project.org/web/views/Spatial.html) i sprawdź czy istnieją tam pakiety, które mogą się Tobie przydać w codziennej pracy?

library(sf)
polska = st_read("data/polska.gpkg")

plot(polska)

polska

library(raster)
dem = raster("data/polska_srtm.tif")

plot(dem)

dem

# Zadania 2
  
# 1. Wczytaj do R plik `wlkp_pn.gpkg` zawierający granice Wielkopolskiego Parku Narodowego.
# Wyświetl ten obiekt i przejrzyj jego strukturę.
# Co możesz powiedzieć o zawartości tego pliku? 
# Jaki typ danych on przechowuje? 
# Jaki ma układ współrzędnych?
# Ile atrybutów zawiera?
# Jaka jest jego geometria?
# 2. Wczytaj do R plik `wlkp_pn_.gpkg_dem.tif` zawierający cyfrowy model wysokościowy dla obszaru Wielkopolskiego Parku Narodowego.
# Wyświetl ten obiekt i przejrzyj jego strukturę.
# Co możesz powiedzieć o zawartości tego pliku? 
# Jaki typ danych on przechowuje? 
# Jaki ma układ współrzędnych?
# Ile atrybutów zawiera?
# Jakie ma wymiary i rozdzielczość?
  

library(tmap)

tm_shape(polska) + 
  tm_polygons()

tm_shape(polska) + 
  tm_polygons() + 
  tm_scale_bar(position = c("left", 
                            "bottom")) 

tm_shape(polska) + 
  tm_polygons() + 
  tm_scale_bar(position = c("left", 
                            "bottom")) + 
  tm_compass()

tm_shape(polska) + 
  tm_polygons() + 
  tm_scale_bar(position = c("left", 
                            "bottom")) + 
  tm_compass() +
  tm_layout(title = "Polska")

tmap_mode("view")

tm_shape(polska) + 
  tm_polygons() + 
  tm_layout(title = "Polska")

tmap_mode("plot")

tm_shape(dem) + 
  tm_raster()

tm_shape(dem) + 
  tm_raster(style = "cont",
            midpoint = NA)

tm_shape(dem) + 
  tm_raster(style = "cont", 
            midpoint = NA,
            palette = "-RdYlGn")

tmaptools::palette_explorer()

tm_shape(dem) + 
  tm_raster(style = "fixed",
            breaks = c(-99, 0, 300,
                       600, 9999), 
            midpoint = NA,
            palette = "-RdYlGn",
            title = "") + 
  tm_layout(legend.position = c("LEFT", 
                                "BOTTOM"))

tm_shape(dem) + 
  tm_raster(style = "fixed",
            breaks = c(-99, 0, 300,
                       600, 9999), 
            labels = c("Depresje", 
                       "Niziny", 
                       "Wyżyny",
                       "Góry"),
            midpoint = NA,
            palette = "-RdYlGn",
            title = "") + 
  tm_layout(legend.position = c("LEFT", 
                                "BOTTOM"))

tm_shape(dem) + 
  tm_raster(style = "fixed",
            breaks = c(-99, 0, 300,
                       600, 9999), 
            labels = c("Depresje",
                       "Niziny",
                       "Wyżyny", 
                       "Góry"),
            midpoint = NA,
            palette = c("#5E8B73",
                        "#DAE97A", 
                        "#EADC70", 
                        "#AF8D5C"),
            title = "") + 
  tm_layout(legend.position = c("LEFT",
                                "BOTTOM"))

mapa1 = tm_shape(dem) + 
  tm_raster(style = "fixed",
            breaks = c(-99, 0, 300, 600, 9999),
            labels = c("Depresje", "Niziny", "Wyżyny", "Góry"),
            midpoint = NA, 
            palette = c("#5E8B73", "#DAE97A", "#EADC70", "#AF8D5C"),
            title = "") +
  tm_layout(legend.position = c("LEFT", "BOTTOM"))

tmap_save(mapa1, "moja_mapa1.png")

# # Zadania 3
# 
# 1. Ulepsz obiekt `mapa1` poprzez np. dodanie skali, strzałki północy, czy tytułu.
# Spróbuj też dodać granicę Polski do tej mapy.
# 2. Wczytaj do R pliki `wlkp_pn.gpkg` oraz `wlkp_pn_.gpkg_dem.tif`.
# Stwórz mapę przedstawiającą model terenu dla dla obszaru Wielkopolskiego Parku Narodowego.
# Zapisz uzyskaną mapę do pliku "WLKP_NAZWISKO.png".
# 3. Dodatkowo: spróbuj powtórzyć kroki z artykułu [Geocomputation with R: maps extended](https://geocompr.github.io/geocompkg/articles/maps.html) w celu wykonania mapy Polski z rzeźbą terenu wykonaną metodą cieniowania.

library(dplyr)

dane_meteo = read.csv("data/polska_meteo_2017.csv", encoding = "UTF-8")
head(dane_meteo)

stacje_meteo = st_read("data/polska_stacje.gpkg")
plot(st_geometry(stacje_meteo))

dane_meteo_wyb = dane_meteo %>% 
  filter(Nazwa.stacji %in% unique(stacje_meteo$NAZWA_ST))

dane_meteo_wyb = dane_meteo %>% 
  filter(Nazwa.stacji %in% unique(stacje_meteo$NAZWA_ST)) %>% 
  mutate(data = as.Date(paste0(Rok, "-", Miesiac, "-", Dzien)))

dane_meteo_wyb = dane_meteo %>% 
  filter(Nazwa.stacji %in% unique(stacje_meteo$NAZWA_ST)) %>% 
  mutate(data = as.Date(paste0(Rok, "-", Miesiac, "-", Dzien))) %>% 
  dplyr::select(-Rok, -Miesiac, -Dzien)

dane_meteo_wyb = dane_meteo %>% 
  filter(Nazwa.stacji %in% unique(stacje_meteo$NAZWA_ST)) %>% 
  mutate(data = as.Date(paste0(Rok, "-", Miesiac, "-", Dzien))) %>% 
  dplyr::select(-Rok, -Miesiac, -Dzien) %>% 
  summarize(tavg = mean(tavg), pressure = mean(pressure))

dane_meteo_wyb = dane_meteo %>% 
  filter(Nazwa.stacji %in% unique(stacje_meteo$NAZWA_ST)) %>% 
  mutate(data = as.Date(paste0(Rok, "-", Miesiac, "-", Dzien))) %>% 
  dplyr::select(-Rok, -Miesiac, -Dzien) %>% 
  group_by(Kod.stacji) %>% 
  summarize(tavg = mean(tavg), pressure = mean(pressure))

meteo = stacje_meteo %>% 
  left_join(dane_meteo_wyb, by = c("KOD_SZS" = "Kod.stacji"))

meteo = stacje_meteo %>% 
  inner_join(dane_meteo_wyb, by = c("KOD_SZS" = "Kod.stacji"))

plot(dem)
plot(st_geometry(meteo), axes = TRUE)

meteo = stacje_meteo %>% 
  inner_join(dane_meteo_wyb, by = c("KOD_SZS" = "Kod.stacji")) %>% 
  st_transform(4326)

tm_shape(dem) +
  tm_raster() +
  tm_shape(meteo) +
  tm_symbols(col = "tavg", palette = "RdBu")

meteo$elev = extract(dem, meteo)
head(meteo)

lc = raster("data/polska_lc.tif")
plot(lc)

wlkp_pn = st_read("data/wlkp_pn.gpkg", quiet = TRUE)
plot(st_geometry(wlkp_pn), axes = TRUE)

wlkp_pn_lc = crop(lc, wlkp_pn)
plot(wlkp_pn_lc)

wlkp_pn_lc2 = mask(wlkp_pn_lc, wlkp_pn)
plot(wlkp_pn_lc2)

# Zadania 4

# 1. Wczytaj dane meteorologiczne dla roku 2017 oraz dane o położeniu stacji meteorologicznych.
# Stwórz obiekt przestrzenny zawierający wszystkie pomiary dla trzech stacji: `BIAŁYSTOK`, `KRAKÓW-BALICE`, oraz `POZNAŃ`. (Porada: użyj do tego, między innymi funkcji `right_join()`).
# 2. Wylicz średnią temperaturę w styczniu oraz w lipcu dla tych trzech stacji.
# 3. Określ na jakim pokryciu terenu znajdują się te trzy stacje.
# 4. Dodatkowe: ulepsz mapę temperatur średnich zaprezentowaną w części "Reprojekcje".

library(landscapemetrics)
library(landscapetools)

show_landscape(wlkp_pn_lc, discrete = TRUE)

list_lsm()

lsm_l_ai(wlkp_pn_lc)

lsm_c_ed(wlkp_pn_lc)

lsm_p_para(wlkp_pn_lc)

calculate_lsm(wlkp_pn_lc, type = "aggregation metric")

mapa_p_para = get_lsm(wlkp_pn_lc, what = "lsm_p_para")
show_landscape(mapa_p_para[[1]][[1]])

show_lsm(wlkp_pn_lc, what = "lsm_p_para")

punkt = st_sf(st_sfc(geom = st_point(c(347500, 493000))))
plot(wlkp_pn_lc)
plot(st_geometry(punkt), add = TRUE, cex = 2)

sample_lsm(wlkp_pn_lc, punkt, shape = "circle", size = 2000, what = "lsm_c_ed")

# Zadania 5
  
# 1. W pliku `polska_pn.gpkg` znajdują się granice parków narodowych w Polsce.
# Wczytaj ten plik do R i wybierz z niego tylko Białowieski Park Narodowy.
# 2. Przytnij mapę pokrycia terenu z pliku `polska_lc.tif` do zasięgu (obwiedni) Białowieskiego Parku Narodowego.
# 3. Jakie kategorie pokrycia terenu można znaleźć na tym obszarze (możesz do tego celu użyć funkcji `unique()`)?
#   4. Wylicz powierzchnię całego obszaru oraz powierchnie kolejnych klas (`TA` i `CA`).
# Jakie są trzy kategorie o największej powierzchni?
#   5. Sprawdź ile płatów znajduje się na tym obszarze (`NP`).
