# 1. PACKAGES
#------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
    geodata, sf, terra,
    tidyverse, classInt,
    cartogram
)

# 2. DATA
#--------

url <- "https://datacatalogfiles.worldbank.org/ddh-published/0061507/DR0089195/aggdp2010.tif"
destfile <- basename(url)

download.file(
    url = url,
    destfile = destfile,
    mode = "wb"
)

aggdp_data <- terra::rast(destfile)

# 3. COUNTRY - LEVEL 1
#---------------------

country <- geodata::gadm(
    country = "BR",
    level = 1,
    path = getwd()
) |>
    sf::st_as_sf()

# 4. ZONAL STATS
#---------------

aggdp_subnational <- terra::zonal(
    x = aggdp_data,
    z = terra::vect(
        country
    ),
    fun = "sum",
    na.rm = TRUE
)

head(aggdp_subnational)

# 5. MERGE & CARTOGRAM
#---------------------

country_aggdp_subnational <- cbind(
    country, aggdp_subnational
)

crs <- "+proj=merc +lat_ts=-2 +lon_0=-43 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"

aggdp_cont <- country_aggdp_subnational |>
    sf::st_transform(crs = crs) |>
    cartogram::cartogram_cont(
        "alloc",
        itermax = 5
    )

# 6. BREAKS, COLORS & THEME
#--------------------------

aggdp_cont$alloc_million <- aggdp_cont$alloc / 1000000

breaks <- classInt::classIntervals(
    aggdp_cont$alloc_million,
    n = 4,
    style = "equal"
)$brks

limits <- c(
    min(
        aggdp_cont$alloc_million,
        na.rm = TRUE
    ),
    max(
        aggdp_cont$alloc_million,
        na.rm = TRUE
    )
)

cols <- hcl.colors(
    n = length(
        breaks
    ),
    palette = "viridis",
    rev = TRUE
)

theme_for_the_win <- function() {
    theme_void() +
        theme(
            legend.position = "top",
            legend.title = element_text(
                size = 11, color = "grey10"
            ),
            legend.text = element_text(
                size = 10, color = "grey10"
            ),
            plot.margin = unit(
                c(
                    t = .25, r = -1,
                    b = -1, l = -1
                ), "lines"
            )
        )
}

# 7. AGGDP CARTOGRAM MAP
#----------------------

map <- ggplot() +
    geom_sf(
        data = aggdp_cont,
        aes(
            fill = alloc_million
        ),
        color = "white",
        size = .2
    ) +
    scale_fill_gradientn(
        name = "USD millions",
        colors = cols,
        breaks = breaks,
        limits = limits,
        labels = round(
            breaks, 0
        ),
        na.value = "white"
    ) +
    guides(
        fill = guide_colorbar(
            direction = "horizontal",
            barheight = unit(1.5, "mm"),
            barwidth = unit(100, "mm"),
            title.position = "top",
            label.position = "bottom",
            title.hjust = .5,
            label.hjust = .5,
            nrow = 1,
            byrow = TRUE
        )
    ) +
    coord_sf(crs = crs) +
    labs(
        title = "Agricultural GDP for Brazil",
        caption = ""
    ) +
    theme_for_the_win()

# 8. AGGDP PER CAPITA
#--------------------

u <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2010_GLOBE_R2023A_4326_30ss/V1-0/GHS_POP_E2010_GLOBE_R2023A_4326_30ss_V1_0.zip"

file_name <- basename(u)

download.file(
    url = u,
    destfile = file_name,
    mode = "wb"
)

unzip(file_name)

raster_name <- gsub(
    ".zip", ".tif",
    file_name
)

pop <- terra::rast(raster_name)

pop_subnational <- terra::zonal(
    x = pop,
    z = terra::vect(
        country
    ),
    fun = "sum",
    na.rm = TRUE
)

head(pop_subnational)
names(pop_subnational)[1] <- "population"

country_aggdp_pc <- cbind(
    country_aggdp_subnational,
    pop_subnational
)

country_aggdp_pc$aggdp_pc <- 
country_aggdp_pc$alloc /
country_aggdp_pc$population

aggdp_pc_cont <- country_aggdp_pc |>
    sf::st_transform(
        crs = crs
    ) |>
    cartogram::cartogram_cont(
        "aggdp_pc",
        itermax = 5
    )

breaks <- classInt::classIntervals(
    aggdp_pc_cont$aggdp_pc,
    n = 4,
    style = "equal"
)$brks

limits <- c(
    min(
        aggdp_pc_cont$aggdp_pc,
        na.rm = TRUE
    ),
    max(
        aggdp_pc_cont$aggdp_pc,
        na.rm = TRUE
    )
)

cols <- hcl.colors(
    n = length(
        breaks
    ),
    palette = "viridis",
    rev = TRUE
)

map2 <- ggplot() +
    geom_sf(
        data = aggdp_pc_cont,
        aes(
            fill = aggdp_pc
        ),
        color = "white",
        size = .2
    ) +
    scale_fill_gradientn(
        name = "USD",
        colors = cols,
        breaks = breaks,
        limits = limits,
        labels = round(
            breaks, 0
        ),
        na.value = "white"
    ) +
    guides(
        fill = guide_colorbar(
            direction = "horizontal",
            barheight = unit(1.5, "mm"),
            barwidth = unit(100, "mm"),
            title.position = "top",
            label.position = "bottom",
            title.hjust = .5,
            label.hjust = .5, # should be label.hjust
            nrow = 1,
            byrow = TRUE
        )
    ) +
    coord_sf(crs = crs) +
    labs( # should be labs
        title = "Agricultural GDP per capita for Brazil",
        caption = ""
    ) +
    theme_for_the_win()
