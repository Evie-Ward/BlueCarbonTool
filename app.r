# Coastal Blue Carbon App to go alongside the mangrove conservation module as part of the Plan Vivo Coastal Blue Carbon methodology
# Author: Evie Ward; evie.ward@outlook.com

library(shiny)
library(terra) # for vectors and rasters and deals with shapefiles
library(sf) # also for vectors because the exactextract package hates terra
library(exactextractr)
library(DT) # data table - should now load
library(leaflet)
library(MetBrewer)
library(shinyalert)

# remember to commit to git


# Part 1 - base stuff that's needed to be loaded in on front end and back end


climate_zones <- rast(
    "www/IPCC_Climate_Zones_Map_raster/Raster/ipcc_climate_1985-2015.tif"
)

climate_key <- read.csv("www/climate_key.csv")

ipcc_zones <- vect(
    "www/IPCC_Climate_Zones_Map_vector/Vector/ipcc_climate_1985-2015.shp"
)

ipcc_zones$Field2 <- ifelse(ipcc_zones$Field2 == "Tropical Moist", "Tropical Wet", ipcc_zones$Field2)

names(ipcc_zones) <- names(climate_key[-1])

marine_ecoregions <- vect(
    "www/Marine_Ecoregions_Cropped/meow_mangrove_GMWv3.shp"
)

marine_province <- marine_ecoregions$PROVINCE


# Part 2 - User Interface (UI)

# define UI for app
ui <- fluidPage(
    useShinyalert(force = TRUE),
    tags$head(
        # Note the wrapping of the string in HTML()
        tags$style(HTML("
        .legend {
        height: 235px;
        overflow-y: auto;
        }

        .leaflet-control {z-index: 800}

        .header{
        border-radius: 5px;
        background: #1171ba;
        width: 100%;
        height: 100%;
        padding: 0px 20px 0px 20px;
        }

        #title{
            color: white;
            float: center;
            font-size: 100%;
            font-weight: bold;
            text-align: left;
            padding: 30px 0px 0px 0px;
        }

        #logo{
            float: right;
            padding: 10px 0px 20px 0px;
        }

        #soctext {
            display: block;
            padding: 9.5px;
            margin: 0 0 10px;
            margin-top: 10px;
            font-size: 14px;
            background-color: #F5F5F5;
            border: 1px solid rgba(0,0,0,0.15);
            border-radius: 4px;
        }
        ")),
    ),
    tags$div(
        class = "header",
        titlePanel(
            fluidRow(
                column(
                    6,
                    tags$div(
                        id = "title",
                        "Plan Vivo Coastal Blue Carbon Tool",
                        textAlign = "left"
                    )
                ),

#to add logo if wanted, commented out as currently undergoing a rebrand, so didn't want to include an outdated logo

                # column(
                #     6,
                #     tags$a(
                #         href = "https://www.planvivo.org",
                #         target = "_blank",
                #         id = "logo",
                #         img(
                #             src = "Images/logo_white.png",
                #             height = "60px",
                #             align = "right"
                #         )
                #     )
                # )
            ),
            windowTitle = "Plan Vivo Coastal Blue Carbon Tool"
        )
    ),

    # Sidebar layout with input and output definitions
    sidebarLayout(

        # Sidebar panel for inputs
        sidebarPanel(

            # Input: upload shapefile or gpkg
            fileInput("projectvector",
                h3("Input Project Area"),

                # only accepts .shp and .gpkg and .kml
                accept = c(
                    ".gpkg", # gpkg is a geo package
                    ".zip", # zipped shapefile
                    ".kml" # kml files (google)
                )
            ),
            div(
                class = "input-group",
                tags$span(
                    style = "vertical-align: bottom;",
                    actionButton("about_b", "More Information",
                        icon = icon("circle-question", "lg")
                    )
                )
            ),
            br(),
            br(),
            downloadButton("downloadST", "Download Summary Table"),
        ),


        # Main panel for displaying outputs
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Summary Table",
                    br(),
                    tableOutput("summary_table"),
                    br()
                ),
                tabPanel(
                    "Soil Organic Carbon",
                    br(),
                    plotOutput("plot_soc"),
                    br(),
                    tags$div(
                        id = "soctext",
                        textOutput("mediansoc_text"),
                        textOutput("totalsoc_text")
                    )
                ),
                tabPanel(
                    "IPCC Climate Zone and Marine Province",
                    br(),
                    leafletOutput("plot_climate"),
                    br(),
                ),
            )
        )
    )
)


# Part 3 - Server - where all the calculations happen / website runs
server <- function(input, output, session) {
    # for the 'more information' button
    observeEvent(input$about_b, {
        shinyalert(
            html = TRUE,
            closeOnClickOutside = TRUE,
            showConfirmButton = FALSE,
            size = "m",
            text = paste(
                "<h3 style='font-size:20px; text-align: left'><strong>",
                # "*CONTEXT AND INTRO HERE*",
                "</strong>",
                "</h3>",
                "<p style = 'font-size:16px; text-align: left'>",
                # "...",
                "</p>",
                "<p style = 'font-size:16px; text-align: left'>",
                "Note: area given in the summary table is calculated for the
                whole project area, not the extent of mangroves within the
                project area.",
                "</p>",
                "<br>",
                "<h3 style='font-size:20px; text-align: left'><strong>",
                "Upload the following for Project Area:",
                "</strong>",
                "</h3>",
                "<ul style = 'font-size:16px; text-align: left'>",
                "<li><strong>",
                "Shapefile - ",
                "</strong>",
                "must be a zipped folder containing files with extensions
                .shp, .shx, and .dbf",
                "</li>",
                "<li><strong>",
                "Geopackage - ",
                "</strong>",
                "file with extension .gpkg",
                "</li>",
                "<li><strong>",
                "KML - ",
                "</strong>",
                "file with extension .kml",
                "</li>",
                "</ul>",
                "<br>",
                "<h3 style='font-size:20px; text-align: left'><strong>",
                "Information on data sources:",
                "</strong>",
                "</h3>",
                "<ul style = 'font-size:16px; text-align: left'>",
                "<li><strong>",
                "Soil Organic Carbon - ",
                "</strong>",
                "Global distribution of mangrove soil carbon stocks for
                the top meter of soil (30m resolution)<sup>1</sup>",
                "</li>",
                "<li><strong>",
                "Marine Province - ",
                "</strong>",
                "Marine provinces of the world<sup>2</sup>, cropped to areas
                with mangroves<sup>3</sup>",
                "</li>",
                "<li><strong>",
                "IPCC Climate Zone - ",
                "</strong>",
                "Updated climate zone map in the 2019 Refinement to the 2006
                IPCC Guidelines for National Greenhouse Gas Inventories
                <sup>4</sup>",
                "</li>",
                "</ul>",
                "<br>",
                "<ol style = 'font-size:14px; text-align: left'>",
                "<li>",
                "Sanderman, J., Hengl, T., Fiske, G., Solvik, K., Adame, M.F.,
                Benson, L., Bukoski, J.J., Carnell, P., Cifuentes-Jara, M.,
                Donato, D. and Duncan, C., 2018. A global map of mangrove
                forest soil carbon at 30 m spatial resolution. Environmental
                Research Letters, 13(5), p.055002.",
                "<a href=
                'https://iopscience.iop.org/article/10.1088/1748-9326/aabe1c/meta'
                target='_blank'>DOI: 10.1088/1748-9326/aabe1c</a>",
                "</li>",
                "<li>",
                "Spalding, M. D., Fox, H. E., Allen, G. R., Davidson, N.,
                Ferdaña, Z. A., Finlayson, M. A. X., Halpern, B. S., Jorge,
                M. A., Lombana, A. L., Lourie, S. A., Martin, K. D., Manus,
                M. C., Molnar, J., Recchia, C. A., & Robertson, J. (2007).
                Marine ecoregions of the world: A bioregionalisation of coastal
                and shelf areas. BioScience, 57(7), 573– 583.",
                "<a href=
                'https://academic.oup.com/bioscience/article/57/7/573/238419'
                target='_blank'>DOI: 10.1641/B570707</a>",
                "</li>",
                "<li>",
                "Adame, M.F., Connolly, R.M., Turschwell, M.P., Lovelock, C.E.,
                Fatoyinbo, T., Lagomasino, D., Goldberg, L.A., Holdorf, J.,
                Friess, D.A., Sasmito, S.D. and Sanderman, J., 2021. Future
                carbon emissions from global mangrove forest loss. Global
                change biology, 27(12), pp.2856-2866.",
                "<a href=
                'https://onlinelibrary.wiley.com/doi/10.1111/gcb.15571'
                target='_blank'>DOI: 10.1111/gcb.15571</a>",
                "</li>",
                "<li>",
                "Audebert, P., Tullis, J., Ogle, S., Bernoux, M., Schiettecatte,
                L.S. 2021. The IPCC Climate Zone Map - Raster. 2019 Refinement 
                to the 2006 IPCC Guidelines for National Greenhouse Gas 
                Inventories. IPCC.",
                "<a href=
                'https://philipaudebert.users.earthengine.app/view/ipcc-climate-zones'
                target='_blank'>Link here.</a>",
                "</li>",
                "</ol>"
            )
        )
    })


    boundaries <- reactive({
        req(input$projectvector)
        # tells terra to open up vector layer (goepackages, shapefiles etc)
        # $ is a subset sign that pulls out objects from within another object
        # bound is the name given to project area shapefile

        # to avoid the error of shapefiles being placed in a tempory file
        # without the rest of the files needed
        # shapefiles must be uploaded as zipped folders
        # the below code then unzips the folder if shapefiles are uploaded
        # if not (i.e. gpkg uploaded) then the 'else' function runs
        
        if (any(tools::file_ext(input$projectvector) == "zip")) {
            tmp <- file.path(tempdir(), "usr_shp")
            clean_path <- gsub("\\\\", "/", input$projectvector$datapath)
            unzip(clean_path, exdir = tmp)
            bound.shp <- list.files(
                tmp,
                recursive = TRUE,
                full.names = TRUE,
                pattern = ".shp$"
            )
            bound <- vect(bound.shp)
            unlink(tmp, recursive = TRUE)
        } else {
            bound <- vect(input$projectvector$datapath)
        }
        # here we are projecting bound into WGS84 if not already
        # != is 'not equal to'. If statements should always feed true.
        if (crs(bound) != crs("epsg:4326")) {
            bound <- project(bound, crs("epsg:4326"))
            # epsg:4326 is the code for WGS84
        }
    })
    tile_id <- reactive({
        req(boundaries())
        tiles <- vect("www/SOC_Sanderman/Mangrove_Tiles/ov_mangrove_tiles.shp")
        # is.related finds if tiles intersect with bound
        # then we filter only tiles that intersect with boundaries.
        tiles <- tiles[is.related(tiles, boundaries(), "intersects")]
        tile_id <- tiles$ID # pulls out ID attribute
    })
    soc_tiles <- reactive({
        req(boundaries(), tile_id())
        soc_directory <- "www/SOC_Sanderman/Mangrove_SOC"
        soc_paths <- list.files(soc_directory, recursive = TRUE)
        if (length(tile_id()) > 1) {
            filter_id <- paste(tile_id(), collapse = "|")
        } else {
            filter_id <- tile_id()
        }
        soc_paths <- grep(filter_id, soc_paths, value = TRUE)
        soc_paths <- file.path(soc_directory, soc_paths) #
        if (length(soc_paths) == 1) {
            soc <- rast(soc_paths)
        } else if (length(soc_paths) > 1) {
            soc <- sprc(soc_paths) |>
                mosaic()
        }
        soc <- crop(soc, boundaries(), mask = TRUE)
        return(soc)
    })


    # median SOC
    mediansoc_data <- reactive({
        req(soc_tiles())
        boundaries_sf <- st_as_sf(boundaries()) |>
            st_union()
        mediansoc_data <- exact_extract(
            (soc_tiles()),
            boundaries_sf,
            "median",
            force_df = TRUE
        )
        return(mediansoc_data)
    })

    # convert from C to CO2e
    mediansocco2e_data <- reactive({
        mediansocco2e_data <- mediansoc_data() * 44/12
        return(mediansocco2e_data)
    })



    #total SOC
    totalsoc_data <- reactive({
        req(soc_tiles())
        # exactextract doesn't love terra objects so we convert to an sf one
        boundaries_sf <- st_as_sf(boundaries()) |>
            # merge the multiple boundaries into a single geometry
            st_union()
        # multiplied by 0.09 as the raster cells were 30*30, but gave soc/ha
        # 30*30m is 0.09ha
        totalsoc_data <- exact_extract(
            (soc_tiles() * 0.09),
            boundaries_sf,
            "sum",
            force_df = TRUE
        )
        return(totalsoc_data)
    })

     # convert from C to CO2e
    totalsocco2e_data <- reactive({
        totalsocco2e_data <- totalsoc_data() * 44/12
        return(totalsocco2e_data)
    })
   

    # project area
    project_area <- reactive({
        expanse(boundaries(), unit = "ha", transform = TRUE)
    })

    sum_area <- reactive({
        sum(project_area())
    })


    # ipcc climate zones
    ipcc_zone <- reactive({
        # exactextract doesn't love terra objects so we convert to an sf one
        boundaries_sf <- st_as_sf(boundaries()) |>
            # merges the multiple boundaries into a single geometry
            st_union()
        # extract the 'unique' values from raster that overlaps with polygon
        # x = raster, y = polygon
        climate_data <- exact_extract(climate_zones, boundaries_sf,
            fun = function(x) unique(x), summarize_df = TRUE, force_df = TRUE
        )[1]
        # change names from number values to the IPCC climate zone names
        # (i.e. numerical -> words)
        climate_key[unique(climate_data)[[1]], "climate_zone"]
    })

    # marine province
    ecoregion_data <- reactive({
        # remove the terra to sf conversion as using a terra function
        # merge the multiple boundaries into a single geometry
        boundaries() |>
            union()
        ecoregion_data <- extract(marine_ecoregions, boundaries())
    })

    # listing all extracted data
    extracted_data <- reactive({
        extracted_data <- data.frame(
            sum_area(),
            mediansocco2e_data(),
            totalsocco2e_data(),
            ipcc_zone(),
            unique(ecoregion_data()$PROVINCE)
        )
        # change names of columns
        names(extracted_data) <- c(
            "Project Area (ha)",
            "Median SOC (t CO2e ha-1)",
            "Total SOC (t CO2e)",
            "IPCC Climate Zone",
            "Marine Province"
        )
        return(extracted_data)
    })


    # outputs
    # summary table
    output$summary_table <- renderTable({
        req(boundaries()) # empty brackets tells R it's a reactive object
        extracted_data()
    })

    # SOC
    output$plot_soc <- renderPlot({
        req(soc_tiles())
        plot(
            soc_tiles(),
            col = rev(map.pal("viridis")), #rev can be used to inverse anything
            main = paste(
                "Distribution of mangrove soil organic carbon ",
                "stocks in the project area (Mg C ha−1)"),
            legend = TRUE, buffer = TRUE
        )
        plot(
            boundaries(),
            add = TRUE
        )
    })

    output$mediansoc_text <- renderText({
        req(mediansocco2e_data())
        paste(
            "Median SOC per hectare for mangroves in the project area: ",
            mediansocco2e_data(),
            "t CO2e ha-1"
        )
    })

    output$totalsoc_text <- renderText({
        req(totalsocco2e_data())
        paste(
            "Total SOC for mangroves in the project area: ",
            totalsocco2e_data(),
            "t CO2e"
        )
    })


    # to add a leaflet map
    output$plot_climate <- renderLeaflet({
        req(boundaries())

        # get long and lat of boundary data so the map can zoom to polygon
        xy <- crds(centroids(boundaries())[1])
        boundary_lat <- xy[2]
        boundary_long <- xy[1]

        ipcc_color <- colorFactor(
            palette = c(
                met.brewer("Hokusai1", type = "discrete")
                # n = "11"
            ),
            na.color = "transparent",
            levels = ipcc_zones$climate_zone,
            reverse = "FALSE"
        )

        province_color <- colorFactor(
            palette = c(
                met.brewer("Klimt", type = "discrete")
            ),
            domain = unique(marine_province),
            na.color = "transparent"
        )

        leaflet() |>
            setView(boundary_long, boundary_lat, zoom = 12) |>
            # to get boundaries on the top layer
            addMapPane(name = "boundaries", zIndex = 2) |>
            addMapPane(name = "other", zIndex = 1) |>
            addProviderTiles(
                provider = providers$CartoDB.Positron,
                options = leafletOptions(pane = "other")
            ) |>
            
            addPolygons(
                options = leafletOptions(pane = "boundaries"),
                data = boundaries(),
                color = "#3F3F3F",
                opacity = 1,
                fillColor = "#3F3F3F",
                fillOpacity = 0.7,
                popup = paste(
                    "<b>IPCC Climate Zone:</b>", ipcc_zone(), "<br>",
                    "<b>Marine Province:</b>", unique(ecoregion_data()$PROVINCE)
                )
            ) |>
            addPolygons(
                group = "IPCC Climate Zone",
                options = leafletOptions(pane = "other"),
                data = ipcc_zones,
                color = ~ ipcc_color(climate_zone),
                weight = 1,
                opacity = 0.2,
                fillOpacity = 0.7, smoothFactor = 0.5,
                popup = paste(ipcc_zones$climate_zone)
            ) |>
            addLegend(
                group = "IPCC Climate Zone",
                values = unique(ipcc_zones$climate_zone),
                pal = ipcc_color,
                "bottomright",
                title = "IPCC Climate Zone",
                opacity = 0.8
            ) |>
            addPolygons(
                options = leafletOptions(pane = "other"),
                group = "Marine Province",
                data = marine_ecoregions,
                color = ~ province_color(PROVINCE),
                weight = 1,
                opacity = 0.2,
                fillOpacity = 0.7, smoothFactor = 0.5,
                popup = paste(marine_ecoregions$PROVINCE)
            ) |>
            addLegend(
                layerId = "marine_province_legend",
                group = "Marine Province",
                values = unique(marine_ecoregions$PROVINCE),
                pal = province_color,
                position = "bottomleft",
                title = "Marine Province",
                opacity = 0.8
            ) |>
            addLayersControl(
                overlayGroups = c("IPCC Climate Zone", "Marine Province"),
                options = layersControlOptions(collapsed = FALSE)
            ) |>
            hideGroup("IPCC Climate Zone") |>
            hideGroup("Marine Province")
    })

    # download data button
    output$downloadST <- downloadHandler(
        filename = function() {
            paste("summary_table-", Sys.Date(), ".csv", sep = "")
        },
        content = function(con) {
            write.csv(extracted_data(), con)
        }
    )
}


# # call to shinyapp function
shinyApp(ui, server)

# library(rsconnect)
# deployApp()