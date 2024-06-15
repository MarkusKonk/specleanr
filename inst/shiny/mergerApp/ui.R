
#-----ui side---

shiny::tagList(shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = 'SPECLEANR Package',
                                  shiny::tags$li(class='dropdown', shiny::tags$a(href='https://github.com/AnthonyBasooma/specleanr', shiny::icon('github'),
                                                                                 'GitHub'))),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(shinydashboard::menuItem(text = 'MERGING FILES', badgeColor = 'red',
                                                         tabName = 'about',
                                                         icon = shiny::icon('address-card')
    ),
    shiny::column(width = 12, shiny::h5('SEARCH FILES')), shiny::hr(),
    shinydashboard::box(width = 12,
                        # shiny::selectInput(inputId = 'folder',
                        #                    label = shiny::tags$span(style="color: black;","Folder"),
                        #                    choices = ''),
                        shinyFiles::shinyDirButton('folder', 'Select a folder', 'Please select a folder', FALSE),
                        shiny::hr(),
                        shiny::selectInput(inputId = 'files',
                                           label = shiny::tags$span(style="color: black;","Files"),
                                           choices = ''),
                        shiny::hr(),
                        shiny::actionButton('loaddata', 'File Load',icon = shiny::icon('upload'),
                                            style="background-color: #656696;
                        color: #fff"))
    )
  ),
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(shinydashboard::tabItem(tabName = 'about',
                                                     shiny::fluidRow(
                                                       shinydashboard::tabBox(width = 12, id = 'fileidbox',
                                                                              shiny::tabPanel('About', icon = shiny::icon('address-card'),
                                                                                              shiny::fluidRow(shiny::column(width=12, shiny::hr(), shiny::h4('DESCRIPTION OF THE PACKAGE')),
                                                                                                              shinydashboard::box(width = 12, style = "padding-top:20px", status = 'primary', solidHeader = F,
                                                                                                                                  shiny::p('This tools is used for merging bulk files moslty during
                                                                 development of biogeographic models such as the
                                                                 species distribution models, bioclimatic envelope models,
                                                                 ecological niche models. The files can be obatined from
                                                                 organised folder and only csv files are accepted.'))),
                                                                                              shiny::fluidRow(shinydashboard::box(width = 6,
                                                                                                                                  shiny::h5('Literature'), shiny::br(),
                                                                                                                                  shiny::p('Developing acceptable  biogeographical models, such as species
                                                    distribution models, requires high quality data. With the enormous
                                                    amount of data continuously archived from different freely accessible
                                                    online repositories from varying sources, the quality of the data is
                                                    heterogenous, which makes data quality checks indispensable.
                                                    Albeit geographical, temporal, and taxonomic data quality checks
                                                    are usually implemented in species distribution modeling workflows,
                                                    examining environmental outliers is seldom done, and methods for
                                                    outlier detection are selected in an ad hoc manner.Therefore, we introduce
                                                    the specleanr package that collates species ecological thresholds or ranges,
                                                    univariate, model-based, density-based, and clustering outlier detection
                                                    methods for species environmental predictors. The methods flag
                                                    different suspicious records as outliers, which are ensembled
                                                    to determine absolute outliers which are flagged in several methods.
                                                    We incorporate similarity measures to identify a method with the
                                                    highest average similarity, and we use majority votes principle
                                                    to identify the best method. The package was tested on 59 fish
                                                    species in the Danube River Basin with different environmental
                                                    predictors, including bioclimatic variables from WORLDCLIM and CHELSA
                                                    and hydromorphological variables from Hydrogrophy90m database.
                                                    Evidently, the best method varied across species and environmental
                                                    data, but was dominated by the Z-score test.')),
                                                                                                              shinydashboard::box(width = 6, shiny::h5('DATA CLEANING WORKFLOW SPECLEANR'),
                                                                                                                                  style = "padding-top:20px", shiny::hr(),
                                                                                                                                  shiny::HTML('<img src="pflow.png", height="650px", width="600px",
                                                           style="float:left"/>','<p style="color:red"></p>')))
                                                                              ),
                                                                              shiny::tabPanel(title = 'Raw Data', icon = shiny::icon('database'), value = 'rawchecks',
                                                                                              shiny::fluidRow(shinydashboard::box(width = 12, shiny::hr(),
                                                                                                                                  shiny::h4('Raw Data for precheck before approval for harmonisation',
                                                                                                                                            style = "font-weight: bold;"),
                                                                                                                                  shiny::hr(),
                                                                                                                                  DT::dataTableOutput('rawdata'),
                                                                                                                                  shiny::actionButton(inputId = 'approve', label = 'Approve Data', icon = shiny::icon('check'),
                                                                                                                                                      style="background-color: #156FE5;
                                                        color: white;
                                                        align='right';"))
                                                                                              )),
                                                                              shiny::tabPanel('Harmonise data files', icon = shiny::icon('screwdriver-wrench'), value = 'approvenav',
                                                                                              style = "background-color:'#5e504f;",
                                                                                              shiny::fluidRow(
                                                                                                shinydashboard::box(width = 12,
                                                                                                                    shiny::column(width = 2, solidHeader = FALSE,style = "background-color: #fff;",
                                                                                                                                  height = 70,
                                                                                                                                  shiny::selectInput(inputId = 'spp', label = 'Species', choices = '')
                                                                                                                    ),
                                                                                                                    shiny::column(width = 3, solidHeader = FALSE, style = "background-color:#fff;",
                                                                                                                                  height = 70,
                                                                                                                                  shiny::selectInput(inputId = 'lat', label = 'decimalLatitude', choices = '')),

                                                                                                                    shiny::column(width = 3, solidHeader = FALSE, style = "background-color:#fff;",
                                                                                                                                  height = 70,
                                                                                                                                  shiny::selectInput(inputId = 'lon', label = 'decimalLongitude', choices = '')),
                                                                                                                    shiny::column(width = 2, solidHeader = FALSE, style = "background-color:#fff;",
                                                                                                                                  height = 70,
                                                                                                                                  shiny::selectInput(inputId = 'country', label = 'Country', choices = '')),
                                                                                                                    shiny::column(width = 2, solidHeader = FALSE, style = "background-color:#fff;",
                                                                                                                                  height = 70,
                                                                                                                                  shiny::selectInput(inputId = 'date', label = 'Date', choices = ''))),
                                                                                                shiny::br(),
                                                                                                shiny::column(width = 3,
                                                                                                              shiny::actionButton('renamebtn', 'Confirm Raname',
                                                                                                                                  icon = shiny::icon('circle-check'),
                                                                                                                                  style="background-color: #156FE5; color: white; align='right';")))

                                                                              ),
                                                                              shiny::tabPanel('Final Data', icon = shiny::icon('people-roof'), value = 'confirmrename',
                                                                                              shiny::fluidRow(

                                                                                                shinydashboard::box(width = 12, shiny::hr(), shiny::h4('Save renamed data',
                                                                                                                                                       style = "font-weight: bold;"), shiny::hr(),
                                                                                                                    shiny::actionButton(inputId = 'selectandsave', label = 'Save individual file',
                                                                                                                                        icon = shiny::icon('floppy-disk'),
                                                                                                                                        style="background-color: #156FE5;
                                                    color: white;
                                                    align='right';")),
                                                                                                DT::dataTableOutput('fdata')
                                                                                              ),
                                                                              ),
                                                                              shiny::tabPanel('Merge, Load, and Review Data', icon = shiny::icon('list-check'),
                                                                                              shiny::fluidRow(
                                                                                                shiny::column(width =3, shiny::actionButton('mergeallfiles', label = 'Merge All Files',
                                                                                                                                            icon = shiny::icon('layer-group'),
                                                                                                                                            style="background-color: #98fb98; color: #000;")),
                                                                                                shiny::column(width = 3, shiny::actionButton('savemerged', label = 'Save Merged File', icon = shiny::icon('floppy-disk'),
                                                                                                                                             style="background-color: #522116; color: #fff;")),
                                                                                                shiny::column(width=3, shiny::actionButton('loadfile', label = 'Load and Review', icon = shiny::icon('layer-group'),
                                                                                                                                           style="background-color: #98fb98; color: #000;")),

                                                                                                shiny::column(width = 3, shiny::actionButton('rmfiles', label = 'Remove files', icon = shiny::icon('trash'),
                                                                                                                                             style="background-color: #FF0000; color: #FFF;"))), shiny::br(),
                                                                                              shiny::fluidRow(shinydashboard::box(width = 12, shiny::h4('Fully merged dataset for use', shiny::hr(),
                                                                                                                                                        style = "font-weight: bold;"),
                                                                                                                                  DT::dataTableOutput(outputId = 'mdata'),
                                                                                                                                  shiny::actionButton(inputId = 'visualize', label = 'Proceed to visualisation',
                                                                                                                                                      icon = shiny::icon('eye'),
                                                                                                                                                      style="background-color: #156FE5; color: #fff;")
                                                                                              ))
                                                                              ),
                                                                              shiny::tabPanel('Visualization', icon = shiny::icon('chart-simple'), value = 'visualchecks',
                                                                                              shiny::fluidRow(shinydashboard::box(width = 6, shiny::h5('Number of records per country'),
                                                                                                                                  shinydashboard::box(width = 6, shiny::h6('With duplicates'),
                                                                                                                                                      shiny::tableOutput(outputId = 'duptable')),
                                                                                                                                  shinydashboard::box(width = 6, shiny::h6('Without duplicates'),
                                                                                                                                                      shiny::tableOutput(outputId = 'wduptable'))),
                                                                                                              shinydashboard::box(width = 6, shiny::h5('Number of species per country'),
                                                                                                                                  shiny::tableOutput(outputId = 'sptable'))),
                                                                                              shiny::fluidRow(shinydashboard::box(width = 6),
                                                                                                              shinydashboard::box(width = 6)),
                                                                                              shiny::tableOutput(outputId = 'summarytable'))
                                                       )
                                                     )#end of fluid row
    )
    )#tabItems
  ) #end of body
),
shiny::tags$footer(shiny::p('Developed by Anthony Basooma'),'This project is funded under the AquaINFRA, Danube4ALL, and HR21 Doctoral School',
                   align = 'center', style="position: relative;
            bottom: 0;
            width: 100%;
            width:100%;
            height:70px;
            color: white;
            padding: 10px;
            background-color: #2F4F4F;
            z-index: 1000;"
)
)
