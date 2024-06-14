
#----server side----

function(input, output, session) {

  #initiate the parameters

  country <- decimalLatitude <- decimalLongitude <- species <-  NULL

  xdir <- getwd()
  #providing the absolute path within the server


  shiny::observe({
    shiny::updateSelectInput(session = session, inputId = 'folder',
                             choices = list.dirs(path = xdir, full.names = FALSE, recursive = TRUE))
  })

  shiny::observe({
    shiny::updateSelectInput(session = session, inputId = 'files',
                             choices = list.files(path = file.path(xdir, input$folder),pattern = 'csv$'))
  })

  readdata <- shiny::eventReactive(input$loaddata,{
    read.csv(file = paste0(rf,'/', input$folder,'/', input$files))

  })

  shiny::observeEvent(input$loaddata,{
    shiny::updateTabsetPanel(session =session, inputId = 'fileidbox', selected = 'rawchecks')
  })


  shiny::observeEvent(input$loaddata,{
    shiny::req(readdata())
    if(nrow(readdata()>1)){
      shiny::showModal(shiny::modalDialog(title = 'Data succesfuly loaded. Proceed to Raw Data window for visual checks.'))
    }else{
      shiny::showModal(shiny::modalDialog(title = 'Data files empty.'))
    }
  })

  shiny::observeEvent(input$visualize,{
    shiny::updateTabsetPanel(session =session, inputId = 'fileidbox', selected = 'visualchecks')
  })

  output$rawdata <- DT::renderDataTable(DT::datatable(readdata(),options = list(pageLength = 10, width="10%",
                                                                                scrollX = TRUE)))
  #approve

  shiny::observeEvent(input$approve,{
    shiny::updateTabsetPanel(session =session, inputId = 'fileidbox', selected = 'approvenav')
  })

  shiny::observeEvent(input$renamebtn,{
    shiny::updateTabsetPanel(session =session, inputId = 'fileidbox', selected = 'confirmrename')
  })

  shiny::observe({
    shiny::req(readdata())
    shiny::updateSelectInput(session, "spp", choices = colnames(readdata()))
  })

  shiny::observe({
    shiny::req(readdata())
    shiny::updateSelectInput(session, "lat", choices = colnames(readdata()))
  })

  shiny::observe({
    shiny::req(readdata())
    shiny::updateSelectInput(session, "lon", choices = colnames(readdata()))
  })

  shiny::observe({
    shiny::req(readdata())
    shiny::updateSelectInput(session, "country", choices = colnames(readdata()))
  })


  shiny::observe({
    shiny::req(readdata())
    shiny::updateSelectInput(session, "date", choices = colnames(readdata()))
  })

  finaldata <- shiny::eventReactive(input$renamebtn,{

    shiny::req(readdata())

    readdata() |> dplyr::rename(species= input$spp, decimalLatitude = input$lat,
                                decimalLongitude = input$lon, date = input$date,
                                country = input$country) |>
      dplyr::select(country, date, species,decimalLatitude, decimalLongitude)
  })

  output$fdata<- DT::renderDataTable(DT::datatable(finaldata(),options = list(pageLength = 10, width="10%", scrollX = TRUE)))

  #select and save
  shiny::observeEvent(input$selectandsave, {

    shiny::req(finaldata())

    filename = paste0(rf,'/', input$folder,'/', 'cleanedmerge', input$files)

    write.csv(x=finaldata(), file=filename, row.names = F)

    shiny::showModal(shiny::modalDialog(title = 'Data successfully saved. Continue if more files are to be merged.'))
  })

  #merge all files
  mergedata <- shiny::eventReactive(input$mergeallfiles,{

    ifiles <- list.files(path = rf, pattern = '^cleanedmerge', recursive = T)
    fmerge <- list()

    for (ifile in seq_along(ifiles)) {

      fname <- ifiles[ifile]

      fmerge[[ifile]] <- read.csv(file = file.path(rf, fname))

      mergedf <- do.call(rbind, fmerge)

      shiny::showModal(shiny::modalDialog(title = 'All files successfully merged.'))
    }
    return(mergedf)
  })

  #Saved merged files

  shiny::observeEvent(input$savemerged,{
    shiny::req(mergedata())

    write.csv(x=mergedata(), file = file.path(rf, 'speciesdatacompeletedfile.csv'), row.names = F)

    shiny::showModal(shiny::modalDialog(title = 'Merged file is saved successfully. Proceed to remove unnecessary files.'))

  })

  #Load file from computer into the system for visualization and final inspections

  finalmerged <- shiny::eventReactive(input$loadfile,{

    lf <- list.files(path = rf, pattern ="^speciesdatacompeletedfile", full.names = T)

    read.csv(file = lf)
  })


  #dom is document object model
  output$mdata<- DT::renderDataTable({
    DT::datatable(finalmerged(),extensions = 'Buttons',
                  options = list(pageLength = 10, width="10%", scrollX = TRUE,
                                 dom='Blfrtip', buttons=list('copy', 'csv', 'pdf',
                                                             'excel','print'))

    )
  })
  #remove cleaned file after merge
  shiny::observeEvent(input$rmfiles,{
    lfiles <- list.files(path = rf, pattern = '^cleanedmerge', recursive = T, full.names = TRUE)
    unlink(lfiles)
    shiny::showModal(shiny::modalDialog(title = 'You have removed the unnecessary files',easyClose = TRUE))
  })



  #add summary table of the data

  dupdata <- shiny::reactive({
    shiny::req(mergedata())

    mergedata() |>
      dplyr::group_by (country) |>
      dplyr::summarise(counts = length(country))
  })
  output$duptable <- shiny::renderTable(dupdata(), striped = T, hover = T)

  w_dupdata <- shiny::reactive({
    shiny::req(mergedata())

    mergedata() |>
      dplyr::group_by (country, species, decimalLatitude) |>
      dplyr::summarise(counts = length(species)) |>  dplyr::group_by(country) |>  dplyr::summarise(counts = length(country))
  })
  output$wduptable <- shiny::renderTable(w_dupdata(), striped = T, hover = T)

  #Number of species per country
  spdata <- shiny::reactive({
    shiny::req(mergedata())

    mergedata() |>
      dplyr::group_by (country, species) |>  dplyr::summarise(counts = length(species)) |>
      dplyr::group_by(country) |>  dplyr::summarise(counts = length(country))
  })
  output$sptable <- shiny::renderTable(spdata(), striped = T, hover = T)

}
