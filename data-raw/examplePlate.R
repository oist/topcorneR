examplePlate <-
  Plate( type       =   "384"
       , deadVolume =  10000
       , maxVolume  = 100000) |>
    setWell(Well("A01"), "dNTP",   100000) |>
    setWell(Well("A02"), "dNTP",   100000) |>
    setWell(Well("A03"), "buffer", 100000)

usethis::use_data(examplePlate, overwrite = TRUE)
