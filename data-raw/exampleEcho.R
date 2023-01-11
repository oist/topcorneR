exampleEcho <- Echo( source      = examplePlate
                   , destination = Plate("96", deadVolume = 1e4, maxVolume = 1e5)
                   , transducer  = exampleTransducer
                   , model       = "525")

usethis::use_data(exampleEcho, overwrite = TRUE)
