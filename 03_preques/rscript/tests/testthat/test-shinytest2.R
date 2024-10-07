library(shinytest2)
test_that("{shinytest2} recording: test_pre_ques", {
  app <-
    AppDriver$new(name = "test_pre_ques",
                  height = 643,
                  width = 971)
  app$upload_file(lc_t1 = "../../../../data/raster/tutupan_lahan_Bungo_1990r.tif")
  app$upload_file(lc_t2 = "../../../../data/raster/tutupan_lahan_Bungo_2005r.tif")
  app$upload_file(lookup_lc = "../../../../data/table/Tabel_landuse_Bungo.csv")
  app$set_inputs(zone_type = "shapefile")
  app$upload_file(
    zone_shapefile = c(
      "../../../../data/vector/unit_perencanaan_bungo.dbf",
      "../../../../data/vector/unit_perencanaan_bungo.prj",
      "../../../../data/vector/unit_perencanaan_bungo.shp",
      "../../../../data/vector/unit_perencanaan_bungo.shx"
    )
  )
  app$upload_file(lookup_trajectory = "../../../../data/table/tabel_trajectory_rules.csv")
  app$set_inputs(t1_year = 1990)
  app$set_inputs(t2_year = 2005)
  app$click("output_dir")
  app$click("run_analysis")
  app$expect_values()
})
