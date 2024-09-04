pur_sa <- path$recon_file |> sf::st_read()

# make attribute from csv
pur_reconciled_top <- PUR_dbfinal %>% 
  select(ID_rec, Rec_phase1b) %>% 
  filter(!Rec_phase1b %in% "unresolved_case") %>% 
  distinct(Rec_phase1b) %>% 
  tibble::rownames_to_column("ID") %>%
  mutate(ID = as.numeric(ID))


pur_reconciled_mid <- PUR_dbfinal %>% 
  select( ID=NEW_ID , Rec_phase1b) %>% 
  filter(Rec_phase1b %in% "unresolved_case") %>% 
  arrange(ID)


pur_reconciled_top %>% bind_rows(pur_reconciled_mid) 


# make a PUR attribute table ----------------------------------------------

# make attribute from shp
pur_sa <- pur_unresolved_vector %>% st_as_sf() %>% st_drop_geometry()

pur_reconciled_top <- pur_sa %>% rename(Rec_phase1b = Rec_phase2 ) %>% 
  select(ID_rec, Rec_phase1b) %>% 
  filter(!Rec_phase1b %in% "unresolved_case") %>% 
  distinct(Rec_phase1b) %>% 
  tibble::rownames_to_column("ID") %>%
  mutate(ID = as.numeric(ID))

pur_reconciled_mid <- pur_sa %>%
  rename(Rec_phase1b = Rec_phase2 ) %>% 
  select( ID, Rec_phase1b) %>% 
  filter(Rec_phase1b %in% "unresolved_case") %>% 
  arrange(ID)

pur_reconciled_top %>% bind_rows(pur_reconciled_mid) 

# make attribute_ori ------------------------------------------------------
# from csv 
readr::read_csv(path$attribute_db) %>% select(ID=NEW_ID,Rec_phase1b)

# from shp
pur_attribute_ori <- pur_sa %>% st_drop_geometry() %>% 
  rename(Rec_phase1b = Rec_phase2 ) %>% 
  select(ID, Rec_phase1b) %>% as_tibble()



ref_att_ori == pur_attribute_ori
