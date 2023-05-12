rm(list = ls())
setwd("C:/Users/marku/OneDrive/Skrivebord/MASTEROPPGAVE/Master_thesis/GITHUB REPOSITORY/Master_Thesis_Spring_2023/Data/")

mlUni <- read_csv("robustMNIR/idf_version/final/ML_score_unigram.csv") #run_19042023_all-three-filters

mlUni <- subset(mlUni, word != "igaming")
mlUni <- subset(mlUni, word != "scanship")
mlUni <- subset(mlUni, word != "easysmart")
mlUni <- subset(mlUni, word != "laptop")
mlUni <- subset(mlUni, word != "bjarnsholt")
mlUni <- subset(mlUni, word != "telenor")
mlUni <- subset(mlUni, word != "orkla")
mlUni <- subset(mlUni, word != "cest")
mlUni <- subset(mlUni, word != "sas")
mlUni <- subset(mlUni, word != "dof")
mlUni <- subset(mlUni, word != "eriksen")
mlUni <- subset(mlUni, word != "gate")
mlUni <- subset(mlUni, word != "bakkafrost")
mlUni <- subset(mlUni, word != "asteks")
mlUni <- subset(mlUni, word != "georgina")
mlUni <- subset(mlUni, word != "profit")


write.csv(mlUni, file = "robustMNIR/ML_score_unigram.csv")



mlBi <- read_csv("robustMNIR/idf_version/final/ML_score_bigram.csv")
mlBi <- subset(mlBi, !word %in% c("mobile_email", "announced_today", "western_bulk", "l??vestam_zwipe",
                                        "zwipe_pay", "e_skajem", "akva_group", "scanship_processes",
                                        "scanship_provides", "residuals_scanship", "taiwan_korea",
                                        "university_copenhagen", "holding_scanship", 
                                        "holding_tel", "cleaner_oceans", "bb_clients", 
                                        "shipping_rasmussengruppen", "aker_solutions", 
                                        "ose_plt", "solutions_softox", "crayon_help", 
                                        "inc_nio", "plt_today", "heatcube_thermal", 
                                        "semisubmersible_drilling", "semisubmersible_drilling",
                                        "gamborg_andreassen", "q_adjusted", 
                                        "offices_france", "tokyo_japan",
                                        "laptop", "contact_hofshagen",  "crayon_group",
                                        "polight_polight", "platform_drilling", 
                                        "skajem_jeshuddlestockcom", "scanship_holding",
                                        "l??vestam_zwipe", "iso_iso", "innovation_group",
                                        "date_wednesday", "health_care", "magnushofshagencrayoncom_crayon",
                                        "crayon_takle", "softox_technology",
                                        "l??vestam_zwipe", "elliptic_labs",
                                        "selected_zwipe", "novel_immunotherapies", "argentine_egypt",
                                        "brazil_argentina", "awilco_drillings", 
                                        "nyse_ose", "email_presentation", 
                                        "dronning_eufemias", "eufemias_gate",
                                        "container_ships", "mpc_container", 
                                        "lse_zen", "favre_ritufavrenextbiometricscom", 
                                        "konferansesenter_bryggetorget", 
                                        "waage_basili", "gross_margin",
                                        "energy_zenith", "k_arnet",
                                        "indirectly_japan", "sparebank_nordnorge",
                                        "zenith_company", "seabird_exploration",
                                        "nordic_nanovector", "prosafe_se",
                                        "larnaca_cyprus", "zename_zeniths",
                                        "exchange_zename", "larnaca_georgina",
                                        "prosafe_worlds", "lsezen_merkur",
                                        "vesting_schedule", "tel_cfo",
                                        "awilco_drilling", "tel_multinational", 
                                        "today_cest", "andr??_sloth",
                                        "dam_madsen", "dof_international",
                                        "dof_listed", "dof_international",
                                        "dofs_core", "modern_offshoresubsea", 
                                        "singapore_brazil","cfo_dronen",
                                        "dof_offers", "psv_charter", 
                                        "ose_nanov", "bermuda_borr",
                                        "approximately_cad", "zenith_int",
                                        "placement_zenith", "cfo_dam",
                                        "meeting_dof", "beggining_one",
                                        "date_srs"))

write.csv(mlBi, file = "robustMNIR/ML_score_bigram.csv")
