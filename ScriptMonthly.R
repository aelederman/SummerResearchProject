# Load libraries
library(tidyverse)
library(here)
library(purrr)
library(stringr)
library(lubridate)
library(zoo)
library(ggplot2)
library(did)


### This code is the best working code so far (6/26). It has concise and full data cleaning with two, accurate grouping logics
###   ...classic = excludes firms that enter post-treatment, strict = includes firms that enter post-treatment in a no_baseline taxpayer size group

# === 1) LOAD METADATA ===
# 1a) Tax codes
impuesto_codes <- read_delim(
  here("Documents/El Salvador Research/project/data/raw/impuesto.csv"),
  delim = ";", show_col_types = FALSE
) %>%
  rename(CODIMP = CODIGO) %>%
  mutate(CODIMP = as.character(CODIMP),
         NOMBRE = toupper(NOMBRE))

# 1b) Industry lookup (2-digit ACTECO → name)
actividad <- read_delim(
  here("Documents/El Salvador Research/project/data/raw/actividadEconomica.csv"),
  delim = ";", show_col_types = FALSE
) %>%
  set_names(~ str_remove_all(.x, '"')) %>%
  rename(CODIGO = CODIGO, INDUSTRY_NAME = NOMBRE) %>%
  mutate(CODIGO = as.character(CODIGO)) %>%
  filter(str_length(CODIGO) == 2)

# === 2) CSV LOADER WITH MUNICIPIO CLEANING ===
load_and_clean <- function(path) {
  read_delim(path, delim = ";", show_col_types = FALSE) %>%
    set_names(~ str_remove_all(.x, '"')) %>%
    mutate(across(c(ACTECO, CODIMP, MUNICIPIO, DEPARTAMENTO),
                  ~ str_trim(str_remove_all(as.character(.), '"')))) %>%
    mutate(
      ANIO         = as.integer(ANIO),
      MES          = as.integer(MES),
      ACTECO       = str_pad(ACTECO, width = 6, side = "left", pad = "0"),
      DEPARTAMENTO = str_pad(DEPARTAMENTO, width = 2, side = "left", pad = "0"),
      MUNICIPIO    = if_else(
        str_detect(MUNICIPIO, "^[0-9]{1,2}$"),
        str_c(DEPARTAMENTO, str_pad(MUNICIPIO, width = 2, side = "left", pad = "0")),
        str_pad(MUNICIPIO, width = 4, side = "left", pad = "0")
      ),
      VALOR = as.numeric(VALOR)
    )
}

# === 3) LOAD & FILTER TAX FILES ===
files <- c(
  "DGT_IMPUESTO1616094232377-0112-2021.csv",
  "DGT_IMPUESTO1648495403637-012022-122022.csv",
  "DGT_IMPUESTO1707167152598-122023-122023.csv",
  "DGT_IMPUESTO1738685926586-122024-122024.csv",
  "DGT_IMPUESTO1750427962354-052025-052025.csv"
) %>%
  map_chr(~ here("Documents/El Salvador Research/project/data/raw", .x))
raw_data <- map_dfr(files, load_and_clean)

# === 4) BUILD FIRM-LEVEL CROSSWALK ===
df_class <- raw_data %>%
  mutate(
    firm_id = str_c(ACTECO, MUNICIPIO, sep = "_"),
    code2d  = str_sub(ACTECO, 1, 2)
  ) %>%
  left_join(actividad, by = c("code2d" = "CODIGO")) %>%
  mutate(industry = coalesce(INDUSTRY_NAME, "Other"))

# === 5) MUNICIPIO NAMES ===
municipio_lookup <- tribble(
  ~MUNICIPIO, ~municipio_name,
  # Dept 01
  "0101","Ahuachapán","0102","Apaneca","0103","Atiquizaya","0104","Concepción de Ataco",
  "0105","El Refugio","0106","Guaymango","0107","Jujutla","0108","San Francisco Menéndez",
  "0109","San Lorenzo","0110","San Pedro Puxtla","0111","Tacuba","0112","Turín",
  # Dept 02
  "0201","Candelaria de la Frontera","0202","Coatepeque","0203","Chalchuapa","0204","El Congo",
  "0205","El Porvenir","0206","Masahuat","0207","Metapán","0208","San Antonio Pajonal",
  "0209","San Sebastián Salitrillo","0210","Santa Ana","0211","Santa Rosa Guachipilín",
  "0212","Santiago de la Frontera","0213","Texistepeque",
  # Dept 03
  "0301","Acajutla","0302","Armenia","0303","Caluco","0304","Cuisnahuat","0305","Santa Isabel Ishuatán",
  "0306","Izalco","0307","Juayúa","0308","Nahuizalco","0309","Nahuilingo","0310","Salcoatitán",
  "0311","San Antonio del Monte","0312","San Julián","0313","Santa Catarina Masahuat",
  "0314","Santo Domingo de Guzmán","0315","Sonsonate","0316","Sonzacate",
  # Dept 04
  "0401","Agua Caliente","0402","Arcatao","0403","Azacualpa","0404","Citalá","0405","Comalapa",
  "0406","Concepción Quezaltepeque","0407","Chalatenango","0408","Dulce Nombre de María",
  "0409","El Carrizal","0410","El Paraíso","0411","La Laguna","0412","La Palma","0413","La Reina",
  "0414","Las Vueltas","0415","Nombre de Jesús","0416","Nueva Concepción","0417","Nueva Trinidad",
  "0418","Ojos de Agua","0419","Potonico","0420","San Antonio la Cruz","0421","San Antonio los Ranchos",
  "0422","San Fernando","0423","San Francisco Lempa","0424","San Francisco Morazán","0425","San Ignacio",
  "0426","San Isidro Labrador","0427","San José Cancasque","0428","San José las Flores","0429","San Luis del Carmen",
  "0430","San Miguel de Mercedes","0431","San Rafael","0432","Santa Rita","0433","Tejutla",
  # Dept 05
  "0501","Antiguo Cuscatlán","0502","Ciudad Arce","0503","Colón","0504","Comasagua","0505","Chiltiupán",
  "0506","Huizúcar","0507","Jayaque","0508","Jicalapa","0509","La Libertad","0510","Nuevo Cuscatlán",
  "0511","Santa Tecla","0512","Quezaltepeque","0513","Sacacoyo","0514","San José Villanueva",
  "0515","San Juan Opico","0516","San Matías","0517","San Pablo Tacachico","0518","Tamanique",
  "0519","Talnique","0520","Teotepeque","0521","Tepecoyo","0522","Zaragoza",
  # Dept 06
  "0601","Aguilares","0602","Apopa","0603","Ayutuxtepeque","0604","Cuscatancingo","0605","El Paisnal",
  "0606","Guazapa","0607","Ilopango","0608","Mejicanos","0609","Nejapa","0610","Panchimalco",
  "0611","Rosario de Mora","0612","San Marcos","0613","San Martín","0614","San Salvador",
  "0615","Santiago Texacuangos","0616","Santo Tomás","0617","Soyapango","0618","Tonacatepeque",
  "0619","Ciudad Delgado",
  # Dept 07
  "0701","Candelaria","0702","Cojutepeque","0703","El Carmen","0704","El Rosario","0705","Monte San Juán",
  "0706","Oratorio de Concepción","0707","San Bartolomé Perulapía","0708","San Cristóbal",
  "0709","San José Guayabal","0710","San Pedro Perulapán","0711","San Rafael Cedros","0712","San Ramón",
  "0713","Santa Cruz Analquito","0714","Santa Cruz Michapa","0715","Suchitoto","0716","Tenancingo",
  # Dept 08
  "0801","Cuyultitán","0802","El Rosario","0803","Jerusalén","0804","Mercedes La Ceiba","0805","Olocuilta",
  "0806","Paraíso de Osorio","0807","San Antonio Masahuat","0808","San Emigdio","0809","San Francisco Chinameca",
  "0810","San Juan Nonualco","0811","San Juan Talpa","0812","San Juan Tepezontes","0813","San Luis Talpa",
  "0814","San Miguel Tepezontes","0815","San Pedro Masahuat","0816","San Pedro Nonualco",
  "0817","San Rafael Obrajuelo","0818","Santa María Ostuma","0819","Santiago Nonualco","0820","Tapalhuaca",
  "0821","Zacatecoluca","0822","San Luis La Herradura",
  # Dept 09
  "0901","Cinquera","0902","Guacotecti","0903","Ilobasco","0904","Jutiapa","0905","San Isidro",
  "0906","Sensuntepeque","0907","Tejutepeque","0908","Victoria","0909","Villa Dolores",
  # Dept 10
  "1001","Apastepeque","1002","Guadalupe","1003","San Cayetano Istepeque","1004","Santa Clara",
  "1005","Santo Domingo","1006","San Esteban Catarina","1007","San Idelfonso","1008","San Lorenzo",
  "1009","San Sebastián","1010","San Vicente","1011","Tecoluca","1012","Tepetitán","1013","Verapaz",
  # Dept 11
  "1101","Alegría","1102","Berlín","1103","California","1104","Concepción Batres","1105","El Triunfo",
  "1106","Ereguayquín","1107","Estanzuelas","1108","Jiquilisco","1109","Jucuapa","1110","Jucuarán",
  "1111","Mercedes Umaña","1112","Nueva Granada","1113","Ozatlán","1114","Puerto El Triunfo",
  "1115","San Agustín","1116","San Buenaventura","1117","San Dionisio","1118","Santa Elena",
  "1119","San Francisco Javier","1120","Santa María","1121","Santiago de María","1122","Tecapán",
  "1123","Usulután",
  # Dept 12
  "1201","Carolina","1202","Ciudad Barrios","1203","Comacarán","1204","Chapeltique","1205","Chinameca",
  "1206","Chirilagua","1207","El Tránsito","1208","Lolotique","1209","Moncagua","1210","Nueva Guadalupe",
  "1211","Nuevo Edén de San Juan","1212","Quelepa","1213","San Antonio del Mosco","1214","San Gerardo",
  "1215","San Jorge","1216","San Luis de la Reina","1217","San Miguel","1218","San Rafael Oriente",
  "1219","Sesori","1220","Uluazapa",
  # Dept 13
  "1301","Arambala","1302","Cacaopera","1303","Corinto","1304","Chilanga","1305","Delicia de Concepción",
  "1306","El Divisadero","1307","El Rosario","1308","Gualococti","1309","Guatajiagua","1310","Joateca",
  "1311","Jocoaitique","1312","Jocoro","1313","Lolotiquillo","1314","Meanguera","1315","Osicala",
  "1316","Perquín","1317","San Carlos","1318","San Fernando","1319","San Francisco Gotera",
  "1320","San Isidro","1321","San Simón","1322","Sensembra","1323","Sociedad","1324","Torola",
  "1325","Yamabal","1326","Yoloayquín",
  # Dept 14
  "1401","Anamorós","1402","Bolívar","1403","Concepción de Oriente","1404","Conchagua",
  "1405","El Carmen","1406","El Sauce","1407","Intipucá","1408","La Unión","1409","Lislique",
  "1410","Meanguera del Golfo","1411","Nueva Esparta","1412","Pasaquina","1413","Polorós",
  "1414","San Alejo","1415","San José La Fuente","1416","Santa Rosa de Lima","1417","Yayantique",
  "1418","Yucuaiquín"
)


# === 6) BUILD BALANCED MONTHLY PANEL ===
panel_obs <- df_class %>%
  group_by(firm_id, ANIO, MES) %>%
  summarise(
    revenue0  = sum(VALOR, na.rm = TRUE),
    MUNICIPIO = first(MUNICIPIO),
    .groups   = "drop"
  ) %>%
  mutate(
    year_month = ANIO * 100 + MES,
    date       = ymd(paste(ANIO, MES, 1, sep = "-"))
  )
all_firms  <- unique(panel_obs$firm_id)
all_months <- sort(unique(panel_obs$year_month))
monthly_data <- expand_grid(firm_id = all_firms, year_month = all_months) %>%
  left_join(panel_obs, by = c("firm_id","year_month")) %>%
  left_join(
    panel_obs %>% distinct(firm_id, MUNICIPIO) %>% rename(MUNICIPIO_fm = MUNICIPIO),
    by = "firm_id"
  ) %>%
  mutate(
    ANIO        = floor(year_month/100),
    MES         = year_month %% 100,
    MUNICIPIO   = coalesce(MUNICIPIO, MUNICIPIO_fm),
    revenue0    = replace_na(revenue0, 0),
    remitted    = as.integer(revenue0 > 0),
    revenue     = revenue0 + 1,
    Y           = log(revenue),
    date        = ymd(paste(ANIO, MES, 1, sep = "-"))
  ) %>%
  select(-MUNICIPIO_fm) %>%
  left_join(municipio_lookup, by = "MUNICIPIO") %>%
  mutate(municipio_name = coalesce(municipio_name, "Unknown"))

# === 7) PRE-TREATMENT SIZE GROUPS ===
baseline_end <- ymd("2023-01-01")

# strict grouping logic (new)
first_obs <- panel_obs %>%
  group_by(firm_id) %>%
  summarise(first_date = min(date), .groups = "drop")
firm_baseline <- panel_obs %>%
  filter(date <= baseline_end) %>%
  group_by(firm_id) %>%
  summarise(
    avg_rev_pre = mean(revenue0, na.rm = TRUE) * 12,
    months_obs  = sum(revenue0 > 0, na.rm = TRUE),
    .groups     = "drop"
  )
df_class <- df_class %>%
  left_join(first_obs, by = "firm_id") %>%
  left_join(firm_baseline, by = "firm_id") %>%
  mutate(
    size_group_strict = case_when(
      first_date  > baseline_end ~ "no_baseline",
      avg_rev_pre > 1e6         ~ "large",
      avg_rev_pre > 1e5         ~ "medium",
      TRUE                       ~ "small"
    )
  )

# classic grouping logic (old, excluding post-baseline entrants)
classic_baseline <- panel_obs %>%
  filter(date <= baseline_end) %>%
  group_by(firm_id) %>%
  summarise(
    avg_rev_pre_classic = mean(revenue0, na.rm = TRUE) * 12,
    .groups = "drop"
  )
df_class <- df_class %>%
  left_join(classic_baseline, by = "firm_id") %>%
  mutate(
    size_group_classic = case_when(
      is.na(avg_rev_pre_classic)    ~ NA_character_,  # drop post-baseline firms
      avg_rev_pre_classic > 1e6     ~ "large",
      avg_rev_pre_classic > 1e5     ~ "medium",
      TRUE                           ~ "small"
    )
  )

# === 8) ASSIGN 10-GROUPS BASED ON PRE-TREATMENT REVENUE ===
target_sizes <- tibble(
  group_id    = paste0("G", 1:10),
  target_size = c(233,239,249,345,657,701,704,4982,14286,15000)
)
firm_groups <- df_class %>%
  distinct(firm_id, avg_rev_pre) %>%
  filter(!is.na(avg_rev_pre)) %>%
  arrange(desc(avg_rev_pre)) %>%
  mutate(row_id = row_number()) %>%
  mutate(
    group_id = cut(
      row_id,
      breaks         = c(0, cumsum(target_sizes$target_size)),
      labels         = target_sizes$group_id,
      include.lowest = TRUE
    )
  ) %>%
  select(firm_id, group_id)

# === 8b) FILTER FOR VAT REVENUE ONLY ===
vat_codes <- impuesto_codes %>%
  filter(str_detect(NOMBRE, "IVA")) %>%
  pull(CODIMP)

df_class <- df_class %>%
  filter(CODIMP %in% vat_codes)

# === 9) DEFINE GROUP TIMING ===
group_timing <- tibble(
  group_id      = paste0("G", 1:10),
  notified_date = ymd(c(
    "2023-02-01","2023-05-01","2023-08-01","2023-12-01",
    "2024-02-01","2024-05-01","2024-08-01","2024-12-01",
    "2025-02-01","2025-05-01"
  )),
  treat_date    = ymd(c(
    "2023-07-01","2023-10-01","2024-01-01","2024-04-01",
    "2024-07-01","2024-10-01","2025-01-15","2025-04-01",
    "2025-07-01","2025-10-01"
  ))
)
# 10) BUILD ONE-ROW-PER-FIRM LOOKUP
firm_info <- firm_groups %>% left_join(group_timing, by = "group_id")
# 11) MERGE INTO df_class
df_class <- df_class %>% left_join(firm_info, by = "firm_id")

# 12) MERGE INTO PANEL AND JOIN GROUPS
monthly_data <- monthly_data %>%
  mutate(
    size_group_classic = df_class$size_group_classic[match(firm_id, df_class$firm_id)],
    size_group_strict  = df_class$size_group_strict[match(firm_id, df_class$firm_id)]
  ) %>%
  left_join(firm_info, by = "firm_id") %>%
  mutate(
    notified_ym     = year(notified_date)*100 + month(notified_date),
    treat_ym        = year(treat_date)*100 + month(treat_date),
    period          = as.integer(factor(year_month, sort(unique(year_month)))),
    notified_period = match(notified_ym, sort(unique(year_month))),
    treat_period    = match(treat_ym,    sort(unique(year_month))),
    id              = as.integer(factor(firm_id))
  )

# 12b) DROP OCTOBER 2023 AS MISSING
monthly_data <- monthly_data %>%
  filter(!(ANIO == 2023 & MES == 10))

# -- adjustable event-time window --
pre_event  <- -12      # months before treatment
post_event <- Inf      # months after treatment (Inf = all)
is_panel <- FALSE

# 13) BASIC EVENT STUDY (classic grouping only)
att_1 <- att_gt(
  yname         = "Y",
  tname         = "period",
  idname        = "id",
  gname         = "treat_period",
  data          = filter(monthly_data, !is.na(size_group_classic)),
  control_group = "notyettreated",
  panel         = is_panel,
  clustervars   = "id"
)

# plot ATT (classic)
ggdid(att_1) +
  labs(
    title = "Callaway–Sant’Anna: ATT of log(revenue) (Classic Groups)",
    x     = "Months Since Treatment",
    y     = "ATT (log revenue)"
  ) +
  theme_minimal()

att_2 <- att_gt(
  yname         = "Y",
  tname         = "period",
  idname        = "id",
  gname         = "notified_period",
  data          = filter(monthly_data, !is.na(size_group_classic)),
  control_group = "notyettreated",
  panel         = is_panel,
  clustervars   = "id"
)

# plot ATT (classic)
ggdid(att_2) +
  labs(
    title = "Callaway–Sant’Anna: ATT of log(revenue) after notification (Classic Groups)",
    x     = "Months Since Notified",
    y     = "ATT (log revenue)"
  ) +
  theme_minimal()

# 14) PLOT TOTAL REVENUE BY CLASSIC SIZE GROUP
rev_by_size <- monthly_data %>%
  filter(!is.na(size_group_classic)) %>%
  group_by(date, size_group_classic) %>%
  summarise(total_rev = sum(revenue0, na.rm = TRUE), .groups = "drop") %>%
  tidyr::complete(date, size_group_classic, fill = list(total_rev = 0))

ggplot(rev_by_size, aes(x = date, y = total_rev, color = size_group_classic)) +
  geom_line(size = 1) +
  labs(
    title = "Total Monthly VAT Revenue by Classic Size Group",
    x     = "Date",
    y     = "Total Revenue (USD$)",
    color = "Size (Classic)"
  ) +
  theme_minimal()

# 15) PLOT TOTAL REVENUE BY STRICT SIZE GROUP
rev_by_size <- monthly_data %>%
  filter(!is.na(size_group_strict)) %>%
  group_by(date, size_group_strict) %>%
  summarise(total_rev = sum(revenue0, na.rm = TRUE), .groups = "drop") %>%
  tidyr::complete(date, size_group_strict, fill = list(total_rev = 0))

ggplot(rev_by_size, aes(x = date, y = total_rev, color = size_group_strict)) +
  geom_line(size = 1) +
  labs(
    title = "Total Monthly VAT Revenue by Strict Size Group",
    x     = "Date",
    y     = "Total Revenue (USD$)",
    color = "Size (Strict)"
  ) +
  theme_minimal()





# 16) MORE EVENT STUDIES (Classic grouping only)

library(did)
library(ggplot2)
library(dplyr)

# -- ensure industry is in your panel for all analyses --
monthly_data <- monthly_data %>%
  left_join(
    df_class %>% distinct(firm_id, industry),
    by = "firm_id"
  )

# -- Global Dynamic Event Study for Log Revenue (Classic Groups) --
att_global <- att_gt(
  yname         = "Y",
  tname         = "period",
  idname        = "id",
  gname         = "treat_period",
  data          = filter(monthly_data, !is.na(size_group_classic)),
  control_group = "notyettreated",
  panel         = is_panel,
  clustervars   = "id"
)
dyn_global <- aggte(
  att_global,
  type  = "dynamic",
  min_e = pre_event,
  max_e = post_event
)
print(
  ggdid(dyn_global) +
    labs(
      title = "Dynamic Event Study: ATT of log(revenue) (Classic Groups)",
      x     = "Event Time (months)",
      y     = "ATT (log revenue)"
    ) +
    theme_minimal()
)

### Dynamic event study for notification (Classic Groups)
# 1) Estimate cohort‐specific ATTs using notified_period
att_notif <- att_gt(
  yname         = "Y",
  tname         = "period",
  idname        = "id",
  gname         = "notified_period",
  data          = filter(monthly_data, !is.na(size_group_classic)),
  control_group = "notyettreated",
  panel         = is_panel,
  clustervars   = "id"
)

# 2) Turn it into a dynamic event‐study
dyn_notif <- aggte(
  att_notif,
  type  = "dynamic",
  min_e = pre_event,
  max_e = post_event
)

# 3) Plot the event‐study
ggdid(dyn_notif) +
  labs(
    title = "Dynamic Event Study: ATT of log(revenue) after notification (Classic Groups)",
    x     = "Months Since Notification",
    y     = "ATT (log revenue)"
  ) +
  theme_minimal()


# --- Dynamic event‐study for remittance after notification (Classic Groups) ---
# 1) Estimate cohort‐specific ATTs on remitted outcome
att_rem_notif <- att_gt(
  yname         = "remitted",              # binary remittance indicator
  tname         = "period",                
  idname        = "id",
  gname         = "notified_period",       # use notification as “treatment”
  data          = filter(monthly_data, !is.na(size_group_classic)),
  control_group = "notyettreated",
  panel         = is_panel,
  clustervars   = "id"
)

# 2) Turn into a dynamic event‐study (use same pre/post window you set)
dyn_rem_notif <- aggte(
  att_rem_notif,
  type  = "dynamic",
  min_e = pre_event,
  max_e = post_event
)

# 3) Plot
ggdid(dyn_rem_notif) +
  labs(
    title = "ATT on Remittance after Notification (Classic Groups)",
    x     = "Months Since Notification",
    y     = "ATT Pr(remitted)"
  ) +
  theme_minimal()


# --- Dynamic event‐study for remittance after treatment, anad notification (Strict Groups) ---
att_rem_strict <- att_gt(
  yname         = "remitted",
  tname         = "period",
  idname        = "id",
  gname         = "treat_period",
  data          = filter(monthly_data, !is.na(size_group_strict)),
  control_group = "notyettreated",
  panel         = is_panel,
  clustervars   = "id"
)

dyn_rem_strict <- aggte(att_rem_strict, type = "dynamic", min_e = pre_event, max_e = post_event)

ggdid(dyn_rem_strict) +
  labs(
    title = "ATT on Remittance (Strict Groups)",
    x     = "Event Time (months)",
    y     = "ATT Pr(remitted)"
  ) +
  theme_minimal()

att_rem_notif_strict <- att_gt(
  yname         = "remitted",
  tname         = "period",
  idname        = "id",
  gname         = "notified_period",
  data          = filter(monthly_data, !is.na(size_group_strict)),
  control_group = "notyettreated",
  panel         = is_panel,
  clustervars   = "id"
)

dyn_rem_notif_strict <- aggte(att_rem_notif_strict, type = "dynamic", min_e = pre_event, max_e = post_event)

ggdid(dyn_rem_notif_strict) +
  labs(
    title = "ATT on Remittance after Notification (Strict Groups)",
    x     = "Months Since Notification",
    y     = "ATT Pr(remitted)"
  ) +
  theme_minimal()


# -- Classic Sample: Log-Revenue Pairs (Large vs Medium & Medium vs Small) --
classic_pairs <- list(
  c("large","medium"),
  c("medium","small")
)
for (grp in classic_pairs) {
  lbl  <- paste(grp, collapse = "_vs_")
  df_i <- monthly_data %>%
    filter(!is.na(size_group_classic),
           size_group_classic %in% grp)
  att  <- att_gt(
    yname         = "Y",
    tname         = "period",
    idname        = "id",
    gname         = "treat_period",
    data          = df_i,
    control_group = "notyettreated",
    panel         = is_panel,
    clustervars   = "id"
  )
  dyn  <- aggte(att, type = "dynamic", min_e = pre_event, max_e = post_event)
  print(
    ggdid(dyn) +
      labs(
        title = paste("Log Revenue ATT (classic):", lbl),
        x     = "Event Time (months)",
        y     = "ATT (log revenue)"
      ) +
      theme_minimal()
  )
}

# # -- Log-Revenue by Top-10 Industries (Classic Groups Only) --
# top10_inds <- df_class %>%
#   count(industry, sort = TRUE) %>%
#   slice_head(n = 10) %>%
#   pull(industry)
# 
# for (ind in top10_inds) {
#   df_i_cl <- monthly_data %>%
#     filter(!is.na(size_group_classic), industry == ind)
#   att_cl <- att_gt(
#     yname         = "Y",
#     tname         = "period",
#     idname        = "id",
#     gname         = "treat_period",
#     data          = df_i_cl,
#     control_group = "notyettreated",
#     panel         = is_panel,
#     clustervars   = "id"
#   )
#   dyn_cl <- aggte(att_cl, type = "dynamic", min_e = pre_event, max_e = post_event)
#   print(
#     ggdid(dyn_cl) +
#       labs(
#         title = paste("Log Revenue ATT classic –", ind),
#         x     = "Event Time (months)",
#         y     = "ATT (log revenue)"
#       ) +
#       theme_minimal()
#   )
# }
# 
# 
# # --- Dynamic Event Study for Top-5 Municipalities (Classic Groups Only) ---
# # 1) Identify top 5 munis by total revenue
# top5_muns <- monthly_data %>%
#   filter(!is.na(size_group_classic)) %>%
#   group_by(municipio_name) %>%
#   summarise(total_rev = sum(revenue0, na.rm = TRUE), .groups = "drop") %>%
#   arrange(desc(total_rev)) %>%
#   slice_head(n = 5) %>%
#   pull(municipio_name)
# 
# # 2) Loop over each muni and run dynamic ATT
# for (mun in top5_muns) {
#   df_m <- monthly_data %>%
#     filter(!is.na(size_group_classic), municipio_name == mun)
#   
#   att_m <- att_gt(
#     yname         = "Y",
#     tname         = "period",
#     idname        = "id",
#     gname         = "treat_period",
#     data          = df_m,
#     control_group = "notyettreated",
#     panel         = is_panel,
#     clustervars   = "id"
#   )
#   
#   dyn_m <- aggte(
#     att_m,
#     type  = "dynamic",
#     min_e = pre_event,
#     max_e = post_event
#   )
#   
#   print(
#     ggdid(dyn_m) +
#       labs(
#         title = paste("Log Revenue ATT classic –", mun),
#         x     = "Event Time (months)",
#         y     = "ATT (log revenue)"
#       ) +
#       theme_minimal()
#   )
# }



# -- Remittance Event-Study (Classic Groups Only) --
att_rem_c <- att_gt(
  yname         = "remitted",
  tname         = "period",
  idname        = "id",
  gname         = "treat_period",
  data          = filter(monthly_data, !is.na(size_group_classic)),
  control_group = "notyettreated",
  panel         = is_panel,
  clustervars   = "id"
)
dyn_rem_c <- aggte(att_rem_c, type = "dynamic", min_e = pre_event, max_e = post_event)
print(
  ggdid(dyn_rem_c) +
    labs(
      title = "ATT on Remittance (classic)",
      x     = "Event Time (months)",
      y     = "ATT Pr(remitted)"
    ) +
    theme_minimal()
)





### Revenue Plot

library(scales) # for comma formatting, rather than scientific notation on the y-axis

# 1) Annotate monthly_data with group info
tool_data <- monthly_data %>%
  mutate(
    group_id      = firm_info$group_id[      match(firm_id, firm_info$firm_id)],
    notified_date = firm_info$notified_date[ match(firm_id, firm_info$firm_id)],
    treat_date    = firm_info$treat_date[    match(firm_id, firm_info$firm_id)]
  ) %>%
  filter(!is.na(group_id)) %>%
  group_by(group_id, date) %>%
  summarise(
    total_rev     = sum(revenue0, na.rm = TRUE),
    notified_date = first(notified_date),
    treat_date    = first(treat_date),
    .groups       = "drop"
  )

# 2) Plot each group’s revenue series with vertical lines
ggplot(tool_data, aes(x = date, y = total_rev)) +
  geom_line() +
  geom_vline(aes(xintercept = notified_date), linetype = "dashed") +
  geom_vline(aes(xintercept = treat_date),    linetype = "solid") +
  labs(
    title    = "Monthly VAT Revenue by Group",
    subtitle = "Dashed = notification date; Solid = treatment date",
    x        = "Date",
    y        = "Total Revenue (USD$)"
  ) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~group_id, scales = "free_y", ncol = 2, drop = FALSE) +
  theme_minimal()


### Remittance plots

library(patchwork)   # for easy stacking of ggplots

# — 1) Classic: count only baseline firms in each of the 10 groups —
remit_classic <- monthly_data %>%
  mutate(
    group_id      = factor(firm_info$group_id[      match(firm_id, firm_info$firm_id)],
                           levels = paste0("G", 1:10)),
    notified_date = firm_info$notified_date[      match(firm_id, firm_info$firm_id)],
    treat_date    = firm_info$treat_date[         match(firm_id, firm_info$firm_id)]
  ) %>%
  filter(!is.na(group_id)) %>%
  group_by(group_id, date) %>%
  summarise(
    n_remit       = sum(remitted, na.rm = TRUE),
    notified_date = first(notified_date),
    treat_date    = first(treat_date),
    .groups       = "drop"
  )

p_classic <- ggplot(remit_classic, aes(x = date, y = n_remit)) +
  geom_line() +
  geom_vline(aes(xintercept = notified_date), linetype = "dashed") +
  geom_vline(aes(xintercept = treat_date),    linetype = "solid") +
  facet_wrap(~group_id, scales = "free_y", ncol = 2, drop = FALSE) +
  labs(
    title    = "Number of Firms Remitting by Classic Group",
    subtitle = "Baseline firms only — Dashed = notification, Solid = treatment",
    x        = NULL,
    y        = "Count of Remitting Firms"
  ) +
  theme_minimal()

# — 2) Strict: count all firms (including post-baseline entrants) by size_group_strict —
remit_strict <- monthly_data %>%
  mutate(
    strict_group  = factor(size_group_strict,
                           levels = c("small","medium","large","no_baseline")),
    notified_date = firm_info$notified_date[      match(firm_id, firm_info$firm_id)],
    treat_date    = firm_info$treat_date[         match(firm_id, firm_info$firm_id)]
  ) %>%
  filter(!is.na(strict_group)) %>%
  group_by(strict_group, date) %>%
  summarise(
    n_remit       = sum(remitted, na.rm = TRUE),
    notified_date = first(notified_date),
    treat_date    = first(treat_date),
    .groups       = "drop"
  )

p_strict <- ggplot(remit_strict, aes(x = date, y = n_remit)) +
  geom_line() +
  geom_vline(aes(xintercept = notified_date), linetype = "dashed") +
  geom_vline(aes(xintercept = treat_date),    linetype = "solid") +
  facet_wrap(~strict_group, scales = "free_y", ncol = 2) +
  labs(
    title    = "Number of Firms Remitting by Strict Group",
    subtitle = "Includes post‐baseline entrants — Dashed = notification, Solid = treatment",
    x        = "Date",
    y        = "Count of Remitting Firms"
  ) +
  theme_minimal()

# — 3) Stack them vertically —
p_classic / p_strict



### How many firms?

#how many unique firms?
unique_firms <- monthly_data %>%
  filter(!is.na(firm_id)) %>%
  summarise(n_unique_firms = n_distinct(firm_id))
print(unique_firms)
# how many firms per group?
firm_counts <- monthly_data %>%
  filter(!is.na(group_id)) %>%
  group_by(group_id) %>%
  summarise(n_firms = n_distinct(firm_id), .groups = "drop")
print(firm_counts)



### 10.2023 data error checks
library(dplyr)
library(ggplot2)

# 1) Merge in group_id if you haven’t already
md <- monthly_data %>%
  mutate(
    group_id = firm_info$group_id[match(firm_id, firm_info$firm_id)]
  )

# 2) Tabulate revenue‐zero incidence in Oct 2023 vs Sep+Nov
zero_tab <- md %>%
  filter(date %in% as.Date(c("2023-09-01","2023-10-01","2023-11-01"))) %>%
  group_by(group_id, date) %>%
  summarise(
    n_firms    = n_distinct(firm_id),
    n_zero     = sum(revenue0 == 0),
    pct_zero   = 100 * n_zero / n_firms,
    avg_rev    = mean(revenue0, na.rm = TRUE),
    median_rev = median(revenue0, na.rm = TRUE),
    .groups    = "drop"
  )
print(zero_tab, n=100)

# 3) Plot time-series of % zeros by group to see spike
md %>%
  filter(date >= "2023-01-01", date <= "2023-12-01") %>%
  group_by(group_id, date) %>%
  summarise(pct_zero = 100 * mean(revenue0 == 0), .groups = "drop") %>%
  ggplot(aes(date, pct_zero, color = group_id)) +
  geom_line() +
  labs(y = "% of Firms with Zero Revenue", title = "Zero‐Revenue Spike in Oct ‘23") +
  theme_minimal()

# 4) Check overall panel completeness (# firms observed per group‐month)
md %>%
  group_by(group_id, date) %>%
  summarise(n_obs = n_distinct(firm_id), .groups = "drop") %>%
  ggplot(aes(date, n_obs, color = group_id)) +
  geom_line() +
  labs(y = "# Firms Reporting", title = "Panel Drop-off in Oct ‘23") +
  theme_minimal()







# a) How many actual rows do you have in raw_data by month?
panel_obs %>%
  count(ANIO, MES) %>%
  mutate(date = ymd(paste(ANIO, MES, 1, sep = "-"))) %>%
  ggplot(aes(date, n)) +
  geom_line() +
  labs(
    title = "Actual raw_data rows by month",
    x     = "Month",
    y     = "Number of Firm–Month Records"
  ) +
  theme_minimal()

# b) Or just tabulate it
panel_obs %>%
  filter(ANIO == 2023, MES == 10) %>%
  summarise(n_obs = n(), n_zero = sum(revenue0 == 0))

panel_obs %>%
  filter(ANIO == 2023, MES == 9) %>%
  summarise(n_obs = n(), n_zero = sum(revenue0 == 0))



# 1) Compute summary stats by month in 2023
stats_2023 <- monthly_data %>%
  filter(ANIO == 2023) %>%
  group_by(ANIO, MES) %>%
  summarise(
    n_obs      = n(),                                     # total firm–month rows
    n_zero     = sum(revenue0 == 0),                      # how many zeros
    pct_zero   = mean(revenue0 == 0)*100,                 # % of zeros
    mean_rev   = mean(revenue0, na.rm = TRUE),            # average revenue
    median_rev = median(revenue0, na.rm = TRUE),          # median revenue
    sum_rev    = sum(revenue0, na.rm = TRUE),             # total revenue
    .groups    = "drop"
  )

print(stats_2023)

# 2) Compare October vs. the other months
stats_2023 %>%
  mutate(group = if_else(MES == 10, "Oct 2023", "Other 2023")) %>%
  group_by(group) %>%
  summarise(
    avg_n_obs     = mean(n_obs),
    avg_pct_zero  = mean(pct_zero),
    avg_mean_rev  = mean(mean_rev),
    avg_median_rev= mean(median_rev),
    avg_sum_rev   = mean(sum_rev)
  ) %>%
  print()








### PERU paper model



# 1) Build a single quarterly panel with both pivots
quarterly_data <- monthly_data %>%
  mutate(
    quarter           = floor_date(date,            "quarter"),
    q_treat           = floor_date(treat_date,      "quarter"),
    q_notify          = floor_date(notified_date,   "quarter")
  ) %>%
  group_by(firm_id, quarter, q_treat, q_notify) %>%
  summarise(
    Y     = mean(Y, na.rm = TRUE),
    .groups = "drop"
  )

# # ───────────────────────────────────────────────────────────
# # A) TWFE around the mandated treatment date
# # ───────────────────────────────────────────────────────────
# 
# # 2A) integerize & event‐time
# q_levels_t <- sort(unique(c(quarterly_data$quarter, quarterly_data$q_treat)))
# qt <- quarterly_data %>%
#   mutate(
#     quarter_num_t     = as.integer(factor(quarter, levels = q_levels_t)),
#     adopt_num_t       = as.integer(factor(q_treat,  levels = q_levels_t)),
#     event_t           = quarter_num_t - adopt_num_t
#   ) %>%
#   filter(q_treat <= max(quarter))
# 
# # 3A) estimate
# twfe_treat <- feols(
#   Y ~ i(event_t, ref = -1)
#   | firm_id + quarter_num_t,
#   data    = qt,
#   cluster = "firm_id"
# )
# 
# # 4A) tidy & plot
# ev_t <- tidy(twfe_treat, conf.int = TRUE) %>%
#   filter(str_detect(term, "^event_t::")) %>%
#   mutate(
#     event_time = as.integer(str_remove(term, "event_t::")),
#     lower      = conf.low,
#     upper      = conf.high
#   )
# 
# ggplot(ev_t, aes(x = event_time, y = estimate)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
#   geom_point(size = 2) +
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
#   labs(
#     title = "TWFE Event‐Time Coeffs (Mandated Treatment)",
#     x     = "Quarters Since Treatment",
#     y     = "Estimate"
#   ) +
#   theme_minimal()
# 
# # ───────────────────────────────────────────────────────────
# # B) TWFE around the notification date
# # ───────────────────────────────────────────────────────────
# 
# # 2B) integerize & event‐time
# q_levels_n <- sort(unique(c(quarterly_data$quarter, quarterly_data$q_notify)))
# qn <- quarterly_data %>%
#   mutate(
#     quarter_num_n     = as.integer(factor(quarter, levels = q_levels_n)),
#     adopt_num_n       = as.integer(factor(q_notify, levels = q_levels_n)),
#     event_n           = quarter_num_n - adopt_num_n
#   ) %>%
#   filter(q_notify <= max(quarter))
# 
# # 3B) estimate
# twfe_notify <- feols(
#   Y ~ i(event_n, ref = -1)
#   | firm_id + quarter_num_n,
#   data    = qn,
#   cluster = "firm_id"
# )
# 
# # 4B) tidy & plot
# ev_n <- tidy(twfe_notify, conf.int = TRUE) %>%
#   filter(str_detect(term, "^event_n::")) %>%
#   mutate(
#     event_time = as.integer(str_remove(term, "event_n::")),
#     lower      = conf.low,
#     upper      = conf.high
#   )
# 
# ggplot(ev_n, aes(x = event_time, y = estimate)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
#   geom_point(size = 2) +
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
#   labs(
#     title = "TWFE Event‐Time Coeffs (Notification Pivot)",
#     x     = "Quarters Since Notification",
#     y     = "Estimate"
#   ) +
#   theme_minimal()


# ──────────────────────────────────────────────────────────────
# C) TWFE EVENT‐STUDY FOR QUARTERLY REMITTANCE RATES
#    (MANDATED TREATMENT DATE)
# ──────────────────────────────────────────────────────────────

library(dplyr)
library(lubridate)
library(fixest)
library(broom)
library(stringr)
library(ggplot2)

# 1) Build a firm–quarter panel of remittance _rates_
qt_remit <- monthly_data %>%
  # a) define calendar quarter
  mutate(quarter = floor_date(date, "quarter")) %>%
  # b) compute fraction of firm-months in which the firm remitted
  group_by(firm_id, quarter) %>%
  summarise(
    remitted_q = mean(remitted, na.rm = TRUE),  # between 0 and 1
    .groups    = "drop"
  ) %>%
  # c) attach your precomputed event‐time indices from `qt`
  left_join(
    qt %>% select(firm_id, quarter, quarter_num_t, adopt_num_t, event_t),
    by = c("firm_id","quarter")
  ) %>%
  # drop any firm‐quarter before you ever observe adoption
  filter(!is.na(event_t))

# 2) Estimate saturated TWFE on the remittance rate
twfe_remit <- feols(
  remitted_q ~ i(event_t, ref = -1)
  | firm_id + quarter_num_t,
  data    = qt_remit,
  cluster = "firm_id"
)

# 3) Tidy & pull out only the event‐time terms
ev_r <- tidy(twfe_remit, conf.int = TRUE) %>%
  filter(str_detect(term, "^event_t::")) %>%
  mutate(
    event_time = as.integer(str_remove(term, "event_t::")),
    lower      = conf.low,
    upper      = conf.high
  )

# 4) Plot the coefficients with 95% CIs
ggplot(ev_r, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(
    title = "TWFE Event‐Time Coefficients: Remittance Rate (Mandated Treatment)",
    x     = "Quarters Since Treatment",
    y     = "Coefficient on event_t"
  ) +
  theme_minimal()




### Revenue

library(scales)

# Compute total VAT revenue across all firms each month
rev_total <- monthly_data %>%
  group_by(date) %>%
  summarise(total_rev = sum(revenue0, na.rm = TRUE), .groups = "drop")

# Plot
ggplot(rev_total, aes(x = date, y = total_rev)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "Total Monthly VAT Revenue (All Firms)",
    x     = "Date",
    y     = "Total Revenue (USD$)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# total VAT revenue in 2025
total_rev_2025 <- rev_total %>%
  filter(year(date) == 2025) %>%
  summarise(total_revenue = sum(total_rev, na.rm = TRUE))
print(total_rev_2025)



# total number of no_baseline firms after 1.2023
no_baseline_firms <- monthly_data %>%
  filter(size_group_strict == "no_baseline", date >= "2023-01-01") %>%
  summarise(n_firms = n_distinct(firm_id))
print(no_baseline_firms)





### Agggregated data event study

# Group–Industry–Month Event Study

# 1. Collapse to Group–Industry–Month level
agg_data <- monthly_data %>%
  filter(!is.na(group_id), !is.na(industry)) %>%
  group_by(group_id, industry, date) %>%
  summarise(
    total_rev = sum(revenue0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Y_agg = log(total_rev + 1),  # log AFTER aggregation
    month = as.integer(factor(date, levels = sort(unique(date))))
  )

# 2. Assign treatment timing by group
group_timing_trim <- group_timing %>%
  mutate(period = as.integer(factor(treat_date, levels = sort(unique(monthly_data$date))))) %>%
  rename(g = period)

agg_data <- agg_data %>%
  left_join(group_timing_trim %>% select(group_id, g), by = "group_id") %>%
  mutate(id = as.integer(factor(paste(group_id, industry))))

# 3. Run event study
agg_att <- att_gt(
  yname = "Y_agg",
  tname = "month",
  idname = "id",
  gname = "g",
  data = agg_data,
  control_group = "notyettreated",
  panel = FALSE,
  clustervars = "id"
)

agg_dyn <- aggte(agg_att, type = "dynamic", min_e = -12, max_e = Inf)

# 4. Plot
ggdid(agg_dyn) +
  labs(
    title = "ATT on Log Aggregated VAT Revenue (Group–Industry–Month)",
    x = "Event Time (Months)",
    y = "ATT (log total revenue)"
  ) +
  theme_minimal()



# view the group–industry–month data
group_industry_month <- monthly_data %>%
  filter(!is.na(group_id), !is.na(industry)) %>%
  distinct(group_id, industry, date) %>%
  arrange(group_id, industry, date)

View(group_industry_month)  # Or use print() if not in RStudio



#plot

# Step 1: Count number of firms per industry
industry_size <- monthly_data %>%
  filter(!is.na(group_id), !is.na(industry), !is.na(firm_id)) %>%
  distinct(firm_id, industry, group_id) %>%
  group_by(industry, group_id) %>%
  summarise(
    n_firms = n(),
    .groups = "drop"
  ) %>%
  left_join(group_timing, by = "group_id")  # attach treat_date

# Step 2: Plot
ggplot(industry_size, aes(x = treat_date, y = reorder(industry, n_firms), size = n_firms)) +
  geom_point(color = "steelblue", alpha = 0.8) +
  labs(
    title = "Industry Group Size and Treatment Timing",
    subtitle = "Each point is an industry-group pair",
    x = "Treatment Date",
    y = "Industry (2-digit ACTECO)",
    size = "Number of Firms"
  ) +
  theme_minimal()
