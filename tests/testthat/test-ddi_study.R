#' # Examplinib FE ID 1, period 1 (500 mg), scaled linearly to 100 mg
#' .examplinib_rich_profile <- tibble::tribble(
#'   ~NTIME,           ~DV,
#'        0,             0,
#'      0.5,      939.4654,
#'        1,     1265.0202,
#'      1.5,     1258.8374,
#'        2,     1186.4928,
#'        3,       863.305,
#'        4,      544.0696,
#'        6,     187.72538,
#'        8,      79.72462,
#'       10,      41.17366,
#'       12,      25.05896,
#'       24,      6.988768,
#'       48,      0.838991,
#'       72,    0.10200496,
#'       96,   0.011295866,
#'      144, 0.00016295992,
#'      168, 1.9408394e-05
#'   )
#'
#'
#'
#' # Multiplicative subject factors around the reference examplinib profile
#' .ddi_subject_factors <- c(0.90, 0.95, 1.00, 1.05, 1.10)
#'
#'
#' make_ddi_study_nif <- function(
#'     n_subjects = 3,
#'     subject_factors = c(0.90, 0.95, 1.00, 1.05, 1.10)
#' ) {
#'   test_dose_days <- c(1L, 12L)
#'   itra_dose_days <- 8L:18L
#'   test_dose <- 100
#'   itra_dose <- 200
#'
#'   day_to_time <- function(day) {
#'     (day - 1L) * 24
#'   }
#'
#'   factors <- rep_len(subject_factors, n_subjects)
#'
#'   rows <- purrr::map_dfr(seq_len(n_subjects), function(i) {
#'     id <- as.integer(i)
#'     usubjid <- sprintf("DDI-%03d", i)
#'     factor_i <- factors[[i]]
#'
#'     test_admins <- tibble::tibble(
#'       ID = id,
#'       USUBJID = usubjid,
#'       TIME = day_to_time(test_dose_days),
#'       NTIME = 0,
#'       AMT = test_dose,
#'       DV = NA_real_,
#'       CMT = 1L,
#'       EVID = 1L,
#'       MDV = 1L,
#'       ANALYTE = "test",
#'       PARENT = "test",
#'       METABOLITE = FALSE,
#'       DOSE = test_dose
#'     )
#'
#'     test_obs <- purrr::map_dfr(test_dose_days, function(day) {
#'       dose_time <- day_to_time(day)
#'       .examplinib_rich_profile |>
#'         dplyr::mutate(
#'           ID = id,
#'           USUBJID = usubjid,
#'           TIME = dose_time + .data$NTIME,
#'           DV = .data$DV * factor_i,
#'           AMT = 0,
#'           CMT = 2L,
#'           EVID = 0L,
#'           MDV = 0L,
#'           ANALYTE = "test",
#'           PARENT = "test",
#'           METABOLITE = FALSE,
#'           DOSE = test_dose
#'         )
#'     })
#'
#'     itra_admins <- tibble::tibble(
#'       ID = id,
#'       USUBJID = usubjid,
#'       TIME = day_to_time(itra_dose_days),
#'       NTIME = 0,
#'       AMT = itra_dose,
#'       DV = NA_real_,
#'       CMT = 3L,
#'       EVID = 1L,
#'       MDV = 1L,
#'       ANALYTE = "itraconazole",
#'       PARENT = "itraconazole",
#'       METABOLITE = FALSE,
#'       DOSE = itra_dose
#'     )
#'
#'     dplyr::bind_rows(test_admins, test_obs, itra_admins)
#'   }) |>
#'     dplyr::arrange(.data$ID, .data$TIME, dplyr::desc(.data$EVID), .data$CMT)
#'
#'   nif(rows) |>
#'     ensure_tad() |>
#'     ensure_tafd()
#' }


ddi_nif <- tibble::tribble(
  ~REF, ~ID,  ~USUBJID, ~TIME, ~NTIME, ~TAFD, ~TAD, ~EVID, ~AMT, ~CMT,            ~DV,       ~ANALYTE,        ~PARENT, ~METABOLITE, ~DOSE, ~MDV,
  1L,  1L, "DDI-001",     0,      0,     0,    0,    1L,  100,   1L,             NA,         "test",         "test",       FALSE,   100,   1L,
  2L,  1L, "DDI-001",     0,      0,     0,    0,    0L,    0,   2L,              0,         "test",         "test",       FALSE,   100,   0L,
  3L,  1L, "DDI-001",   0.5,    0.5,   0.5,  0.5,    0L,    0,   2L,      845.51886,         "test",         "test",       FALSE,   100,   0L,
  4L,  1L, "DDI-001",     1,      1,     1,    1,    0L,    0,   2L,     1138.51818,         "test",         "test",       FALSE,   100,   0L,
  5L,  1L, "DDI-001",   1.5,    1.5,   1.5,  1.5,    0L,    0,   2L,     1132.95366,         "test",         "test",       FALSE,   100,   0L,
  6L,  1L, "DDI-001",     2,      2,     2,    2,    0L,    0,   2L,     1067.84352,         "test",         "test",       FALSE,   100,   0L,
  7L,  1L, "DDI-001",     3,      3,     3,    3,    0L,    0,   2L,       776.9745,         "test",         "test",       FALSE,   100,   0L,
  8L,  1L, "DDI-001",     4,      4,     4,    4,    0L,    0,   2L,      489.66264,         "test",         "test",       FALSE,   100,   0L,
  9L,  1L, "DDI-001",     6,      6,     6,    6,    0L,    0,   2L,     168.952842,         "test",         "test",       FALSE,   100,   0L,
  10L,  1L, "DDI-001",     8,      8,     8,    8,    0L,    0,   2L,      71.752158,         "test",         "test",       FALSE,   100,   0L,
  11L,  1L, "DDI-001",    10,     10,    10,   10,    0L,    0,   2L,      37.056294,         "test",         "test",       FALSE,   100,   0L,
  12L,  1L, "DDI-001",    12,     12,    12,   12,    0L,    0,   2L,      22.553064,         "test",         "test",       FALSE,   100,   0L,
  13L,  1L, "DDI-001",    24,     24,    24,   24,    0L,    0,   2L,      6.2898912,         "test",         "test",       FALSE,   100,   0L,
  14L,  1L, "DDI-001",    48,     48,    48,   48,    0L,    0,   2L,      0.7550919,         "test",         "test",       FALSE,   100,   0L,
  15L,  1L, "DDI-001",    72,     72,    72,   72,    0L,    0,   2L,    0.091804464,         "test",         "test",       FALSE,   100,   0L,
  16L,  1L, "DDI-001",    96,     96,    96,   96,    0L,    0,   2L,   0.0101662794,         "test",         "test",       FALSE,   100,   0L,
  17L,  1L, "DDI-001",   144,    144,   144,  144,    0L,    0,   2L, 0.000146663928,         "test",         "test",       FALSE,   100,   0L,
  18L,  1L, "DDI-001",   168,      0,     0,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  19L,  1L, "DDI-001",   168,    168,   168,  168,    0L,    0,   2L, 1.74675546e-05,         "test",         "test",       FALSE,   100,   0L,
  20L,  1L, "DDI-001",   192,      0,    24,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  21L,  1L, "DDI-001",   216,      0,    48,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  22L,  1L, "DDI-001",   240,      0,    72,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  23L,  1L, "DDI-001",   264,      0,    96,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  24L,  1L, "DDI-001",   264,      0,   264,    0,    1L,  100,   1L,             NA,         "test",         "test",       FALSE,   100,   1L,
  25L,  1L, "DDI-001",   264,      0,   264,  264,    0L,    0,   2L,              0,         "test",         "test",       FALSE,   100,   0L,
  26L,  1L, "DDI-001", 264.5,    0.5, 264.5,  0.5,    0L,    0,   2L,      845.51886,         "test",         "test",       FALSE,   100,   0L,
  27L,  1L, "DDI-001",   265,      1,   265,    1,    0L,    0,   2L,     1138.51818,         "test",         "test",       FALSE,   100,   0L,
  28L,  1L, "DDI-001", 265.5,    1.5, 265.5,  1.5,    0L,    0,   2L,     1132.95366,         "test",         "test",       FALSE,   100,   0L,
  29L,  1L, "DDI-001",   266,      2,   266,    2,    0L,    0,   2L,     1067.84352,         "test",         "test",       FALSE,   100,   0L,
  30L,  1L, "DDI-001",   267,      3,   267,    3,    0L,    0,   2L,       776.9745,         "test",         "test",       FALSE,   100,   0L,
  31L,  1L, "DDI-001",   268,      4,   268,    4,    0L,    0,   2L,      489.66264,         "test",         "test",       FALSE,   100,   0L,
  32L,  1L, "DDI-001",   270,      6,   270,    6,    0L,    0,   2L,     168.952842,         "test",         "test",       FALSE,   100,   0L,
  33L,  1L, "DDI-001",   272,      8,   272,    8,    0L,    0,   2L,      71.752158,         "test",         "test",       FALSE,   100,   0L,
  34L,  1L, "DDI-001",   274,     10,   274,   10,    0L,    0,   2L,      37.056294,         "test",         "test",       FALSE,   100,   0L,
  35L,  1L, "DDI-001",   276,     12,   276,   12,    0L,    0,   2L,      22.553064,         "test",         "test",       FALSE,   100,   0L,
  36L,  1L, "DDI-001",   288,      0,   120,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  37L,  1L, "DDI-001",   288,     24,   288,   24,    0L,    0,   2L,      6.2898912,         "test",         "test",       FALSE,   100,   0L,
  38L,  1L, "DDI-001",   312,      0,   144,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  39L,  1L, "DDI-001",   312,     48,   312,   48,    0L,    0,   2L,      0.7550919,         "test",         "test",       FALSE,   100,   0L,
  40L,  1L, "DDI-001",   336,      0,   168,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  41L,  1L, "DDI-001",   336,     72,   336,   72,    0L,    0,   2L,    0.091804464,         "test",         "test",       FALSE,   100,   0L,
  42L,  1L, "DDI-001",   360,      0,   192,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  43L,  1L, "DDI-001",   360,     96,   360,   96,    0L,    0,   2L,   0.0101662794,         "test",         "test",       FALSE,   100,   0L,
  44L,  1L, "DDI-001",   384,      0,   216,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  45L,  1L, "DDI-001",   408,      0,   240,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  46L,  1L, "DDI-001",   408,    144,   408,  144,    0L,    0,   2L, 0.000146663928,         "test",         "test",       FALSE,   100,   0L,
  47L,  1L, "DDI-001",   432,    168,   432,  168,    0L,    0,   2L, 1.74675546e-05,         "test",         "test",       FALSE,   100,   0L,
  48L,  2L, "DDI-002",     0,      0,     0,    0,    1L,  100,   1L,             NA,         "test",         "test",       FALSE,   100,   1L,
  49L,  2L, "DDI-002",     0,      0,     0,    0,    0L,    0,   2L,              0,         "test",         "test",       FALSE,   100,   0L,
  50L,  2L, "DDI-002",   0.5,    0.5,   0.5,  0.5,    0L,    0,   2L,      892.49213,         "test",         "test",       FALSE,   100,   0L,
  51L,  2L, "DDI-002",     1,      1,     1,    1,    0L,    0,   2L,     1201.76919,         "test",         "test",       FALSE,   100,   0L,
  52L,  2L, "DDI-002",   1.5,    1.5,   1.5,  1.5,    0L,    0,   2L,     1195.89553,         "test",         "test",       FALSE,   100,   0L,
  53L,  2L, "DDI-002",     2,      2,     2,    2,    0L,    0,   2L,     1127.16816,         "test",         "test",       FALSE,   100,   0L,
  54L,  2L, "DDI-002",     3,      3,     3,    3,    0L,    0,   2L,      820.13975,         "test",         "test",       FALSE,   100,   0L,
  55L,  2L, "DDI-002",     4,      4,     4,    4,    0L,    0,   2L,      516.86612,         "test",         "test",       FALSE,   100,   0L,
  56L,  2L, "DDI-002",     6,      6,     6,    6,    0L,    0,   2L,     178.339111,         "test",         "test",       FALSE,   100,   0L,
  57L,  2L, "DDI-002",     8,      8,     8,    8,    0L,    0,   2L,      75.738389,         "test",         "test",       FALSE,   100,   0L,
  58L,  2L, "DDI-002",    10,     10,    10,   10,    0L,    0,   2L,      39.114977,         "test",         "test",       FALSE,   100,   0L,
  59L,  2L, "DDI-002",    12,     12,    12,   12,    0L,    0,   2L,      23.806012,         "test",         "test",       FALSE,   100,   0L,
  60L,  2L, "DDI-002",    24,     24,    24,   24,    0L,    0,   2L,      6.6393296,         "test",         "test",       FALSE,   100,   0L,
  61L,  2L, "DDI-002",    48,     48,    48,   48,    0L,    0,   2L,     0.79704145,         "test",         "test",       FALSE,   100,   0L,
  62L,  2L, "DDI-002",    72,     72,    72,   72,    0L,    0,   2L,    0.096904712,         "test",         "test",       FALSE,   100,   0L,
  63L,  2L, "DDI-002",    96,     96,    96,   96,    0L,    0,   2L,   0.0107310727,         "test",         "test",       FALSE,   100,   0L,
  64L,  2L, "DDI-002",   144,    144,   144,  144,    0L,    0,   2L, 0.000154811924,         "test",         "test",       FALSE,   100,   0L,
  65L,  2L, "DDI-002",   168,      0,     0,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  66L,  2L, "DDI-002",   168,    168,   168,  168,    0L,    0,   2L, 1.84379743e-05,         "test",         "test",       FALSE,   100,   0L,
  67L,  2L, "DDI-002",   192,      0,    24,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  68L,  2L, "DDI-002",   216,      0,    48,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  69L,  2L, "DDI-002",   240,      0,    72,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  70L,  2L, "DDI-002",   264,      0,    96,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  71L,  2L, "DDI-002",   264,      0,   264,    0,    1L,  100,   1L,             NA,         "test",         "test",       FALSE,   100,   1L,
  72L,  2L, "DDI-002",   264,      0,   264,  264,    0L,    0,   2L,              0,         "test",         "test",       FALSE,   100,   0L,
  73L,  2L, "DDI-002", 264.5,    0.5, 264.5,  0.5,    0L,    0,   2L,      892.49213,         "test",         "test",       FALSE,   100,   0L,
  74L,  2L, "DDI-002",   265,      1,   265,    1,    0L,    0,   2L,     1201.76919,         "test",         "test",       FALSE,   100,   0L,
  75L,  2L, "DDI-002", 265.5,    1.5, 265.5,  1.5,    0L,    0,   2L,     1195.89553,         "test",         "test",       FALSE,   100,   0L,
  76L,  2L, "DDI-002",   266,      2,   266,    2,    0L,    0,   2L,     1127.16816,         "test",         "test",       FALSE,   100,   0L,
  77L,  2L, "DDI-002",   267,      3,   267,    3,    0L,    0,   2L,      820.13975,         "test",         "test",       FALSE,   100,   0L,
  78L,  2L, "DDI-002",   268,      4,   268,    4,    0L,    0,   2L,      516.86612,         "test",         "test",       FALSE,   100,   0L,
  79L,  2L, "DDI-002",   270,      6,   270,    6,    0L,    0,   2L,     178.339111,         "test",         "test",       FALSE,   100,   0L,
  80L,  2L, "DDI-002",   272,      8,   272,    8,    0L,    0,   2L,      75.738389,         "test",         "test",       FALSE,   100,   0L,
  81L,  2L, "DDI-002",   274,     10,   274,   10,    0L,    0,   2L,      39.114977,         "test",         "test",       FALSE,   100,   0L,
  82L,  2L, "DDI-002",   276,     12,   276,   12,    0L,    0,   2L,      23.806012,         "test",         "test",       FALSE,   100,   0L,
  83L,  2L, "DDI-002",   288,      0,   120,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  84L,  2L, "DDI-002",   288,     24,   288,   24,    0L,    0,   2L,      6.6393296,         "test",         "test",       FALSE,   100,   0L,
  85L,  2L, "DDI-002",   312,      0,   144,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  86L,  2L, "DDI-002",   312,     48,   312,   48,    0L,    0,   2L,     0.79704145,         "test",         "test",       FALSE,   100,   0L,
  87L,  2L, "DDI-002",   336,      0,   168,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  88L,  2L, "DDI-002",   336,     72,   336,   72,    0L,    0,   2L,    0.096904712,         "test",         "test",       FALSE,   100,   0L,
  89L,  2L, "DDI-002",   360,      0,   192,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  90L,  2L, "DDI-002",   360,     96,   360,   96,    0L,    0,   2L,   0.0107310727,         "test",         "test",       FALSE,   100,   0L,
  91L,  2L, "DDI-002",   384,      0,   216,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  92L,  2L, "DDI-002",   408,      0,   240,    0,    1L,  200,   3L,             NA, "itraconazole", "itraconazole",       FALSE,   200,   1L,
  93L,  2L, "DDI-002",   408,    144,   408,  144,    0L,    0,   2L, 0.000154811924,         "test",         "test",       FALSE,   100,   0L,
  94L,  2L, "DDI-002",   432,    168,   432,  168,    0L,    0,   2L, 1.84379743e-05,         "test",         "test",       FALSE,   100,   0L
) |>
  nif()


test_that("index_dosing_interval works with real-life data set", {
  temp <- ddi_nif |>
    index_dosing_interval() |>
    as.data.frame()

  expect_equal(unique(filter(temp, TIME <= 168, EVID == 0)$DI), 1)
  expect_equal(unique(filter(temp, TIME > 168, EVID == 0)$DI), 2)
})



