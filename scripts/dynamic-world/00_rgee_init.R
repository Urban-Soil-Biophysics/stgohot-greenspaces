# ── 00_gee_init.R ────────────────────────────────────────────────────────────
# Safe GEE session initializer — source this at the top of any script
# that uses rgee. Do NOT call ee_Authenticate() or
# ee_clean_user_credentials() inside scripts — they destroy cached tokens.
#
# First-time setup (run ONCE manually in the console, never in scripts):
#   library(rgee)
#   ee_Authenticate(user = "saryace")
#   ee_Initialize(user = "saryace", project = "ee-saryace", drive = TRUE,
#                 asset_root = "projects/earthengine-legacy/assets/users/saryace")
#   ee$data$createAssetHome("projects/earthengine-legacy/assets/users/saryace")
# ─────────────────────────────────────────────────────────────────────────────

library(rgee)

# ── Internal helpers ─────────────────────────────────────────────────────────

.gee_do_init <- function() {
  ee_Initialize(
    user       = "saryace",
    project    = "ee-saryace",
    drive      = TRUE,
    asset_root = "projects/earthengine-legacy/assets/users/saryace"
  )
  .gee_write_session_file()
}

.gee_write_session_file <- function() {
  # rgee's ee_path() always returns ~/.config/earthengine/ (no username subfolder)
  # ee_Initialize() fails to write the session file reliably so we force it
  # using rgee's own internal writer (write.table format, not key=value)
  rgee:::ee_sessioninfo(
    email     = NA,
    user      = "saryace",
    drive_cre = TRUE,
    gcs_cre   = FALSE
  )
}

.gee_is_expired <- function(msg) {
  grepl(
    "credential|expired|authenticate|EEException|earthengine authenticate|not initialized",
    msg,
    ignore.case = TRUE
  )
}

# ── Main init block ──────────────────────────────────────────────────────────

tryCatch(
  {
    # Lightweight ping — if GEE is already active this is instant
    ee$String("ping")$getInfo()
    # Session active — still force-write session file in case it was lost
    .gee_write_session_file()
    message("✔ GEE session already active.")
  },
  error = function(e) {
    msg <- conditionMessage(e)
    
    if (.gee_is_expired(msg)) {
      message("⚠ GEE token expired or session not initialized. Re-authenticating...")
      ee_Authenticate(user = "saryace")
      
      tryCatch(
        {
          .gee_do_init()
          message("✔ GEE session ready after re-authentication.")
        },
        error = function(e2) {
          stop(
            "GEE initialization failed after re-authentication.\n",
            "Run manually in the console:\n",
            "  rgee::ee_Authenticate(user = 'saryace')\n",
            "  rgee::ee_Initialize(\n",
            "    user = 'saryace', project = 'ee-saryace',\n",
            "    drive = TRUE,\n",
            "    asset_root = 'projects/earthengine-legacy/assets/users/saryace'\n",
            "  )\n",
            "Original error: ", conditionMessage(e2)
          )
        }
      )
    } else {
      # Session not started yet — normal cold start after R restart
      tryCatch(
        {
          .gee_do_init()
          message("✔ GEE session ready.")
        },
        error = function(e2) {
          msg2 <- conditionMessage(e2)
          if (.gee_is_expired(msg2)) {
            message("⚠ Token expired on cold start. Re-authenticating...")
            ee_Authenticate(user = "saryace")
            .gee_do_init()
            message("✔ GEE session ready after re-authentication.")
          } else {
            stop("GEE initialization failed (non-credential error):\n", msg2)
          }
        }
      )
    }
  }
)