## Config file, please edit to your needs
db_user <- "YOUR_MONGODB_USERNAME"
db_passwd <- "YOUR_MONGODB_PASSWORD"

# For TLS, please see mongolite / MongoDB TLS docs
db_tls <- FALSE  # set to TRUE to use TLS

db_host <- "YOUR_MONGODB_HOST_OR_IP"
db_ca <- NULL    # CA cert if TLS in use, see mongolite docs
db_cert <- NULL  # client auth cert if in use, see mongolite docs
db_key <- NULL   # client cert key if in use, see mongolite docs
allow_invalid_hostname <- NULL  # see mongolite docs
weak_cert_validation <- NULL  # see mongolite / MongoDB TLS docs


data_dir <- "~/PATH/TO/YOUR/BLITZDATA" # see utils_arrow.R and ds_load_xxx() funcs


# Your WG API token. get it from https://developers.wargaming.net
WG_AppID <- "YOUR_WG_API_TOKEN"
