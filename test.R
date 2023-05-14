install.packages("webshot")
library(webshot)
install_phantomjs()

library(trackdown)
install.packages("remotes")
remotes::install_github("claudiozandonella/trackdown",
                        build_vignettes = TRUE)
library(trackdown)
my_app <- httr::oauth_app(
  appname = "Desktop-app",
  key     = "655340588749-u7foe8koqanrvj2udcetbfmv1sj8ljo7.apps.googleusercontent.com",
  secret  = "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"
)

trackdown_auth_configure(app = my_app)
usethis::edit_r_environ()
trackdown_auth_configure(path = "/Users/jeanbaptistelagrangedupuis/Downloads/client_secret_655340588749-u7foe8koqanrvj2udcetbfmv1sj8ljo7.apps.googleusercontent.com.json")

update_file("rendu_final.Rnw", hide_code = TRUE)
render_file("rendu_final.Rnw", gpath="trackdown")


options(
  gargle_oauth_email = "jb772477@gmail.com",
  gargle_oauth_cache = "/path/to/folder/that/does/not/sync/to/cloud"
)