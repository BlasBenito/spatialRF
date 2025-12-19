install.packages("rhub")
Sys.setenv(GITHUB_PAT = "xxx")
rhub::rhub_setup()
rhub::rhub_doctor()

#windows
devtools::check_win_devel()

#full check
rhub::rhub_check(
  platforms = c(
    "linux",
    "macos",
    "macos-arm64",
    "windows",
    "atlas",
    "c23",
    "clang-asan",
    "clang16",
    "clang17",
    "clang18",
    "clang19",
    "gcc13",
    "gcc14",
    "intel",
    "mkl",
    "nold",
    "nosuggests",
    "ubuntu-clang",
    "ubuntu-gcc12",
    "ubuntu-next",
    "ubuntu-release"
  )
)
