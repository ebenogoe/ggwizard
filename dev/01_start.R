## Run this script once to initialise the development environment.
## It is NOT part of the built package.

# 1. Install dependencies listed in DESCRIPTION
devtools::install_deps()

# 2. Attach for interactive development
devtools::load_all()

# 3. Launch the app in a browser
run_app()
