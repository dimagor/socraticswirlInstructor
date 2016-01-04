# Fill in the application key and REST api key for the test server

Sys.setenv(PARSE_APPLICATION_ID_TEST = "TEST-KEY")
Sys.setenv(PARSE_API_KEY_TEST = "TEST-KEY")

# Fill in the application key and REST api key for the production server

Sys.setenv(PARSE_APPLICATION_ID_PROD = "PROD-KEY")
Sys.setenv(PARSE_API_KEY_PROD = "PROD-KEY")







#
# We assume the default goes to production
#
Sys.setenv(PARSE_APPLICATION_ID = Sys.getenv("PARSE_APPLICATION_ID_PROD"))
Sys.setenv(PARSE_API_KEY = Sys.getenv("PARSE_API_KEY_PROD"))
