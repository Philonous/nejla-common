|File|Description|
|-|-|
| elk | Example ELK (Elasticsearch, Logstash, Kibana) stack |
| elk/README.md | |
| elk/logstash.conf.d | Example configuration components for Logstash |
| elk/logstash.conf.d/1-input-gelf.conf | Handle inputs via the GELF protocol |
| elk/logstash.conf.d/2-filter-parse-logs.conf | Parsing logs produced by Nejla Common |
| elk/logstash.conf.d/3-filter-geopip.conf | Resolving IPs to geographical regions|
| elk/logstash.conf.d/4-output-elasticsearch.conf | Output to elasticsearch |
| elk/logstash | |
| elk/logstash/Dockerfile | Custom logstash image to enable required modules|
| elk/docker-compose.yaml | Docker compose configuration for the example ELK stack |
| tests | Test suite |
| tests/Persistent | Testing persistence related code |
| tests/Persistent/DelayedIO.hs | Tests for IO actions that should run after the transactions commits|
| tests/Persistent/Serializable.hs | Tests for SERIALIZABLE and retryable transactions|
| tests/Persistent/Common.hs | Common setup and helpers for tests|
| tests/logstash | |
| tests/logstash/conf.d | Logstash parsing configuration for testing |
| tests/logstash/conf.d/1-input-stdin.conf | |
| tests/logstash/conf.d/2-filter-parse-logs.conf | |
| tests/logstash/conf.d/3-output-stdout.conf | |
| tests/logstash/docker-compose.yaml | |
| tests/Config.hs | Tests for configuration handling |
| tests/Helpers.hs | Tests for helper functions |
| tests/Logging.hs | Tests for logging functionality |
| tests/Logstash.hs | Tests for logstash parsing functionatlity |
| tests/Persistent.hs | Tests for persistence handling code |
| tests/Test.hs | Entrypoint for test suite|
| nuxt-template-files | Nuxt.js example project |
| nuxt-template-files/.dockerignore | |
| nuxt-template-files/.gitignore | |
| nuxt-template-files/Dockerfile | |
| nuxt-template-files/assets | |
| nuxt-template-files/assets/main.scss | |
| nuxt-template-files/middleware | |
| nuxt-template-files/middleware/authenticated.js | Redirection to sign-in page when not logged in |
| nuxt-template-files/nginx | |
| nuxt-template-files/nginx/auth-service.include | Example NGINX configuration |
| nuxt-template-files/nginx/nginx.conf | Example NGINX configuration |
| nuxt-template-files/nuxt.config.js | Project configuration with Axios, Bootstrap, Vee Validate and a proxy configuration |
| nuxt-template-files/pages | |
| nuxt-template-files/pages/sign-in.vue | Example sign-in page |
| nuxt-template-files/plugins | |
| nuxt-template-files/plugins/vee-validate.js | Example on how to customize Vee Validate |
| nuxt-template-files/plugins/axios.js | Redirection to sign-in page on 403 |
| nuxt-template-files/store | |
| nuxt-template-files/store/actions.js | Vuex example with nuxtServerInit |
| nuxt-template-files/.vscode | |
| nuxt-template-files/.vscode/settings.json | Prettier and Format on Save settings |
| nuxt-template-files/.eslintrc.json | ESLint settings |
| nuxt-template-files/.gitlab-ci.yml | GitLab CI configuration for ESLint |
| nuxt-template-files/jsconfig.json | jsconfig.json file improves code completion in Code |
| resources | Resources for CI |
| resources/badge-documentation.svg | CI Badge for linking to the documentation |
| LICENSE | Project license |
| README.md | |
| src | Root for project source code |
| src/NejlaCommon |  |
| src/NejlaCommon/Persistence.hs | Database and persistence combinators |
| src/NejlaCommon/Persistence | Database and persistence combinators |
| src/NejlaCommon/Persistence/sql | SQL code |
| src/NejlaCommon/Persistence/sql/initialize_versioning.sql | Initializing the database versioning scheme |
| src/NejlaCommon/Persistence/Migration.hs | System for handling database setup and migrations |
| src/NejlaCommon/Persistence/Util.hs | Utility code that doesn't fit anywhere else |
| src/NejlaCommon/Persistence/Compat.hs | Compatibility shim for different Haskell LTS releases. Conditionally compiled code should go in here |
| src/NejlaCommon/Persistence/SqlStatistics.hs | Collecting statistics of executed SQL queries |
| src/NejlaCommon/Test.hs | Infrastructure for writing tests |
| src/NejlaCommon/Test | Infrastructure for writing tests |
| src/NejlaCommon/Test/Expectation.hs | Combinators for expectations |
| src/NejlaCommon/Test/Json.hs | Handling JSON responses |
| src/NejlaCommon/Test/Logging.hs | Handling of logging during tests |
| src/NejlaCommon/Test/Postgres.hs | Efffective use of postgres databases in tests (setup and cleanup between tests) |
| src/NejlaCommon/Config.hs | Configuration parsing and handling |
| src/NejlaCommon/Helpers.hs | Generic helpers |
| src/NejlaCommon/Logging.hs | Structured logging |
| src/NejlaCommon/Sendmail.hs | Sending emails using sendmail |
| src/NejlaCommon/Wai.hs | WAI combinators |
| src/NejlaCommon.hs | Common infrastructure for writing web services |
| Dockerfile | Builds docker image for running tests in |
| docker-compose.tests.yaml | Docker setup for running tests (use make test)|
| hie.yaml | Project configuration for haskell-language-server |
| package.yaml | Haskell project configuration|
| shell.nix | Nix setup for development and building |
| stack.lts16.yaml | Haskell project configuration for lts 16 |
| stack.lts17.yaml | Haskell project configuration for lts 17 |
| Setup.hs | Haskell project infrastructure |
| floskell.json | Rules for automatic formatting of Haskell code |
| Makefile | Build and test coordination |
| stack.lts18.yaml | Haskell project configuration for lts 17 |
| stack.yaml | Symlink to the latest Haskell lts project configuration |
| Files.md | Descriptions of project files |
