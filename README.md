# jhipster-elm-demo
WIP: Single Page Web Application with Java Spring Boot backend and Elm UI frontend.


## Run Locally:

Make sure Elm and Java are installed.
Download Create Elm App by `npm install create-elm-app -g`, and make sure http-server is installed by `npm install -g http-server`.

In the jhipster directory, run `./mvnw`

In the elm directory, run `elm-app build` to build the project.
Then run `http-server ./build`.

## Using Elm Analyse

To use Elm Analyse, run:

`elm-analyse -s -o -p XXXX`

in the elm directory to open a server at localhost:XXXX (default is 3000). This provides a nice, visual tool which shows the analysis of the elm code.
