# Haskell Ray-Tracing Server

A static web server serving a web application one one thread, the web application connects through a websocket to a second thread listening for scenes to render. 

# Instrucions
 1. Download and install the Haskell Tool Stack https://docs.haskellstack.org/en/stable/README/
 2. Clone this repository and run `stack setup` to download the compiler.
 3. Run `stack build` to download the necessary dependencies (this can take a while) and compile the code.
 4. Change line one of index.js to your ip address. 
 5. Run `stack run` to start the webserver on localhost. 
 6. Go to `localhost:8000` to see the web application.


