
# Pomodoro Timer in Elm

This project is a simple pomodoro timer written in Elm

## Requirement
* Elm lang v.0.19.1

## Installation
To build the project
```$bash
elm-make src/Main.elm --output=js/main.js 
elm-make src/Timer.elm --output=js/timer.js
```

## Usage
This project uses web workers so you need to have a
web server to running to serve the files. 

I wrote a small server using Common Lisp to serve 
the files. Assumming you have sbcl and quicklisp installed. 

Start a repl and quickload hunchentoot. 
Load the file and run the start function.

Visit localhost:4242 to see the application running
