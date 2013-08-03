Copycat
======

Copycat is a minimal chat app that uses Erlang as its backend and AngularJS for its UI
and WebSockets to connect the two.

Goals
-----

 * Demonstrate how things become simple when we use websockets, erlang and angular.
 * Learn a bit about Erlang/Angular/JS

Getting Started
---------------

This repo is the source of Erlang app. I use rebar to compile, build and generate
release (TODO: drop in rel templates). To simply get going the following is the
set of command I use:

    > git clone git@github.com:ashneyderman/copycat.git
    > cd copycat
    > rebar get-deps
    > rebar compile
    > ./start-dev.sh

This will get you from a clean plate to the running chat app (provided you have Erlang
installed already). You can access the app at
[http://localhost:9900/index.html] (http://localhost:9900/index.html)

Support
-------

 * Things were only tested on Chrome.
 * Alex Shneyderman [mailto:a.shneyderman@gmail.com]
