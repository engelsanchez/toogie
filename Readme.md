Toogie: A game server for two player turn based games 
=====================================================

Two players can connect to the server and play against each other.
Currently only supports 4 in line games.
Uses [cowboy](https://github.com/extend/cowboy) as the server layer
to allow for multiple protocols (raw sockets, websockets and possibly
later other http push technologies as fallback)

Building
--------

You will need to have git installed (to fetch the cowboy server dependency)
as well as Erlang, of course.  Simply go to the main directory and type:

   make

EDoc documentation
------------------

To generate detailed source code documentation of the modules in this 
application, simply go to the base directory and type: 

    make doc

The HTML documentation is placed in doc/index.html
