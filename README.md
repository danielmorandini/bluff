# bluff
Bluff game written in erlang

###Intro
I tried to simulate the game as close as possible to a real environment, so in order to play, players
and dealer will be in different erlang shells.


###Usage
* Enter an erlang shell using `erl -sname somename` (this will be you dealer node)
  1. `c(bluffserver).` (required only the first time) 
  2. `bluffserver:start().` (dealer will start accepting new users)
  
* Create other different erlang shells (between 2 and 10) using `elr -sname othername`
  1. `c(bluffclient).` (required only the first time)
  2. `Me = bluffclient:player(your_username, dealers_node_name).` (you can get dealers node name with `node().` 
  in dealer's shell

*NB: If you want to run the nodes in a distributed system, use* `-name` *instead of* `-sname`. 

* Start the game in the dealers shell `bluff_reg ! start.`
* Now every played gets noticed when it is his turn and when he can play. 

###What you can do
* Send cards with `Me ! play.` (Plays every card that you have with the required rank) or 
with `Me ! {play, [1,4,6...]}.` (Plays cards at the chosen indexes)
* Bluff last player that played with `Me ! bluff.`
