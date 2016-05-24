- module(bluffserver).

- export([start/0]).
- export([prepareGame/1]).
- export([dealer/3]).

%suits  s (Spades), h (Hearts), d (Diamonds), c (Clubs)
%ranks a,1,2,3,4,5,6,7,8,9,10,j,q,k

%card example -> {2, h} is a 2 of Hearts

%global atoms: bluff_reg available to let people connect to the game, 
%              dealer for the game itself               

start() ->
    register(bluff_reg, spawn(bluffserver, prepareGame, [[]])).
    
    
prepareGame(Players) ->
    
    process_flag(trap_exit,true),
    receive
        {add, {PName, PPid}} when length(Players) < 11 ->
            link(PPid), 
            NewPlayers = [{PName, PPid}|Players],
            noticeAll(io_lib:format("New Player ~p connected!", [PName]), NewPlayers),
            prepareGame(NewPlayers);
            
        {add, {_PName, _PPid}} ->
            io:format("Cannot add any more players! start the game.~n"),
            prepareGame(Players);
            
        start when length(Players) > 1 ->
            M = "Game is starting!",
            io:format(M), noticeAll(M, Players),
            
            giveCards(mixDeck(deck()), Players),
            D = spawn_link(bluffserver, dealer, [Players, [], []]),
            register(dealer, D),
            letPlay(Players, []),
            
            dealer(Players, [], []),
            prepareGame(Players);
            
        start ->
            io:format("Number of players has to be between 2 and 10. Currently ~p~n", [length(Players)]),
            prepareGame(Players);
            
        %others           
        reset -> reset()
            
    end.
    
%Desk is the list of card currently on the Desk
%PWinner is a list that containes the possible winner
dealer(Players, Desk, PWinner) ->
    receive
         
        {put, Cards, {PName, PPid}} when length(PWinner) =:= 0 ->
            CP = lists:nth(1, Players),
            if
                {PName, PPid} =:= CP ->  
                   io:format("~p played from ~p~n", [Cards, PName]),
                   noticeAll(io_lib:format("~p played!", [PName]), Players), %TODO
                   L = Players -- [CP],
                   NewPlayers = L ++ [CP],
                   NewDesk = [Cards|Desk],
                   letPlay(NewPlayers, NewDesk),
                   
                   dealer(NewPlayers, NewDesk, PWinner);
                true -> 
                   io:format("ERROR: not turn of ~p, cards ~p~n", [PName, Cards]),
                   dealer(Players, Desk, PWinner)  
            end;  
            
        {put, _Cards, {_PName, _PPid}} ->
            %we have a winner!
            io:format("~nVicory!!~n"),
            handleWin(lists:nth(1, PWinner), Players);
            
        {bluff, {_PName, PPid}} when length(PWinner) =:= 0 ->
            {_LPName, LPPid} = lists:nth(length(Players), Players),
            Flag = playerBluffed(lists:nth(1, Desk), Desk),
            
            if
                Flag =:= true -> giveDesk(Desk, LPPid), letPlay(Players, []), dealer(Players, [], PWinner);
                true -> giveDesk(Desk, PPid), letPlay(Players, []), dealer(Players, [], PWinner)
            end;
        
        %PWinner must be the last player in this case
        {bluff, {_PName, _PPid}} -> 
            {LPName, LPPid} = lists:nth(1, PWinner),
            Flag = playerBluffed(lists:nth(1, Desk), Desk),
            
            if
                Flag =:= true -> giveDesk(Desk, LPPid), letPlay(Players, []), dealer(Players, [], []);
                true -> handleWin({LPName, LPPid}, Players)
            end;   
            
        {claimsVictory, {PName, PPid}} ->
            noticeAll(io_lib:format("~p claims victory!", [PName]), Players),
            dealer(Players, Desk, [{PName, PPid} | PWinner]);
        
        %process monitoring   
        {'EXIT',From,Reason} -> 
            io:format("Process ~p died due to ~p~n",[From,Reason]),
            lists:delete(From, Players),
            noticeAll("A player exited the game. Registration Queue is open.", Players),
            reset();
            
        %others
        desk -> %just for tests
            io:format("Desk: ~p~n", [Desk]),
            prepareGame(Players)
        
    end.    
    

%private helper functions  
%create ordered deck
deck() ->
    Cards = ranks(),
    Suites = [s, h, d, c],
    [{C, S} || C <- Cards, S <- Suites].
    
ranks() -> [a,1,2,3,4,5,6,7,8,9,10,j,q,k].     

%daeler stuff
mixDeck(D) -> mixDeckH(D, []).

mixDeckH([], Acc) -> Acc;

mixDeckH(Deck, Acc) -> 
    Index = rand:uniform(length(Deck)),
    Card = lists:nth(Index, Deck),
    mixDeckH(lists:delete(Card, Deck), [Card|Acc]).
    

giveCards([], _Players) -> true;

giveCards([Card|Cards], [{PName, PPid}|T]) ->
    PPid ! {get, Card},
    giveCards(lists:delete(Card, Cards), T ++ [{PName, PPid}]).
    
    
giveDesk([], _PPid) -> true;

giveDesk([H|T], PPid) ->
    lists:foreach(fun(E) -> PPid ! {get, E} end, H),
    giveDesk(T, PPid).
    
letPlay(Players, Desk) -> 
    {PName, PPid} = lists:nth(1, Players),
    noticeAll(io_lib:format("~p is playing..", [PName]), Players),
    PPid ! {canPlay, nextRank(Desk)}.
            

%helpers    
playerBluffed(LastCards, _Desk) when length(LastCards) > 4 -> true;
playerBluffed(_LastCards, Desk) when length(Desk) =:= 0 -> false;

playerBluffed(LastCards, Desk) ->
    io:format("BLUFF REQUIRED! LAST CARDS ~p~n", [LastCards]),
    CurrentRank = currentRank(Desk),
    io:format("RANK ~p~n", [CurrentRank]),
    
    C2 = lists:filter(fun(E) -> {R, _S} = E, R =:= CurrentRank end, LastCards),
    io:format("LAST CARDS FILTERED~p~n", [C2]),
    (length(C2) /= length(LastCards)).
    
    
currentRank(Desk) ->
    CurrentRankIndex = length(Desk) rem length(ranks()),
    lists:nth(CurrentRankIndex, ranks()).
    
    
nextRank(Desk) ->
    CurrentRankIndex = length(Desk) rem length(ranks()),
    lists:nth(CurrentRankIndex + 1, ranks()). 
    

noticeAll(Message, Players) ->
    lists:foreach(fun({_PName, PPid}) -> PPid ! {notice, Message} end, Players). 
    
handleWin({PName, _PPid}, Players) ->
    noticeAll(io_lib:format("Game finished, ~p won!:) Registration Queue is open.~n", [PName]), Players), reset().
    
    
unregister_safe(Atom) ->
    Exists = lists:any(fun(E) -> E =:= Atom end, registered()),
    if
        Exists =:= true -> unregister(Atom);
        true -> void    
    end.
    
    
reset() ->
    unregister_safe(dealer),
    unregister_safe(bluff_reg),
    register(bluff_reg, spawn(bluffserver, prepareGame, [[]])).