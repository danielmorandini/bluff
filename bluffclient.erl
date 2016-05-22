- module(bluffclient).

- export([player/2]).
- export([player/5]).


player(PName, Node) ->
    
    Pid = spawn(bluffclient, player, [Node, {PName, void}, [], false, 0]),
    {bluff_reg, Node} ! {add, {PName, Pid}}, 
    Pid.
    

player(Node, {PName, PPid}, Hand, CanPlay, RequiredRank) ->
    
    receive 
        {get, Card} ->
            io:format("Player ~p received ~p!~n", [PName, Card]),
            player(Node, {PName, PPid}, [Card|Hand], CanPlay, RequiredRank);
         
        {canPlay, RequiredR} -> 
            canPlayHandler(PName, Hand, RequiredR), 
            player(Node, {PName, PPid}, Hand, true, RequiredR);
        
        {play, CardIndexes} when CanPlay =:= true ->
            Cards = cardsFromIndexes(CardIndexes, Hand),
            Flag = areCardsLegal(Cards, Hand),
            if
                Flag =:= true -> player(Node, {PName, PPid}, sendCards({PName, PPid}, Node, Cards, Hand), false, RequiredRank);
                true ->
                    io:format("You don't have some of these cards.. ~n"),
                    player(Node, {PName, PPid}, Hand, true, RequiredRank)
            end;
        
        {play, _Card} ->
            io:format("~p this is not your turn.~n", [PName]),
            player(Node, {PName, PPid}, Hand, CanPlay, RequiredRank);
            
        %if you have / want to play the right cards you can put them fast
        play ->
            Cards = cardsForRank(RequiredRank, Hand),
            if
                length(Cards) > 0 -> player(Node, {PName, PPid}, sendCards({PName, PPid}, Node, Cards, Hand), false, RequiredRank);
                true ->
                    io:format("Cannot use fast play, use {play, [...]}~n"),
                    player(Node, {PName, PPid}, Hand, true, RequiredRank)
            end;
            
            
        bluff ->
            {dealer, Node} ! {bluff, {PName, PPid}},
            io:format("REQUIRED RANK: ~p~n", [a]),
            player(Node, {PName, PPid}, Hand, CanPlay, RequiredRank);
            
        %others
        kill -> io:format("~p is terminating.~n", [PName]);
        
        show ->
            io:format("~p hand is: ~p~n", [PName, Hand]),    
            player(Node, {PName, PPid}, Hand, CanPlay, RequiredRank);
            
        {notice, Message} ->
            io:format("~n~p~n", [lists:flatten(Message)]),
            player(Node, {PName, self()}, Hand, CanPlay, RequiredRank);
            
        reset -> io:format("~p ok~n", [{PName, PPid}]), player(Node, {PName, PPid}, [], false, RequiredRank)             
                    
    end.
    
%message handling

cardsFromIndexes(CardIndexes, Hand) ->
    lists:foldl(fun(I, Acc) -> [lists:nth(I, Hand) | Acc] end, [], CardIndexes).

sendCards({PName, PPid}, Node, Cards, Hand) ->
    io:format("You play ~p~n", [Cards]),
    {dealer, Node} ! {put, Cards, {PName, PPid}},
    
    HandUpdated = Hand -- Cards, %cannot be less than 0, thanks to "areCardsLegal" function
    
    if
        length(HandUpdated) =:= 0 -> claimVictory({PName, PPid}, Node), HandUpdated; 
        true -> HandUpdated           
    end.
    
    
claimVictory({PName, PPid}, Node) ->
    io:format("You claim the victory!~n"),
    {dealer, Node} ! {claimsVictory, {PName, PPid}}.
    
    
%helper functions
canPlayHandler(PName, Hand, RR) ->
    io:format("~nIt's your turn ~p! ~n REQUIRED RANK: ~p~n", [PName, RR]),
    showHand(Hand).
    
areCardsLegal(Cards, Hand) ->
    C2 = lists:filter(fun(E) -> lists:member(E, Hand) end, Cards),
    (length(C2) == length(Cards)).
    
cardsForRank(Rank, Hand) ->
    lists:filter(fun(E) -> {R, _S} = E, R =:= Rank end, Hand).
    
    
showHand(Hand) ->
    lists:foldl(fun(C, Index) -> io:format("~p: ~p~n", [Index, C]),  Index + 1 end, 1, Hand).