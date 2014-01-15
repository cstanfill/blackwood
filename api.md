# Blackwood API v0.1
===============================================================================
## JSON Datatypes:

All arrays, such as "bids", "tricks", etc, are ordered ascending by age.
That is, the most recent card should be FIRST in the trick, as it is most
likely the only one an intelligent application will need.

```
Player: { "id": number, "name" : name } | "Guest"

Bid: { "player": "north"/"south" ... ,
       "bid": { "suit" : "spades"/...
                "level": number} | "Pass",
     ( "modifier": "alert"/"stop",)
     }

Trick: { "originator": "north"/... ,
         "cards": [ Card, ... ] // beginning with the latest
       }

Card: { "number": "two"/.../"king"/"ace",
        "suit": "spades"/...
      }
```

## HTTP API requests:
Any of these can give 401 for users not logged in, or 403 for users that are 
not allowed to do the action. Logging in is not yet specified.

Arrays are again in reversed-time order.

```
GET '/games/[id]' => "NoSuchGame" | { "players": { "north" : Player, 
                                                   "south" : Player, ... }, 
                                    ( "player": "north"/... ,)
                                    ( "bids": [ Bid, Bid, ... ],)
                                    ( "trump": "spades"/..."notrump",)
                                    ( "tricks": [ Trick, Trick, ...],)
                                    ( "trick": [ Card, ...],)
                                    }
    'player' specifies whose turn it is to make a play / bid.

POST '/games' => { "game": id }

POST '/games/[id]/bids' Bid => updated "bids" object | 409 CONFLICT error

PUT '/games/[id]/trick/[n]' Card => new "trick" object | 409 CONFLICT error
    The new trick object MUST contain the specified Card if it is valid.

POST '/games/[id]/trick/[n]' CARD => updated "trick" object | 409 CONFLICT error

## To do
[ ] error bodies
[ ] figure out auth
