# sunrise

## Build
`$ rebar3 compile`

## Release
`$ rebar3 release`

## Run
`$ rebar3 shell`

Listens for telnet connections on port 5000.

## Features

### Implemented

#### Phase 0 - Raw
- raw mode telnet server
- a message can be broadcast to all users
-- `net_sup:send_all({message, self(), Msg}).`
- a message can be sent to a specified user
-- `Pid ! {send_from_server, Msg}.`
- a user can list connected users
- users can register an account
    - registered accounts are persisted through server restart
- users can login with username and password
- users can create a character after logging in
- users can select one of their characters after logging in

#### Phase 1 - space
- characters enter the world in a configurable starting room
- rooms have descriptions
- rooms have configurable and describable exits
- characters can look at their current surroundings
- characters can move between rooms by traversing exits

#### Phase 1.5 - Reliability
- unit tests for existing features (future new work requires unit tests)

### TODO

#### Phase 2 - characters
- characters have stats
- characters have skills then can activate, with success and impact based on their stats
- characters can make basic skill checks
- characters have body systems (instead of numerical health)
- body systems can be healthy, degraded, impaired, or destroyed
- some body systems are core, others are peripheral
- degraded, impaired, or destroyed body systems reduce the success and impact of character skills
- skills can cause body systems to become degraded, impaired, and destroyed


#### Phase 3 - NPCs
- npc's are a type of character controlled by an AI instead of a user
- npc's have all the capabilities of characters but follow behavioural state machines
- some npc's are peaceful
- some npc's are hostile
- npc's can move based on their state machine
- npc's can use skills based on their state machine

#### Phase 4 - items / inventory / shopping
- characters can have 0+ coins to use as money
- characters can have items
- character items are kept in their bags
- characters can throw items in their bags on the floor
- character can pick items up from the floor
- characters can equip and unequip items
- equipped items modify character stats or add additional skills
- some peaceful npc's are shop keepers
- characters can give shop keepers coins in exchange for items

#### Phase 5 - combat
- characters can fight other characters
- hostile npc's will automatically fight the first other character who enters their room
- peaceful npc's will defend themselves if attacked
- characters will die if any of their core systems are destroyed
- some skills will improve the condition of a body system

#### Phase 6 - web supplement
- a dynamically-generated map of the world can be viewed on the web
- users can view their characters' stats, skills, body systems, and items on the web

#### Phase 7 - web play
- Web-accessible game client

#### Phase 8 - better game loop
- Improved advancement system

#### Phase 9 - HIP
- Improved test suite
- configuration management
- Balance, refactor, hardening

#### Phase 10 - polish
- Polish 

#### Phase 11 - Building tool
- Custom tool to build web-ready rooms and maps
