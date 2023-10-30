# WordLadder Solver

#### Responds to post requests with a list of the shortest path between two five letter words. 

#### If haskell is not installed visit this link https://www.haskell.org/ghcup/install/

#### To Run the app 
- Clone the repo
-  cd to the directory
- cabal run in the directory 

#### An example of a curl cl post request.
```
curl -X POST -H "Content-Type: application/json" -d '{"start": "finks", "end": "slurs"}' http://localhost:3000/shortest-path
```
