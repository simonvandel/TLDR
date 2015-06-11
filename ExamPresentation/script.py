import urllib.request
import random

baseurl = "http://localhost:5555/"
tracks = ["spotify:track:1O8d2pWtBclzuY3FctBPVa" # kings of leon : closer
         , "spotify:track:32OlwWuMpZ6b0aN2RZOeMS" # uptown funk mark ronson
         , "spotify:track:34gCuhDGsG4bRPIf9bb02f" # thinking out loud : ed sheeran
         , "spotify:track:5eWgDlp3k6Tb5RD8690s6I" # sugar : maroon 5
         , "spotify:track:7pqgMEKsDMOHUdFQ7n0N9K" # dangerous david guetta
 ]

userId = "simon"

def vote(trackId, userId):
  url = baseurl + "vote/" + trackId + "/" + userId 
  urllib.request.urlopen(url)

for trackId in tracks:
  for j in range(1, random.randrange(1,10)):
    vote(trackId, userId)
    userId += "1"